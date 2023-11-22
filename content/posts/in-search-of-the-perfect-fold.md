+++
title = "In Search of the Perfect Fold"
date = "2023-11-22T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Rust"]
series = []
keywords = ["Programming Languages", "Compiler", "Recursion Schemes", "Fold", "Iterators", "AST", "Trees", "Tree Transformations", "Tree Folds", "Rust", "Mutation", "Design"]
description = "A quest to design the perfect tree fold for Rust"
+++

I'm writing a compiler in Rust.
Which is to say, I do a lot of tree folds in Rust.
Rust shines at a lot of things.
Overall, I'm happy with my choice to use it.
It's just...for the life of me I can't write a satisfactory fold over trees.

Perhaps it's a personal failing.
The perfect fold is out there, and I'm simply ignorant of its divine design.
If you're reading this and thinking that, please I beg of you, enlighten me.
If you're reading this and not thinking that, first off thank you, and second off welcome to my tar pit.

# What's a fold?

Rust aficionados might eagerly point out Rust has a built-in fold: [Iterator::fold](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.fold) (in fact Iterator has a whole bunch of folds).
They're not wrong, and I love the enthusiasm, but that's not quite what we're talking about with tree folds.
There are some issues with using [Iterator::fold](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.fold) as a tree fold:

* Nodes can't control their own traversal
* We need multiple Iterator implementations to have multiple traversal orders
* Using a closure causes lifetime issues

A tree fold is more specialized than [Iterator::fold](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.fold).
It often exists solely to traverse a specific tree (usually an AST in a compiler) and perform an action at each node.
The fold is frequently required to produce a new tree.
In exchange for this more narrow use case, a tree fold can be more brief and convenient to write.
A compiler has a lot of folds.
Rustc has [20+ folds](https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/ty/trait.TypeFolder.html) for just one of their type trees, so this is a worthwhile tradeoff.

[Rust Design Patterns](https://rust-unofficial.github.io/patterns/intro.html) has a great introduction to [tree folds](https://rust-unofficial.github.io/patterns/patterns/creational/fold.html) in Rust.
We're going to reference some of the code snippets, so I've included them below:

{{< accessory title="A Glance at the Code" >}}

Here's what we'll be working off of for our discussion.

### AST
```rs
pub enum Stmt {
  Expr(Box<Expr>),
  Let(Box<Name>, Box<Expr>),
}

pub struct Name {
  value: String,
}

pub enum Expr {
  IntLit(i64),
  Add(Box<Expr>, Box<Expr>),
  Sub(Box<Expr>, Box<Expr>),
}
```

### Folder
```rs
pub trait Folder {
  // A leaf node just returns the node itself. In some cases, we can do this
  // to inner nodes too.
  fn fold_name(&mut self, n: Box<Name>) -> Box<Name> { n }

  // Create a new inner node by folding its children.
  fn fold_stmt(&mut self, s: Box<Stmt>) -> Box<Stmt> {
    match *s {
      Stmt::Expr(e) => Box::new(Stmt::Expr(self.fold_expr(e))),
      Stmt::Let(n, e) => Box::new(Stmt::Let(self.fold_name(n), self.fold_expr(e))),
    }
  }

  fn fold_expr(&mut self, e: Box<Expr>) -> Box<Expr> { 
    match *e {
      Expr::IntLit(i) => Box::new(Expr::IntLit(i)),
      Expr::Add(lhs, rhs) => Box::new(Expr::Add(
        self.fold_expr(lhs),
        self.fold_expr(rhs),
      )),
      Expr::Sub(lhs, rhs) => Box::new(Expr::Sub(
        self.fold_expr(lhs),
        self.fold_expr(rhs),
      )),
    }
  }
}
```

### An Example Fold
```rs
struct Renamer;
impl Folder for Renamer {
  fn fold_name(&mut self, n: Box<Name>) -> Box<Name> {
    Box::new(Name { value: "foo".to_owned() })
  }
  // Use the default methods for the other nodes.
}
```

I highly recommend checking out the [full example](https://rust-unofficial.github.io/patterns/patterns/creational/fold.html).
It does a great job motivating why the fold is done this way.

{{< /accessory >}}

Their `Folder` trait looks quite different from `Iterator::fold`. 
It's a whole trait instead of just a function on a trait.
We can see the utility of this in the example `Renamer` fold.
`Renamer` only has to implement `fold_name`.
It relies on the default trait implementations to perform the rest of the fold.

This separation of our fold into subcomponents allows us to write folds DRY as the desert, a valuable attribute in anything you're going to have more than 20 of.
A reader is able to immediately digest the purpose of `Renamer`.
The use of default methods has alleviated them of sifting through traversal logic trying to ascertain its purpose.

A second advantage to using a trait: traits are implemented on structs.
We can think of the struct as an explicit closure capture for our `fold_*` methods.
Because our fold is a struct (and not a closure) it has a concrete type where we can store named lifetimes.
Complicated folds require this to express the lifetime relationship of their captures.

# What's a good fold?

`Folder` provides a good starting point for our fold.
We saw how it allows us to specify folds more succinctly by omitting traversal logic we don't care about.
Unfortunately, this fold design still has some problems.
Before presenting our gripes, let's slip into an AST that's a little more functional for our purposes:
```rs
#[derive(...)]
struct Var(usize);

#[derive(...)]
enum Expr {
  Int(i64),
  Var(Var),
  Abs(Var, Box<Self>),
  App(Box<Self>, Box<Self>)
}
```
paired with a new more functional `Folder` trait that really makes our eyes pop:
```rs
trait Folder {
  fn fold_var(&mut self, var: Var) -> Var {
    var
  }

  fn fold_expr(&mut self, expr: Box<Expr>) -> Box<Expr> {
    let e = match *expr {
      Expr::Var(v) => Expr::Var(self.fold_var(v)),
      Expr::Abs(var, body) => {
        let var = self.fold_var(var);
        let body = self.fold_expr(body);
        Expr::Abs(var, body)
      },
      Expr::App(func, arg) => {
        let func = self.fold_expr(func);
        let arg = self.fold_expr(arg);
        Expr::App(func, arg)
      }
    };
    Box::new(e)
  }
}
```
Phew, that's better.
All that imperative statement stuff gives me the willies.
To exemplify our problem, let's write a more involved fold that will substitute an expression for a variable:

```rs
// Substitute any occurence of `var` by expression `subst`
struct Subst {
  needle: Var,
  subst: Expr,
}
impl Folder for Subst {
  fn fold_expr(&mut self, expr: Box<Expr>) -> Box<Expr> {
    let e = match *expr {
      Expr::Var(v) if v == self.needle => 
        self.subst.clone(),
      Expr::Var(v) => Expr::Var(self.fold_var(v)),
      Expr::Abs(var, body) => {
        let var = self.fold_var(var);
        let body = self.fold_expr(body);
        Expr::Abs(var, body)
      },
      Expr::App(func, arg) => {
        let func = self.fold_expr(func);
        let arg = self.fold_expr(arg);
        Expr::App(func, arg)
      }
    };
    Box::new(e)
  }
}
```

I don't know about you, but this is much too humid for my tastes.
Performing the actual substitution requires a modest snippet of code:
```rs
Expr::Var(v) if v == self.needle => 
  self.body.clone(),
```
We can hardly see it in the sea of traversing our expression.
Our current fold is great if we need to override one of our leaf methods (`fold_var`, `fold_name`, etc.).
However, it forces us to trudge through an entire expression traversal if we want to override `fold_expr`.
Spoiler alert: we're going to want to override `fold_expr` **a lot**.

{{< accessory title="Mm, I'm not convinced" >}}
Sounding the alarm over repeating 15 lines of code might seem melodramatic.
It's certainly debatable for our example here, but the repetition becomes much less tractable for real ASTs.
[Rust's AST](https://doc.rust-lang.org/stable/nightly-rustc/rustc_ast/ast/enum.ExprKind.html) has 45 variants!
The language would've never released if they were trapped endlessly repeating the traversal of all those variants.
{{< /accessory >}}

Our problem stems from a lack of separation between action and traversal.
`fold_expr` both traverses an expression and acts upon it.
That's an easy fix.
Let's split our fold methods into action and traversal methods:

```rs
trait FolderTwo {
  fn action_var(&mut self, var: Var) -> Var {
    var
  }

  fn action_expr(&mut self, expr: Expr) -> Expr {
    expr
  }

  fn traverse_expr(&mut self, expr: Box<Expr>) -> Box<Expr> {
    let e = match *expr {
      Expr::Var(v) => Expr::Var(self.action_var(v)),
      Expr::Abs(var, body) => {
        let var = self.action_var(var);
        let body = self.traverse_expr(body);
        Expr::Abs(var, body)
      },
      Expr::App(func, arg) => {
        let func = self.traverse_expr(func);
        let arg = self.traverse_expr(arg);
        Expr::App(func, arg)
      }
    };
    Box::new(self.action_expr(e)) // !!
  }
}
```
`action_expr` is called on each expression traversed by `traverse_expr`. 
By default, `action_expr` doesn't do anything, so we can recover our old `fold_expr` behavior by simply using the default implementation.
However, we can also override it, which we use to write a better `Subst` fold:
```rs
impl FolderTwo for Subst {
  fn action_expr(&mut self, expr: Expr) -> Expr {
    match expr {
      Expr::Var(v) if self.needle == v => self.subst.clone(),
      expr => expr,
    }
  }
}
```
Now that's a good-looking fold.
We only write and read the code that's critical to this particular fold.
Everything else is handled by a reasonable default behind the scenes.

We can stop here.
In fact, we should stop here.
But I can't ignore this whisper in the wind.
The rustacean in me can't help but notice that `traverse_expr` always allocates a new expression.
Even when we return our node as is, our fold allocates an entire fresh copy.

You might, reasonably, ask: is the reallocation really such a big deal?

Are you sane?

Can you not hear the tell-tale allocation thumping upon each traversal of our tree?

A beating beneath the floorboards, so cacophonous as to reverberate in my skull until I go deaf.  
ONLY THE SANCTIMONIOUS SOUNDS OF OVERENGINEERING CAN QUIET ITS DEAFENING DIRGE.  

...*ahem*  

So then, we agree, this is an issue most pressing -- for both of us.
How do we go about solving it?

# What's a _perfect_ fold?

My calm demeanor and experience with folds has guided us this far.
But here we leave its charted territory for unknown waters.
I know of a couple solutions to avoid superfluous allocations and none of them are ideal:

 * A `should_traverse_*` suite of methods
 * Track changes explicitly
 * Make a new kind of fold: `InPlaceFolder`

## `should_traverse` methods
The first option is to add a set of `should_traverse` methods to our `Folder` trait.
These new methods determine if we should traverse a particular entity or skip it:

```rs
trait ShouldFolder {
  // our other methods...

  fn should_traverse(&mut self, expr: Expr) -> bool {
    true
  }

  fn traverse_expr(&mut self, expr: Box<Expr>) -> Box<Expr> {
    if !self.should_traverse(expr) {
      return expr;
    }
    // the rest of traverse is the same...
  }
}
```

This is definitely viable (Clang employs this strategy for their [RecursiveASTVisitor](https://clang.llvm.org/doxygen/classclang_1_1RecursiveASTVisitor.html).)
So clearly this strategy can be used in production.
However, it's not great for our purposes.

If we want to apply this strategy to our example `Subst` fold, what would that look like?
Well, to skip an expression we have to determine if it contains any variables that will be substituted.
But to do that, we have to traverse the expression and inspect any variable nodes we find.
We're right back where we started.
We have to traverse our expression to determine if we should traverse our expression.

This strategy has its strengths (otherwise it wouldn't have found itself a home in Clang).
But it doesn't suit our particular purpose of avoiding allocations.
We'll have to move on.

## Track Changes Explicitly
Our next attempt is to track changes explicitly.
The idea is that whoever is writing the fold knows when they make a change.
We modify our API to allow a fold writer to inform us a change has been made.
Instead of our `action_*` methods always returning something, they'll return an `Option`:

```rs
trait Folder {
  fn action_var(&mut self, var: &Var) -> Option<Var> {
    None
  }

  fn action_expr(&mut self, expr: &Expr) -> Option<Expr> {
    None
  }

  // ...
}
```

If no changes were made, `action_*` returns None (the default case).
A fold write overrides one of these to return `Some` when their action changes something.
Then we modify our `traverse_expr` method to account for the change:

```rs
fn opt_traverse_expr(&mut self, expr: &Expr) -> Option<Expr> {
  let e = match &expr {
    Expr::Var(v) => {
      self.action_var(v)
        .map(Expr::Var)
    },
    Expr::Abs(var, body) => {
      let opt_var = self.action_var(var);
      let opt_body = self.opt_traverse_expr(body);
      match (opt_var, opt_body) {
        (Some(var), Some(body)) => Some(Expr::Abs(var, body)),
        (None, Some(body)) => Some(Expr::Abs(var.clone(), body)),
        (Some(var), None)) => Some(Expr::Abs(var, body.clone())),
        (None, None) => None,
      }
    },
    Expr::App(func, arg) => {
      let opt_func = self.opt_traverse_expr(func);
      let opt_arg = self.opt_traverse_expr(arg);
      match (opt_func, opt_arg) {
        (Some(func), Some(arg)) => Some(Expr::App(func, arg)),
        (None, Some(arg)) => Some(Expr::App(func.clone(), arg)),
        (Some(func), None) => Some(Expr::App(func, arg.clone())),
        (None, None) => None
      }
    }
  };
  match e {
    Some(changed_expr) => {
      // Our expr changed so we always return Some
      Some(self.action_expr(&changed_expr)
        .unwrap_or(changed_expr))
    }
    None => {
      self.action_expr(expr)
    }
  }
}

fn traverse_expr(&mut self, expr: Box<Expr>) -> Box<Expr> {
  let opt_expr = self.opt_traverse_expr(&expr);
  match opt_expr {
    Some(changed_expr) => Box::new(changed_expr),
    None => expr,
  }
}
```

`traverse_expr` now reuses our original expression if `expr` is unchanged by our fold.
But at what cost?
This solution required significant changes to our API: everything returns an Option and our parameters are references instead of values now.
We have to do it this way to avoid lifetime issues, but it incurs a lot of complexity.

This added complexity is because we're writing unidiomatic code.
Our goal is to track changes explicitly and only update our value when it's changed.
But Rust already has a tool for that: mutation.
In our frenzy to avoid allocations, we've incidentally assembled a poor man's mutable reference.

## InPlaceFolder
Given that revelation, couldn't we just use mutable references in our fold?
We can, and we will -- which brings us to our final attempt: `InPlaceFolder`.
If Goldilocks is to be believed, this is our salvation.
Instead of modifying our `Folder` trait, we introduce a new trait `InPlaceFolder`:

```rs
trait InPlaceFolder {
  fn action_var(&mut self, var: &mut Var) {}

  fn action_expr(&mut self, expr: &mut Expr) {}

  fn traverse_expr(&mut self, expr: &mut Expr) {
    match expr {
      Expr::Var(v) => 
        self.action_var(v),
      Expr::Abs(var, body) => {
        self.action_var(var);
        self.traverse_expr(body);
      },
      Expr::App(func, arg) => {
        self.traverse_expr(func);
        self.traverse_expr(arg);
      }
    };
    self.action_expr(expr)
  }
}
```

This is already a lot simpler than our Option returning `Folder`.
What does `Subst` look like with `InPlaceFolder`:

```rs
impl InPlaceFolder for Subst {
  fn action_expr(&mut self, expr: &mut Expr) {
    if let Expr::Var(var) = expr {
      if self.needle == v {
        *expr = self.subst.clone()
      }
    }
  }
}
```
We only specify the branch we care about `Expr::Var` and do nothing otherwise.
This solution has the advantage that it never allocates (actually it can't allocate).
It is also our most idiomatic solution so far as you might gleam from its simplicity.
We don't have to add any new methods to our trait, and we make a very minor change to its signature.

Alas, it still isn't perfect. 
(Otherwise, this article would be called "I Found the Perfect Fold".)
It's nice for performance that our fold can't allocate, but sometimes we have to allocate a new tree for correctness.
We need a fold that can do both: allocate a new tree when required and reuse allocations when we don't.
But this fold can only support one.

This means to support all our use cases we'll need two independent folds: `Folder` and `InPlaceFolder`.
On some level this makes sense, the semantics between a fold that creates a new tree and one that consumes a tree are quite different.
Part of the power of Rust is allowing us to reason about these distinctions to better ensure correctness.
On another level, there are some serious practicality issues here.

Do we implement each of our folds twice?
Maybe any given fold can only implement one or the other, so each fold implements the trait that's relevant.
This saves us duplicating fold logic but forces us to consider what kind of fold we have when applying folds to our entities.
We'll have to have both a `fold` and `fold_in_place` method on everything that might be folded.
We've just moved the duplication elsewhere.

Could we make each trait implementation a stub that calls out to a shared helper defined for a particular fold?
The differences between owning a value and holding a mutable reference to a value make this difficult.
The shared helper can only use the lowest common denominator between ownership and mutable references.
Even without that issue, that's still a new function and two boilerplate trait `impl`s required for every fold.

This is where our sad tale ends.
It's a known pain point in Rust that you can't be polymorphic about ownership (that's why the standard library is filled with `get` & `get_mut` methods).
We're forced to choose between wasteful allocations and convenient fold definitions.
Certainly not the end of the world, but I'm wistful for a fold that doesn't make us choose.
