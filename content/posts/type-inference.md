+++
title = "Part 1: Type Driven Design"
date = "2023-06-17T16:56:18Z"
author = "thunderseethe"
tags = ["Programming Languages", "Type Inference"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Type Inference", "Bidirectional Typechecking", "Unification", "Union-Find", "Constraint Solving"]
description = "Designing a language, type first"
+++

I'd like to design a language, more specifically implement a compiler for a programming language I've made up. This is not the first time I've wanted to do this. In fact, I've had the [itch](https://github.com/thunderseethe/waht) [quite a](https://github.com/thunderseethe/panera) [few](https://github.com/thunderseethe/brainfuck_interpreter) [times](https://github.com/thunderseethe/false_interpreter) [before](https://github.com/thunderseethe/tiger). I can't tell you why I keep returning to this venture when I've failed at it so many times. What I can tell you is why I always fail. Every time I begin a sparkly new project with fresh eyes and quickly whip together a lexer.

However, once I start constructing a parser, progress slows to a crawl. I endlessly struggle with bike-shedding my syntax or fret to figure out my operator precedence. Without fail by the time I've managed to produce an Abstract Syntax Tree (AST) I've lost all steam. The language is added to my ever-growing pile of incomplete side projects doomed to forever be an empty repository of listless aspirations. The more attempts I've made the clearer this pattern has become to me.

So let's take a step back, why do I start with the lexer and parser at all? Compiler construction can be thought of as a couple of big steps where the output of each step is fed as input into the next step:

![Infographic for Compiler Pipeline](/img/compiler_pipeline.svg)

Because the compiler executes these steps in order when it runs, it's natural to think about creating them in order. The problem is compounded by the educational material on creating languages. Teaching material focuses a **lot** on the many methods by which you can parse text. It's to the point that the wonderful [Crafting Interpreters](https://craftinginterpreters.com/) felt the need to include [a note](https://craftinginterpreters.com/compiling-expressions.html#design-note) explaining why they are brushing over parsing. This over emphasis on parsing could lead one (certainly led me) to believe that parsing is the best place to start on a new language. However, there's no rule that states things have to be created in this order. In fact, doing things this way is impractical if you're designing a language from scratch. Trying to create syntax for your language without first knowing its semantics is a trap. It's like trying to design a vehicle's interior without knowing how many wheels it will have or how its engine will power them. You might design the interior to comfortably seat 4, but if you discover down the line you're designing a motorcycle you're in trouble!

So how do I overcome my parsing problem? I'll simply not write a lexer or parser. I can't get stuck writing a parser if I don't start writing one. I can come back later and add a parser once I've designed more of the internals of my language. My hope is having the other steps (Type Checking, Code Generation, etc.) will provide a guiding hand for what my parser should look like when I do eventually write one. The goal of writing a parser is to produce an AST. But we don't have to parse our AST out of text during prototyping, we can design an AST and then construct it ad hoc where we need it. Then the question is, if we're not starting with lexing/parsing then what step do we start with? The answer is type inference (surprise), but it's relevant to answer why we're starting with type inference. 

To explain why we're starting with our types, look no further than the explanation in [Practical Foundations for Programming Languages](http://www.cs.cmu.edu/~rwh/pfpl.html):
> Types are the central organizing principle of the theory of programming languages. Language features are the manifestations of type structure. The syntax of a language is governed by the constructs that define its types, and its semantics is determined by the interactions among those constructs.

Types inform every other part of our language design, so it's important we nail them down first. Conversely, once we've designed our types the syntax and semantics of our language should easily fall out of our type system.

## Background
This article will not be a great introduction to programming language design in general. There are already a lot of great resources for that:

  * [Crafting Interpreters](https://craftinginterpreters.com/)
  * [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/)

We're focused on implementing type inference (For brevity, we won't even cover definitions of types, polymorphism, or ASTs). I've cobbled together an idea of how to do type inference from reading a lot of sources and looking at some production compilers. I couldn't find a good resource that tied all the concepts together, so I decided to write one. 

We'll build our language in Rust. One might wonder why we'd pick a low level language like Rust, and why not a higher level language that is less verbose such as Haskell or OCaml. These languages are often featured in literature on compilers, and for good reason. They make expressing the tree traversals involved in compilation very natural. However, I don't know OCaml. I'm more familiar with Haskell, but our type inference will make heavy use of mutation making the immutable purity of Haskell a poor fit. Rust has enough features to be functional for our tree traversals while still allowing for mutation.

## Abstract Syntax Tree (AST)
The whole point of parsing is to convert a text file into an AST. Since we're skipping over parsing, we'll start with an AST already constructed. Our language is new, so we'll start small and add more fancy AST nodes incrementally. Initially, our AST will have just 4 nodes: variables, integer literals, function literals, and function application.
```rs
enum Expr<V> {
  /// A local variable
  Var(V),
  /// An integer literal
  Int(isize),
  /// A function literal 
  /// (lambda, closure, etc.)
  Fun(V, Box<Self>),
  /// Function application
  App(Box<Self>, Box<Self>), 
}
```
It doesn't get much simpler than this. This isn't really enough for a usable language, although it is Turing complete. For our purposes, it's plenty to get a minimal type inference system setup. We parameterize our AST by a type parameter `V`. `V` will be the variable of our AST used in `Var` and `Fun`. Initially our AST variable type parameter will be:
```rs
struct Var(usize);
```
For simplicity, we'll represent variables as a `usize`. We can imagine these might be interned strings in a more sophisticated compiler. Once we type check our AST, each variable will be annotated with its type:
```rs
struct TypedVar(Var, Type);
```
Rust requires a layer of indirection for recursive types. Boxing values will make our code samples pretty noisy, let's define some helper methods to hide that noise:
```rs
impl<V> Expr<V> {
  fn fun(
    arg: V, 
    body: Self
  ) -> Self {
    Self::Fun(
      arg, 
      Box::new(body))
  }

  fn app(
    fun: Self, 
    arg: Self
  ) -> Self {
    Self::App(
      Box::new(fun), 
      Box::new(arg))
  }
}
```

### Type
Before we can begin inferring types, we have to know what types can be inferred. Our `Type` type (ha) is determined by our `AST`. We'll need a type to cover each possible value our `AST` could produce. Since our `AST` only has 4 cases, we can check each one and determine what values they can produce:

 * `Var` - it produces any value `AST` produces (but is not a value itself)
 * `Int` - Integer literals are values and will need a type, the type of integers
 * `Fun` - Function literals are also values and will need a type, the type of functions
 * `App` - Function application is not a value, it produces a value when applied to an argument.

So we have two values we can produce from our `AST`, `Int`, and `Fun`. We'll need two types for these values. We'll also need a type for type variables; these won't be for any values but allow us to support polymorphism. Knowing all this we can lay out `Type`:
```rs
struct TypeVar(u32);
enum Type {
  // A type variable
  Var(TypeVar),
  // Type of integers
  Int,
  // Type of functions
  Fun(Box<Self>, Box<Self>),
}
```
Similar to `Var`, our `TypeVar` is just a number under the hood. Our `Type` is a recursive tree, same as `Expr`, so we have to box our nodes. We'll introduce similar helper methods to alleviate the boxing:
```rs
impl Type {
  fn fun(
    arg: Self, 
    ret: Self
  ) -> Self {
    Self::Fun(
      Box::new(arg), 
      Box::new(ret))
  }
}
```

## Type Inference Algorithm
At a high level the goal of type inference is to use contextual information from our AST to infer the type of each AST node. In trivial cases this is very easy when we see an `Int` AST node we know it always has type `Int`. It's not always so straightforward though. If we want to know the type of an `App` node, we have to look at the return type of the function of that `App` node. However, we may not know the type of that function yet. The function might be a variable that refers to an input parameter to our overall AST term. In that case we can't know the type of our function until we know the type of our whole term, which seems circular. The way to break up this circular reasoning is by waiting to infer our type until we have enough information to do so. Instead of inferring a type immediately, when we encounter a type we don't know, we'll track some constraints about that type. Then once we've walked our entire AST we should have enough constraints to infer all our types. 

That sounds a little hand wavy; how can we be sure we'll always generate enough constraints to figure out our types? The answer is **math**! I won't go into details here, but we'll lean on some very nifty proofs a lot of folks worked on to guide us to a type system that is always inferrable. For more information, look into the [Hindley-Milner (HM) type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system).

So we're going to track information about types. We'll do this on our type variables since they represent unknown types. When we encounter a node, we'll give it a fresh type variable and emit a constraint on that variable. Any other nodes that rely on that node will reuse its type variable, producing their own constraints on that variable. This is the basis for implementations of the HM type system such as [Algorithm J](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_J) (or its cousin Algorithm W). There's a key difference between those implementations and ours though. Algorithm J/W attempts to solve constraints as soon as it encounters them and uses a clever data structure to avoid the circular reasoning we saw earlier. Our implementation will instead generate all constraints in one pass and then solve them all in a separate pass.

This split into constraint generation followed by constraint solving offers some nice properties over the "solve as you go" solution. Because we generate a set of constraints from the AST and then solve them, constraint solving only has to know about constraints and nothing about the AST. This means constraint solving requires no modifications when we change our AST. Right now while we have 4 AST nodes the benefit is small, but most languages have 100+ AST nodes. Being able to handle all these cases in constraint generation and not have them bleed over into constraint solving is a huge win for managing complexity. An explicit list of constraints can also make error reporting easier. We can associate a span with each constraint. Then use these spans help to print smarter error messages on a type error. LambdaAle has a great talk [Type Inference as Constraint Solving](https://www.youtube.com/watch?v=-TJGhGa04F8&t=2731s) that goes over the benefits in more detail. The last benefit is more minor but still tractable, having a concrete set of constraints can be invaluable for debugging your type inference pass. Being able to filter and print all the type constraints for a term has saved me some headaches debugging more gnarly typing bugs.

Our two passes will need to share some state between each other. We'll introduce a `TypeInference` struct to hold the shared state and implement our passes as methods on that struct:
```rs
struct TypeInference  {
  unification_table: InPlaceUnificationTable<TypeVar>,
}
```
We'll talk more about `unification_table` when we talk about constraint solving. For now, it's enough to think of it as a mapping from type variables to type, and we'll use it in constraint generation to generate new type variables and keep track of them for later.

## Constraint Generation
We generate our set of constraints from contextual information in our AST. To get this context we need to visit every node of our AST and collect constraints for that node. Traditionally this is done with a bottom-up tree traversal (e.g. HM's Algorithm J). We visit all of a node's children and then use the children's context to better infer the type of our node. This approach is logically correct. We always infer the correct type and type error when we should. While correct, this approach doesn't make the most efficient use of information available. For example, in an application node we know that the function child must have a function type. Since types are only inferred bottom-up, we have to infer an arbitrary type for our function node and add a constraint that the inferred type must be a function.

In recent years, a new approach to type checking called Bidirectional Type Checking has arisen to solve this inefficiency. With Bidirectional Type Checking we have two modes of type checking: infer and check. Infer works the same as the HM type systems we just described, so types are inferred bottom-up. Check works in the opposite direction. We start with a type, and we check that our AST node matches that type. When the AST node and type match, we destructure the type and match each AST child against each type child. Our two modes will call each other mutually recursively to traverse the AST. Now when we want to type check an application node, we have a new option. We can construct a function type at the application node and check the function node against our constructed function type. Making better use of top-down contextual info like this allows us to generate fewer type variables and produce better error messages. Fewer type variables may not immediately appear as a benefit, but it makes the type checker faster. Our constraint solving is in part bound by the number of type variables we have to solve. It also makes debugging far easier. Each type variable acts as a point of indirection so fewer is better for debugging. So while Bidirectional Type Checking doesn't allow us to infer "better" types, it does provide tractable benefits for a modest extension of purely bottom-up type inference.

Now that we know how we'll be traversing our AST to collect constraints, let's talk about what our constraints will actually look like. For our first Minimum Viable Product (MVP) of type inference, `Constraint` will just be type equality:
```rs
enum Constraint {
  TypeEqual(Type, Type)
}
```
We'll talk more about what it means for two types to be equal during constraint solving. For now, it suffices to produce a set of type equalities as a result of our constraint generation.

Constraint generation will be implemented on our `TypeInference` struct with 3 methods:
```rs
impl TypeInference {
  fn fresh_ty_var(&mut self) 
    -> TypeVar { ... }

  fn infer(
    &mut self, 
    env: &mut HashMap<Var, Type>, 
    ast: Expr<Var>
  ) -> (GenOut, Type) { ... }

  fn check(
    &mut self, 
    env: &mut HashMap<Var, Type>, 
    ast: Expr<Var>, ty: Type
  ) -> GenOut { .. }
}
```
`fresh_ty_var` is a helper method we're going to brush past for now as well. (We'll have a lot to cover in constraint solving!) It uses our `unification_table` to produce a unique type variable every time we call it. Past that, we can see some parallels between our `infer` and `check` method that exemplify each mode. `infer` takes an AST node and returns a type, whereas `check` takes both an AST node and a type as parameters. This is because `infer` is working bottom-up and `check` is working top-down.

Let's take a second to look at `env` and `GenOut`. Both `infer` and `check` take an `env` parameter. This is used to track the type of AST variables in their current scope. `infer` and `check` both also return a `GenOut`. This is a pair of our set of constraints and our typed AST:
```rs
struct GenOut {
  constraints: Vec<Constraint>,
  typed_ast: Expr<TypedVar>,
}
```
One final thing to note, we have no way to return an error from `infer` or `check`. We could of course panic, but for the sake of our future selves, we'll return errors with `Result` where relevant. It just so happens it's not relevant for constraint generation. Our output is a set of constraints. It's perfectly valid for us to return a set of constraints that contradict each other. We'll discover the contradiction and produce a type error when we try to solve our constraints. That means there aren't error cases during constraint generation. Neat! 

With that out of the way, we can dive into our implementation of `infer` and `check`. We'll cover `infer` first. Because our AST begins untyped, we'll always call `infer` first in our type inference, so it is a natural starting point. `infer` is just a match on our input `ast`:
```rs
fn infer(
  &mut self, 
  env: &mut HashMap<Var, Type>, 
  ast: Expr<Var>
) -> (GenOut, Type) {
  match ast {
    Expr::Int(i) => todo!(),
    Expr::Var(v) => todo!(),
    Expr::Fun(arg, body) => todo!(),
    Expr::App(fun, arg) => todo!(),
  }
}
```
We'll talk about each case individually, let's start with an easy one to get our feet wet:
```rs
Expr::Int(i) => (
  GenOut::new(
    vec![],
    Expr::Int(i)
  ), 
  Type::Int
),
```
When we see an integer literal, we know immediately that its type is `Int`. We don't need any constraints to be true for this to hold, so we return an empty `Vec`.
```rs
Expr::Var(v) => {
  let ty = &env[&v];
  let typed_var = 
    TypedVar(v, ty.clone());
  (
    GenOut::new(
      vec![], 
      Expr::Var(typed_var)
    ),
    ty.clone(),
  )
},
```
When we encounter a variable, we have to do a lookup of it's type in our `env` and return it. Our env lookup might fail though. What happens if we ask for a variable we don't have an entry for? That means we have an undefined variable, and we'll `panic!`. That's fine for our purposes; we expect to have done some form of name resolution prior to type inference. If we encounter an undefined variable, we should've already exited with an error during name resolution. Past that, our `Var` case looks very similar to our `Int` case. We have no constraints to generate and immediately return the type we look up. Note that in the typed AST we return `TypedVar` with our new `ty` instead of `Var`.
```rs
Expr::Fun(arg, body) => {
  let arg_ty_var = self.fresh_ty_var();
  let env = env.update(arg, Type::Var(arg_ty_var));
  let (body_out, body_ty) = self.infer(env, *body);
  (
    GenOut {
      typed_ast: Expr::fun(
        TypedVar(arg, Type::Var(arg_ty_var)),
        body_out.typed_ast,
      ),
      ..body_out
    },
    Type::fun(Type::Var(arg_ty_var), body_ty),
  )
}
```
`Fun` is where we actually start doing some nontrivial inference. We'll create a fresh type variable and record it as the type of `arg` in our `env`. With our fresh type variable in scope, we'll infer a type for `body`. We then use our inferred body type to construct the function type for our `Fun` node. While `Fun` itself doesn't produce any constraints, it does pass on any constraints that `body` generated.
```rs
Expr::App(fun, arg) => {
  let (arg_out, arg_ty) = self.infer(env.clone(), *arg);

  let ret_ty = Type::Var(self.fresh_ty_var());
  let fun_ty = Type::fun(arg_ty, ret_ty.clone());

  let fun_out = self.check(env, *fun, fun_ty);

  (
    GenOut::new(
      arg_out
        .constraints
        .into_iter()
        .chain(fun_out.constraints.into_iter())
        .collect(),
      Expr::app(fun_out.typed_ast, arg_out.typed_ast),
    ),
    ret_ty,
  )
}
```
`App` is more nuanced than our previous cases. We infer the type of our `arg` and use that to construct a function type with a fresh type variable as our return type. We use this function type to check our `fun` node is a function type as well. Our final type for our `App` node is our fresh return type, and we combine the constraints from `fun` and `arg` to produce our final constraint set.

You may wonder why we've chosen to infer the type for `arg` instead of inferring a type for our `fun` node. This would be reasonable and would produce equally valid results. We've opted not to for a few key reasons. If we infer a type for our `fun` node, it is opaque. We know it has to be a function type, but all we have after inference is a `Type`. To coerce it into a function type we have to emit a constraint against a freshly constructed function type:
```rs
let (fun_out, infer_fun_ty) = self.infer(env.clone(), *fun);
let arg_ty = self.fresh_ty_var();
let ret_ty = self.fresh_ty_var();

let fun_ty = Type::fun(arg_ty.clone(), ret_ty.clone());
let fun_constr = Constraint::TypeEqual(infer_fun_ty, fun_ty);

let arg_out = self.check(env, *arg, arg_ty);
// ...
```
We have to create an extra type variable and an extra constraint compared to inferring a type for `arg` first. Not a huge deal, and in fact in more expressive type systems this tradeoff is worth inferring the function type first as it provides valuable metadata for checking the argument types. Our type system isn't in that category though, so we'll take fewer constraints and fewer type variables every time. Choices like this crop up a lot where it's not clear when we should infer and when we should check our nodes. [Bidirectional Typing](https://arxiv.org/pdf/1908.05839.pdf) has an in-depth discussion of the tradeoffs and how to decide which approach to take.

That covers all of our inference cases, completing our bottom-up traversal. Next let's talk about its sibling `check`. Unlike `infer`, `check` does not cover every AST case explicitly. Because we are checking our AST against a known type, we only match on cases we know will check and rely on a catch-all bucket case to handle everything else. We're still working case by case though, so at a high level our check looks very similar to `infer`:
```rs
fn check(
  &mut self, 
  ast: Expr<Var>, 
  ty: Type
) -> GenOut {
  match (ast, ty) {
    // ...
  }
}
```
Notice we match on both our AST and type at once, so we can select just the cases we care about. Let's look at our cases:
```rs
(Expr::Int(i), Type::Int) => 
  GenOut::new(
    vec![], 
    Expr::Int(i)
  ),
```
An integer literal trivially checks against the integer type. This case might appear superfluous; couldn't we just let it be caught by the bucket case? Of course, we could, but this explicit case allows us to avoid type variables and avoid constraints.
```rs
(Expr::Fun(arg, body), Type::Fun(arg_ty, ret_ty)) => {
  let env = env.update(arg, *arg_ty);
  self.check(env, *body, *ret_ty)
}
```
Our `Fun` case is also straightforward. We decompose our `Type::Fun` into it's argument and return type. Record our `arg` has `arg_ty` in our `env`, and then check that `body` has `ret_ty` in our updated `env`. It almost mirrors our `infer`'s `Fun` case, but instead of bubbling a type up, we're pushing a type down.
```rs
(ast, expected_ty) => {
  let (mut out, actual_ty) = self.infer(ast);
  out.constraints
    .push(Constraint::TypeEqual(expected_ty, actual_ty));
  out
}
```
Finally, we have our catch-all case. At first this might seem a little too easy. If we encounter an unknown pair, we just infer a type for our AST and add a constraint saying that type has to be equal to the type we're checking against. If we think about this, it makes some sense though. In the unlikely case that neither of our types are variables (`(Int, Fun)` or `(Fun, Int)`), we will produce a type error when we try to solve our constraint. In the case that one of our types is a variable, we've now recorded the contextual info necessary about that variable by adding a constraint. We can rely on constraint solving to propagate that info to wherever it's needed. 

This is the only place where we emit a constraint explicitly. Everywhere else we just propagate constraints from our children's recursive calls. The point where we switch from checking back to inference is the only point where we require a constraint to ensure our type line up. Our intuition for `infer` and `check` help guide us to that conclusion. This is in part the insight and the power of a bidirectional type system. It will only become more valuable as we extend our type system to handle more complicated types.

With that we've finished generating our constraints. As output of constraint generation we produce two things: a set of constraints and a typed AST. Our typed AST has a type associated to every variable (and from that we can recover the type of every node). However, a lot of these are still unknown type variables. We'll save that AST for now and revisit it once we've solved our set of constraints and have a solution for all our type variables.

## Constraint Solving
Now that we've got our set of constraints, we can attempt to solve them. So what does it mean to solve a constraint? A constraint is something that has to be true about our types. If we can prove whatever our constraint wants true, we have solved the constraint. Since our `Constraint` datatype is just type equality, we can solve all our constraints by proving their types equal. If we fail to prove two types equal, we have a type error, and we can bail out early.

Okay, that all sounds reasonable. But begs the question, what does it mean for two types to be equal? For us, we'll determine two types are equal structurally. We'll examine the tree of each type and if they are the same node and all their children are equal, we'll consider them equal. This works great except for type variables. It's not enough to just check that two variables are equal. A type variable could stand for any type (even another type variable). When we compare it to another type, we can freely assume the type variable stands for that type and make our two types equal, easy! This works great for any individual equality, but we can get into trouble if we do that haphazardly for our whole set. We may assume for one equality that type variable `a` is `Type::Int` and for another equality assume it is `Type::Fun(Type::Var(b), Type::Var(c))`. While a type variable can stand for any type, it can only stand for one type. If our set of equalities needs a type variable to stand for two types, we have a type error. To remedy this we need to remember what types we've assigned type variables as we solve.

An assignment of type variables to types is called a type substitution. We'll build up a type substitution over the course of constraint solving, and at the end our substitution should have an entry for every type variable. This process of solving a set of constraints by building up a substitution that makes them equal has a name: it's called [Unification](https://en.wikipedia.org/wiki/Unification_(computer_science)).

### Unification
Unification is the process of determining if a type substitution exists that can make our set of constraints true. If no such substitution exists, as always, we have a type error. Unification is fundamentally a search problem since we're trying out various types for our type variables until we find an assignment that works or discover an assignment cannot exist. We won't, but we could imagine if we tried to bruteforce our solution it would take quite awhile. Fortunately, once again, math has our back! Unification is a well studied subject and a couple of algorithms have been found that are able to unify types in near linear time. We'll grab one of these algorithms off the shelf and be on our merry way to quick type inference.

#### Union-find
The secret to speeding up our unification lies in how we store our type substitution. We'll use a [Union-Find](https://en.wikipedia.org/wiki/Disjoint-set_data_structure) data structure to build our type substitution. There's a lot that can be said about the union-find, but we're going to skip most of it. The union-find is only an important implementation detail of our unification algorithm. Union-Find is an efficient way to store disjoint sets of things, for us, that will be sets of types. It does this via two operations: union and find. Each of our disjoint sets is represented by one of its elements. Union merges two sets combining their representatives to form a new representative for the set. Find will look up the set a type is a member of and return that set's representative.

It is not immediately apparent how these two operations will let us implement a type substitution. To see how we'll implement our type substitution with union-find, let's revisit some code we brushed over in constraint generation: `unification_table` and `fresh_ty_var`:
```rs
struct TypeInference {
  unification_table: InPlaceUnificationTable<TypeVar>,
}

fn fresh_ty_var(&mut self) -> TypeVar {
  self.unification_table.new_key(None)
}
```
`unification_table` makes more sense now that we have some context. It's our union-find data structure. Whenever we call `fresh_ty_var`, it's creating a new empty set in our union-find and returning its representative.

When we begin constraint solving, our union-find already has a single set for each type variable with itself as the representative. We can think of this as a type substitution where every variable substitutes itself. As we unify things, we'll call union to merge these sets. Anytime we merge two sets all the type variables in that set now map to its new representative. If we need to substitute a type variable during unification (spoiler we will), we can do that by calling find to look up its current representative.


#### Back to Unification
Now armed with the knowledge of how to maintain our type substitution in near linear time, let's start looking at how we unify types. Our unification implementation begins with a function `unification`:
```rs
fn unification(
  &mut self, 
  constraints: Vec<Constraint>
) -> Result<(), TypeError> {
  for constr in constraints {
    match constr {
      Constraint::TypeEqual(left, right) => 
        self.unify_ty_ty(left, right)?,
    }
  }
  Ok(())
}
```
Not a lot to see yet. We take a set of constraints, and for each constraint we call `unify_ty_ty` to check the types are equal after unification. If we encounter an error in any individual constraint, we bail early and don't try to solve any more constraints.

Cool, so what does `unify_ty_ty` look like?
```rs
fn unify_ty_ty(
  &mut self, 
  unnorm_left: Type, 
  unnorm_right: Type
) -> Result<(), TypeError> {
  let left = self.normalize_ty(unnorm_left);
  let right = self.normalize_ty(unnorm_right);
  match (left, right) {
    // ...
  }
}
```
Oh, it's gonna be a big match statement; neat, we've seen those before. But what's that bit at the top there about `normalize_ty`? We're building up a type substitution as we unify, assigning types to type variables. When we solve a type variable to a type, we don't want to re-solve that type variable everytime we encounter it in our constraints. To avoid re-solving known type variables, whenever we unify a solved type variable, we first want to substitute the variable for its type. This ensures that our type variable is only assigned one type and is equal to all other types our type variable could be assigned. So before we unify a type, we first apply all possible substitutions to the type in `normalize_ty`:
```rs
fn normalize_ty(
  &mut self, 
  ty: Type
) -> Type {
  match ty {
    Type::Int => Type::Int,
    Type::Fun(arg, ret) => {
      let arg = self.normalize_ty(*arg);
      let ret = self.normalize_ty(*ret);
      Type::fun(arg, ret)
    }
    Type::Var(v) => match self.unification_table.probe_value(v) {
      Some(ty) => self.normalize_ty(ty),
      None => Type::Var(v),
    },
  }
}
```
When we normalize, we'll look up the representative for our type variable in our union-find and return a new type with all our instances of the type variable replaced by its representative. Note that when we replace our variable by its type from the union-find, we call `normalize_ty` on that type as well. We might have solved type variables that are present in this type, so we normalize it as well to produce the most known type we can.

Great! Now that we've removed all the type variables we can with normalization, we can break down our unification cases:
```rs
(Type::Int, Type::Int) => Ok(()),
```
As is tradition by now, we'll start with our `Int` case. Since any two `Int` types are trivially equal, we can immediately return.
```rs
(Type::Fun(a_arg, a_ret), Type::Fun(b_arg, b_ret)) => {
  self.unify_ty_ty(*a_arg, *b_arg)?;
  self.unify_ty_ty(*a_ret, *b_ret)
}
```
Two function types are equal if their argument and return types are equal.  We check that by unifying their argument types and the return types.
```rs
(Type::Var(a), Type::Var(b)) => {
  self.unification_table
    .unify_var_var(a, b)
    .map_err(|(l, r)| TypeError::TypeNotEqual(l, r))
}
```
Here's our first case where we add to our type substitution. Two type variables are equal if we can union their sets in our union-find. This might fail if both our variables already have types as representatives. In that case, we have to check both representative types are equal to each other. If they are not, we fail to union and return a type error. If the types are equal, or only one variable has a representative type, we union our sets, and all the type variables in each set now map to the new representative type. It doesn't matter which type becomes the representative, since they are equal, so we just pick one arbitrarily.
```rs
(Type::Var(v), ty) | (ty, Type::Var(v)) => {
  ty.occurs_check(v)
    .map_err(|ty| TypeError::InfiniteType(v, ty))?;
  self
    .unification_table
    .unify_var_value(v, Some(ty))
    .map_err(|(l, r)| TypeError::TypeNotEqual(l, r))
}
```
This case is where we actually assign a type to a type variable. When a type variable encounters a (non-variable) type, we unify the two making our concrete type the representative of that type variable's set. Moving forward whenever we normalize that type variable, or a type variable in it's set, we'll return this type.

Before any of that, we call `occurs_check` on our type possibly returning an `InfiniteType` error. `occurs_check` walks our type and makes sure it doesn't have any instance of the type variable we're about to unify it with. The reason we need to avoid this is it forms a cycle in our type substitution. This is a problem during normalization. When normalization substitutes a type, it first normalizes that type.  But that type contains our variable. So we'll look up our type again and normalize it again, looping infinitely. Infinite loops do not make for fast type inference, so before we can unify our variable and type we want to make sure we're not creating a cycle.
```rs
(left, right) => Err(TypeError::TypeNotEqual(left, right)),
```
Our final case covers all the combinations where our types don't line up (`(Int, Fun(...))`, etc.). This means our set of constraints has a contradiction. All we can do is return a type error and bail early.

With that we've successfully solved our set of constraints producing a type substitution. That tidies up our constraint solving phase. Last we have to tie everything together to form our type inference system.

## Tying the knot
After generating and solving constraints we have 3 things:

   * An inferred type for our whole AST
   * A typed AST where all our variables are annotated with their types
   * A type substitution to map all our type variables to their types

The final step in our inference is to use our type substitution to solve all of our inferred types from constraint generation. We do this by walking our AST and normalizing each type we encounter using our type substitution. The code for this is pretty rote, so we won't cover it in detail, but if you want to see it check out the [full source]().

### Generalization
While we're walking our AST normalizing types, we'll come across a case I haven't touched on yet. What do we do if our type substitution solves a type variable to itself (or another type variable)? Can such a thing even occur? It absolutely can. It turns out it's not even hard to construct such an example:
```rs
let x = Var(0);
Expr::Fun(x, Expr::Var(x))
```
What type should we infer for our AST here? We don't have enough contextual information to infer a type for `x`. After constraint solving, we'll discover `x`'s type is solved to a type variable.

This is actually fine -- intentional even. When we see this, it means the type for our whole AST is polymorphic and should contain a type variable. The way we handle this case is to generalize all our unbound type variables at the end of type inference. A type paired with it's unbound type variables is called a [type scheme](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Polytypes). So the type scheme we'd infer for our example AST is:
```rs
TypeScheme {
  unbound: vec![TypeVar(0)],
  ty: Type::Fun(Type::Var(TypeVar(0)), Type::Var(TypeVar(0)))
}
```
We have one unbound type variable, and our function takes that type variable and returns it. Anyone that wants to call our function will have to provide us a type for our type variable, and only then will we know the type of our function.

## Conclusion
Now that we've put all our pieces in place we can pretty succinctly glue them together to perform type inference in our culminating `type_infer` function:
```rs
fn type_infer(ast: Expr<Var>) -> Result<(Expr<TypedVar>, TypeScheme), TypeError> {
  let mut ctx = TypeInference {
    unification_table: InPlaceUnificationTable::default(),
  };

  // Constraint generation
  let (out, ty) = ctx.infer(im::HashMap::default(), ast);

  // Constraint solving
  ctx.unification(out.constraints)?;

  // Apply our substition to our inferred types
  let (mut unbound, ty) = ctx.substitute(ty);
  let (unbound_ast, typed_ast) = ctx.substitute_ast(out.typed_ast);
  unbound.extend(unbound_ast);

  // Return our typed ast and it's type scheme
  Ok((typed_ast, TypeScheme { unbound, ty }))
}
```
Our function even divides cleanly into our phases. We generate a set of constraints, solve them into a substitution, and apply our substitution.

We've done it! We can infer types for a simple language now. There's a ton we can do to make this more performant or infer more powerful types, but all of that can wait for another day. This is a great MVP that we can start playing around with, and in the future we can start extending it with the fancy features we'd like our language to support. Best of all we didn't get stuck endlessly fiddling with our parser. Type driven design has allowed us to escape the orbital decay of parser generator bike-shedding.
