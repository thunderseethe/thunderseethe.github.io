+++
title = "Simplify[0].Base: Back to basics by simplifying our IR"
date = "2025-04-30T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Simplify"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Lowering", "Optimization", "IR", "Inlining", "Simplification"]
description = "Optimizing our base IR via inlining"
+++

{{< accessory title="Making a Language Series" >}}
This post is part of the [making a languages series](/series/making-a-language).
A series that teaches you how to implement a programming language using Rust.

Today's post is preceded by the [base lowering pass](/posts/lowering-base-ir/).
From that post we'll need our `IR`, and it's accompanying data types.
These will be covered in a refresher below.
{{< /accessory >}}

Today we're talking about a magical aspect of compilation: optimization.
Magic both in its ability to peel away abstractions, leaving only efficient machine code, and its inscrutable behavior.
Optimizers are known for being black boxes with a million knobs, each serving as butterfly wings flapping towards the final result.

The ultimate goal of optimization, improving the runtime performance of your code, is a large cause of this mystical nature.
The runtime performance of your code relies on many, _many_, factors.
Optimization wants to account for as many of these factors as it can, but this is a futile endeavor.
Code performance is often contingent on dynamic input to the program, which is simply unavailable during compilation.

Given optimization can't account for every factor, it's left to try and estimate what changes will give the best outcomes from the information it does have.
An assuredly imperfect process.
Which is not to say optimization isn't worthwhile, in fact it's arguably the reason compilers exist in the first place, simply that our goals are going to be less clear than our previous passes.
Prior passes knew the exact destination they needed to arrive at and failing that is an error.
It's much harder to tell when we've arrived at our final destination while optimizing.

Compilers perform many kinds of optimizations.
Our optimizations today will be performed on our intermediate representation (IR) from [lowering](/posts/lowering-base-ir/) and produces a new optimized IR term.
I'll refer to this pass as simplification, to distinguish it from the other optimizations performed by a compiler, such as instruction specific optimizations, memory layout optimizations etc.


{{< accessory title="Quick Refresher" >}}

In lowering, we turned our `Ast` into our `IR`:

```rs
enum IR {
  Var(Var),
  Int(isize),
  Fun(Var, Box<Self>),
  App(Box<Self>, Box<Self>),
  TyFun(Kind, Box<Self>),
  TyApp(Box<Self>, Type),
  Local(Var, Box<Self>, Box<Self>),
}
```

Our IR is explicitly typed and represents generics using type functions and type applications.
It also introduces `Locals` which are not strictly required, but we'll see are very useful in simplification.
Simplification will work purely in terms of the IR (as will the remainder of our downstream passes), so we'll just need to be familiar with IR to understand it.

Alongside our new IR, we introduce a new `Type` (that is lowered from our AST's type):

```rs
enum Type {
  Int,
  Var(TypeVar),
  Fun(Box<Self>, Box<Self>),
  TyFun(Kind, Box<Self>),
}
```

`TyFun` is the only new addition from our AST's type.
It is the type of type functions.
Type functions contain a kind rather than a type variable.
This is because our type variables use [DeBruijn Indices](/posts/debruijn-indices/), so each variable points at its binding type function without the function containing the variable itself.

In place of a variable, type functions do track the kind of the variable they bind.
`Kind` can only be one variant in base:

```rs
enum Kind {
  Type
}
```

Kinds help us ensure that we apply the right kind of type to a type function.
The same way types help us ensure we apply the right type of value to a function.

That's everything we need to know about our IR, back to simplification.

{{</ accessory >}}

## Simplification

Simplification is the pass where we examine our IR and look for optimization opportunities.
For example, if we see a function applied to its argument:

```rs
let x = Var { ... };
IR::app(IR::fun(x, IR::Var(x)), IR::Int(3))
```

we can optimize that into a local:

```rs
let x = Var { ... };
IR::local(x, IR::Int(3), IR::Var(x))
```

This is a performance win, functions require us to allocate closures, whereas locals can be kept on the stack.

Its worth wondering how often does code like this show up in practice?
Virtuosos, such as you and I, would never write such a function immediately applied to an argument.
Rarely do programmers write such code by hand, but it arises commonly from another aspect of simplification: inlining.

## Inlining 

Inlining replaces a variable by its definition.
On its own this can be a small optimization, removing a variable removes a potential memory load.
The real benefit, however, lies in inlinings potential to unlock further optimizations.
Let's look at some Rust code to see this in action:

```rs
impl Option<T> {
  fn is_none<T>(&self) -> bool {
    match self {
      Some(_) => false,
      None => true
    }
  }
}

enum LinkedList<T> {
  Cons(T, Box<LinkedList<T>>),
  Nil
}

impl LinkedList<T> {
  fn head(&self) -> Option<&T> {
    match self {
      Cons(x, _) -> Some(x),
      Nil -> None
    }
  }

  fn is_empty(&self) -> bool {
    self.head()
        .is_none()
  }
}
```

We like reading the definition of `is_empty`. 
It composes two high level operation to describe its functionality.
But at runtime it makes two separate function calls and perform two separate matches.
Inlining our definitions removes the function calls:

```rs
fn is_empty(&self) -> bool {
  match (match self {
    Cons(x, _) => Some(x),
    Nil => None
  }) {
    Some(_) => false,
    None => true,
  }
}
```

Hard to say this is better.
We've removed the function calls, but we're still doing two matches and it's unreadable.
Our compiler, however, can now see an optimization opportunity previously hidden behind our function calls.
We construct an `Option<T>` only to immediately scrutinize it.
The compiler can elide this now that it can see both matches:

```rs
fn is_empty(&self) -> bool {
  match self {
   Cons(x, _) => false,
   Nil => true,
  }
}
```

Far better than our first or intermediary step.
Inlining is the pan mining these golden optimizations out of our code.
While we programmers may not directly apply an argument to a function, inlining reveals such cases regularly.
Especially for our functional programming language, where composing many small functions is the norm.

We have a better idea of why we simplify.
Now it's time to talk about how we simplify.
It's important to understand that simplification is fundamentally a heuristic based endeavor.
Unlike our passes in the frontend of the compiler (typechecking etc.), we don't _need_ to do anything to simplify.
A perfectly valid implementation would be:

```rs
fn simplify(ir: IR) -> IR {
  ir
}
```

Of course while valid, this makes me sad inside.
It also highlights some problems.
How do we know when we're done simplifying?

Naively, you can run simplification until it stops changing your term, until it reaches a fixed point.
For our simple language, this will actually suffice.
But upon adding recursion, it becomes possible, even easy, to construct terms that never reach a stable state.

With the dream of one day supporting recursion, we'll architect our simplifier to terminate without necessarily reaching a fixed point.
This unfortunately means there will be some terms we don't optimize as much as we could.
We can't distinguish between a term that's making progress and one that's endlessly spinning its wheels, so we have to draw a line in the sand somewhere.

Our goal puts our simplifier in tension:

  * On one hand, it would like to do as much work as possible in a single pass over our IR.
  * On the other hand, it does not want to do so much work that it never terminates.

A valiant goal shared by most computer programs.
It also wants to improve the performance of our IR.
This will involve some inlining, but we can't simply inline everything.

The downside of inlining is found in the upside of variables.
Variables represent sharing.
Rather than duplicate the same work in multiple places, we save it in a variable to be reused.
It must be the case, then, that inlining is reducing that sharing.
Imprudent inlining will cause a duplication of work that outweighs any optimizations it reveals.

We'll need heuristics to help us determine when inlining is worthwhile.
Unfortunately, as is the nature of heuristics, different choices in inlining will work better or worse on different programs.
We won't be able to craft one algorithm that perfectly handles all input.
We'll have to make tradeoffs.

## Our Algorithm

Due to the inherent tradeoffs in simplification, our algorithm will need to be tunable, so a user can tailor optimizations to fit the code they're compiling as best as possible.
Tuning a simplifier to perform well on the widest set of programs is an ongoing issue for compilers.
We'll introduce a few knobs to tune our simplifier, but if you look at a real compiler they often have dozens if not hundreds of knobs dedicated to tuning optimizations.

Alongside tuning, care has to be taken in constructing our algorithm such that it does not loop infinitely.
We break simplification into 3 components to accomplish that:

  * Occurrence analysis
  * Simplify
  * Rebuild

Our first step, occurrence analysis, walks our IR and determines how often each variable occurs.
Simplify takes this occurrence information and uses it to traverse down into our term, making inlining decisions along the way.
Once it reaches a leaf of our IR term it calls rebuild to begin building back up our simplified term.

Rebuild reassembles our simplified term bottom up, looking for opportunities to optimize.
Rebuild, not simplify, is where we'll look for functions applied to arguments and convert them into locals.
It's up to rebuild to call simplify on the other subtrees of our IR.
Simplify only drills down along a single path of our IR tree before calling rebuild.

We'll maintain a loose invariant in rebuild to help us ensure termination: rebuild will only call simplify once on each subtree.
Once we've simplified the function of an `App` node, we won't call simplify on it again (in a given pass).
Eventually we'll have simplified and rebuilt all of our IR at which point we're done.
The diagram below gives a high level overview of how our simplifier will be structured:

![Diagram explaining call hierarchy of our three components](/img/simplify_base_diagram.svg)

I'd love to tell you this architecture came to me in a fever dream as I was relentlessly hacking away on simplification.
Alas, my temperature is normal.
This architecture actually comes from two titans of functional programming: Haskell and OCaml.
Two resources were used when putting the simplifier together:

  * [Secrets of the GHC Inliner](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/8DD9A82FF4189A0093B7672193246E22/S0956796802004331a.pdf/secrets-of-the-glasgow-haskell-compiler-inliner.pdf)
  * [Flambda2](https://ocamlpro.com/blog/2024_03_19_the_flambda2_snippets_1/)

They're both great approachable resources I highly recommend.
We borrow ideas heavily from them, with some caveats for the specifics of our language.
Our base language is:

  * Strict (as opposed to lazy)
  * Side effect free
  * Recursion free

This combination of features is somewhat optimal for simplification (although less than ideal for a usable language).
Lack of side effects means we're free to reorder expressions and can always delete unreferenced local variables.
Being strict means we don't have to worry about simplification making lazy code eager accidentally.
A sizeable portion of Secrets of the GHC Inliner is dedicated to dealing with laziness, so not having to worry about it reduces our complexity.

These factors mean the simplifier presented here is well... simpler then what would be found for a full-blown language.
We do, however, employ the same core architecture as our predecessors.
Reaching parity will be a matter of adding features rather than a fundamental change in architecture.
It does mean that we get to see that foundation more directly without all the complexity required by the features we'll eventually add.

## Occurrence Analysis

With that we can start talking about our implementation with our first step Occurrence Analysis.
Our aim is to analyze our term and determine how often variables occur.
We'll use this information to help us inline.
Variables can occur in one of a few ways:

```rs
enum Occurrence {
  /// Our variable never occurs
  Dead,
  /// Our variable appears once.
  Once,
  /// Our variable appears once inside a Fun node.
  OnceInFun,
  /// Our variable appears many times.
  Many
}
```

Dead, Once, and Many are just counts of variable occurrences: 0, 1, 2 or more respectively.
`OnceInFun` notes that a variable occurs once but within a function body.
When a variable is captured inside a function body, we have to worry about work duplication. 
Consider the code:

```rs
let x = fib(100);
|y| x + y
```

If we inline `x`, all of a sudden we'll calculate `fib(100)` once per function invocation rather than a single time.
`OnceInFun` helps us remember to be wary of duplicate work.
We don't have to do this for `Many` (or `Dead`) variables because we already consider work duplication for them regardless of their residence.
It's only a concern for `Once` variables because they are unconditionally inlined when found outside a function.

We keep track of this information in `Occurrences`:

```rs
struct Occurrences {
  vars: HashMap<VarId, Occurrence>,
}
```

Brought to us by the fine folks at `occurrence_analysis`:

```rs
fn occurrence_analysis(ir: &IR) -> (HashSet<VarId>, Occurrences) {
  match ir {
    // ...
  }
}
```

`occurrence_analysis` returns the set of free variables and the occurrences for `ir`.
We won't need the set of free variables after analysis, but it's used while determining occurrences.
Our first case is `Var`:

```rs
IR::Var(var) => {
  let mut free = HashSet::default();
  free.insert(var.id);
  (free, Occurrences::with_var_once(var.id))
}
```

A variable is free and occurs once, easy.
Another easy one is `Int`:

```rs
IR::Int(_) => (HashSet::default(), Occurrences::default()),
```

Integers have no variable occurrences and no free variables.
Things start ramping up with `Fun`:

```rs
IR::Fun(var, ir) => {
  let (mut free, occs) = 
    occurrence_analysis(ir);
  free.remove(&var.id);
  let occs = occs.in_fun(&free);
  (free, occs)
}
```

For a `Fun` we start by analyzing our function body.
We update our free set, functions bind a variable.
Our updated free set is used to mark our occurrences in functions using `in_fun`:

```rs
impl Occurrences {
  fn in_fun(self, free: &HashSet<VarId>) -> Self {
    Self {
      vars: 
        self.vars
          .into_iter()
          .map(|(id, occ)| {
            ( id
            , match occ {
              Occurrence::Once 
                if free.contains(&id) 
              => Occurrence::OnceInFun,
              occ => occ,
            })
          })
          .collect()
    }
  }
}
```

`in_fun` updates occurrences of `Once` into `OnceInFun` where appropriate.
We only mark free variables captured by our `Fun` as `OnceInFun`.
Bound variables are left untouched because they present no chance of work duplication.
Any bound variable is re-evaluated any time our function is applied, so the work is duplicated regardless of inlining the variable.

We pick back up in `occurrence_analysis` with `App`:

```rs
IR::App(fun, arg) => {
  let (mut fun_free, fun_occs) = 
    occurrence_analysis(fun);
  let (arg_free, arg_occs) = 
    occurrence_analysis(arg);
  fun_free.extend(arg_free);
  (fun_free, fun_occs.merge(arg_occs))
}
```

`App` acquires the Occurrences for `fun` and `arg` and merges them together.
`merge` combines two `Occurrences` updating variable occurrences:

```rs
impl Occurrences {
  fn merge(mut self, other: Self) -> Self {
    for (var, occ) in other.vars {
      self
        .vars
        .entry(var)
        .and_modify(|self_occ| {
          self_occ = match (*self_occ, occ) {
            // ...
          };
        })
        .or_insert(occ);
    }
    self
  }
}
```

For each variable in `other.vars`, we attempt to insert it into `self.vars`:
* If it's not present, we can simply insert it as is.
* If it is present, we match on our two occurrences to combine them.

When one occurrence of our match is `Dead`, we take the other occurrence.
```rs
(Occurrence::Dead, occ) | (occ, Occurrence::Dead) => occ,
```
`0 + x` (and `x + 0`) gives us `x`.
Similarly, if one of our occurrences is `Many`, the result is `Many`:
```rs
(Occurrence::Many, _) | (_, Occurrence::Many) => Occurrence::Many,
```

Two `Once` occurrences meeting is where we actually update:

```rs
(Occurrence::Once, Occurrence::Once) => Occurrence::Many,
```

Merge works on occurrences from disjoint IR sub-terms.
If the same variable occurs once in both terms, that's two occurrences, spelled `Many`, for the overall IR term we're constructing.
We can cover the rest of our cases with one branch, marking the variable `Many`.

```rs
(Occurrence::Once, _) | (Occurrence::OnceInFun, _) => Occurrence::Many,
```

Technically this covers the `(Occurrence::Once, Occurrence::Once)` case as well, but I wanted to call that one out specifically.
That's all our cases.
Returning to `occurrence_analysis`, we knock out `TyFun` and `TyApp` in the same stroke:

```rs
IR::TyFun(_, ir) => occurrence_analysis(ir),
IR::TyApp(ir, _) => occurrence_analysis(ir),
```

Type function don't affect occurrences because, unlike functions, they won't show up at runtime.
Finally, `Local`s:

```rs
IR::Local(var, defn, body) => {
  let (mut free, occs) =
    occurrence_analysis(body);
  let (defn_free, defn_occs) = 
    occurrence_analysis(defn);
  free.extend(defn_free);
  free.remove(&var.id);
  (free, defn_occs.merge(occs))
}
```

We merge our definition and body occurrences to produce the occurrences of our `Local`.
With that we've calculated our occurrences.

## Simplify

With our `occurrence_analysis` output, we construct a new struct `Simplifier` that holds our state for simplification.

```rs
struct Simplifier {
  occs: Occurrences,
  subst: Subst,
  saturated_fun_count: usize,
  saturated_ty_fun_count: usize,
  locals_inlined: usize,
  inline_size_threshold: usize,
}
```

`Simplifier` maintains all the state we'll need for simplifying IR terms.
It keeps track of how many simplifications we've performed with:
  * `saturated_fun_count` - number of functions applied to arguments that were reduced.
  * `satured_ty_fun_count` - number of type functions applied to type arguments that were reduced.
    * This might open up new optimization opportunities, so we count it as performing simplification.
  * `locals_inlined` - number of locals inlined.
These are used to determine if our simplifier performed any work this pass.
`inline_size_threshold` is a configurable option to help determine when to inline a term.

`subst` is a substitution conveniently of type `Subst`.
It tracks what variables we want to inline and what we'll inline them with.
When we encounter a variable binding and determine it's worth inlining we'll map our variable to its definition.
Once we no longer want to inline that variable, we remove it from the substitution.
If this sounds like a Hashmap, it's because `Subst` is a Hashmap:

```rs
type Subst = HashMap<VarId, SubstRng>;
```
Where a `SubstRng` is one of two cases:

```rs
enum SubstRng {
  Suspend(IR, Subst),
  Done(IR),
}
```

`Done` is an `IR` term that has already been optimized.
When we inline it, we should not `simplify` it.
In fact, simplifying it could trigger exponential runtimes.
This is for variables that occur more than once (which includes `OnceInFun`).

`Suspend` is an `IR` term that has yet to be optimized.
When we inline it, we also call `simplify` on it to optimize the term.
Only variables that occur `Once` are suspended, so we don't have to worry about simplifying the same term multiple times.

Couldn't we simplify `Once` variables at their definition and just the `Done` variant?
This works, but misses out on an opportunity to optimize using the surrounding context of variable.
For example if we have:

```rs
let x = |y| 1 + y;
x(10)
```

Simplifying our variable definition yields `|y| 1 + y` which performs about the same as the unsimple definition.
Delaying simplification allows us to inline, producing the term `(|y| 1 + y)(10)` which simplifies to `11`.
An alluring result, assuredly.
The question then isn't why don't we only have the `Done`, but why don't we only have the `Suspend` variant?

If we had the time, we would delay all simplification until after inlining.
Delaying simplification for `Many` variables induces exponential runtime.
Suddenly we're calling simplify on the same IR once per occurrence, rather than once per IR term.
We have to settle for only delaying `Once` variable where we can be confident it won't explode our runtime.

Simplifier offers a single method, `simplify`, as its public API:

```rs
impl Simplifier {
  fn simplify(
    &mut self, 
    mut ir: IR, 
    in_scope: InScope, 
    mut ctx: Context
  ) -> IR {
    todo!()
  }
}
```

Before we dive into the implementation, we have to talk about all the new datatypes introduced.
`IR` we're familiar with from lowering.
`InScope` tracks the set of in-scope variables for the `ir` term we're considering:

```rs
type InScope = HashMap<VarId, Definition>;
```

`InScope` looks similar to `Subst`, a hashmap from `VarId` to an enum of two cases (we're going to find out in a second `Definition` is an enum of two cases).
But they fulfill subtly different roles highlighted by `Definition`.
For each in scope variable, `Definition` tells us if it's bound to a known term or an unknown parameter:

```rs
enum Definition {
  Unknown,
  BoundTo(IR, Occurrence),
}
```

A variable will be unknown if it's a function parameter.
A local variable will be bound to a known `IR` term.
`Subst` only contains variables we know we want to inline, whereas `InScope` contains every variable regardless of inlining.

Our last datatype, `Context`, tracks the surrounding context of our current term `ir`.
As we're simplifying we drill down into `ir`.
`Context` tracks the path through the tree we've drilled down, so we can reconstruct our final term when we finish.
Consider an `IR` term:

```rs
IR::Local(
  Var(...), <defn>,
  IR::App(
    IR::TyApp(
      <ir>, 
      <ty>), 
    <arg>)
  )
```

We're going to drill down and eventually reach the `<ir>`.
When we do, our context will be the path we took to get there:

```rs
vec![IR::Local(Var(...), <defn>, _), IR::App(_, <arg>), IR::TyApp(_, <ty>)]
```

`_` represents a hole where we can put an `IR` term later.
Once we're done optimizing our `TyFun` we walk context and rebuild our final IR.

Why do we track `context` as a parameter at all?
We could rely on recursion to track this information for us.
Imagine a recursive `simplify` implementation such as:

```rs
// imagine we're in simplify().
match ir {
  IR::App(fun, arg) => {
    let fun = self.simplify(fun, ...);
    let arg = self.simplify(arg, ...);
    IR::app(fun, arg)
  }
  // ...
}
```

This is a pattern that has served us well many times before.
What we're doing with `context` is more complicated, but it buys us something important.
When we drill into `fun`, `context` remembers that it's surrounded by an `App`.
We'll make use of this information to make better inlining decisions.

Concretely, `Context` is actually a vector:

```rs
type Context = Vec<(ContextEntry, Subst)>;
```
We'll use the vector like a stack, pushing and popping entries from the end.
`ContextEntry` is our `IR` with a hole:

```rs
enum ContextEntry {
  App(IR),
  TyApp(Type),
  TyFun(Kind),
  Local(Var, Occurrence, IR),
}
```
When we see a `ContextEntry::App(arg)`, we can combine it with some `ir` to reconstruct an `IR::App(ir, arg)`.
`Context` stores a `Subst` for each entry that is restored when rebuilding a term.
When `rebuild` calls simplify we want to do so under the correct substitution, restoring `subst` from the one saved in context ensures we're using the right one.
We've finally covered enough to start into the implementation of `simplify`:

```rs
fn simplify(
  &mut self, 
  mut ir: IR, 
  in_scope: InScope, 
  mut ctx: Context
) -> IR {
  loop {
    ir = match ir {
      // ...
    }
  }
}
```

`simplify` is a tree traversal, a fundamentally recursive algorithm.
We've written it as an iterative algorithm here, though.
This is a matter of preference.
Where Rust supports mutation, I find it easier to express our algorithm iteratively with mutation.
If we were to write this in Haskell, the recursive algorithm might be more attractive.

Each iteration will match on `ir` and return its new value from our match.
Our first two cases are straightforward:

```rs
IR::App(fun, arg) => {
  ctx.push(
    ( ContextEntry::App(*arg)
    , self.subst.clone()
    ));
  *fun
}
IR::TyApp(ty_fun, ty_app) => {
  ctx.push(
    ( ContextEntry::TyApp(ty_app)
    , self.subst.clone()
    ));
  *ty_fun
}
```
When we encounter an `App` or a `TyApp`, we don't do anything, yet.
We add them to our context for later and drill down into `fun` or `ty_fun` respectively.
Type functions are handled the same way:

```rs
IR::TyFun(kind, body) => {
  ctx.push(
    ( ContextEntry::TyFun(kind)
    , self.subst.clone()
    ));
  body
}
```
After those, we have our first terminal case `Int`:

```rs
IR::Int(i) =>
  break self.rebuild(IR::Int(i), in_scope, ctx),
```

There's nothing to optimize about an integer and no subterms to traverse.
It's time to start rebuilding.
Rebuilding will take care of calling simplify on the rest of our IR, so this simplify call is done. 
We break out of the loop, returning our rebuilt IR.
Next up is functions:

```rs
IR::Fun(var, body) => {
  let body = 
    self.simplify(*body, in_scope.update(var.id, Definition::Unknown), vec![]);
  break self.rebuild(IR::fun(var, body), in_scope, ctx);
}
```

We may be shocked to find our case doesn't read:

```rs
IR::Fun(var, body) => {
  ctx.push(
    ( ContextEntry::Fun(var)
    , self.subst.clone()
    ));
  *body
}
```

Which seems simpler.
Our code accomplishes the same, effectively, but with an extra recursive call and break.
We need this extra recursive call, so we can update the `in_scope` set with our parameter.
Because it's a function parameter, we give it an `Unknown` definition in `in_scope`.

Up till now we've been drilling and rebuilding, but to what end.
`Local`s are where the real work starts:

```rs
IR::Local(var, defn, body) => 
  self.simplify_local(
    var, 
    *defn, 
    *body, 
    &mut ctx),
```

### Simplify Locals

Ahem, inside `simplify_local` is where the _real_ work starts:

```rs
fn simplify_local(
  &mut self, 
  var: Var, 
  defn: IR, 
  body: IR, 
  ctx: &mut Context
) -> IR {
  match self.occs.lookup_var(&var) {
    // ...
  }
}
```
The fate of our binding is decided by the occurrence info of our defined `var`.
First up is a `Dead` variable:

```rs
Occurrence::Dead => {
  self.locals_inlined += 1;
  body
}
```

These are straightforward.
The variable isn't ever referenced, so we simply throw away the binding by returning `body` directly.
Because our language lacks side effects we're free to drop any dead binding.
In a more effective language we'd have to be more careful.

Our `Once` case is similar to `Dead`:

```rs
Occurrence::Once => {
  self.locals_inlined += 1;
  let subst = self.subst.clone();
  self.subst.insert(var.id, SubstRng::Suspend(defn, subst));
  body
}
```

Except we modify `subst` before returning body.
Our updated `subst` will be applied to body when it's scrutinized in the next loop of simplify, so for now we're done.
Since we know we're gonna inline the sole occurrence of this variable, making the variable dead, we save some work by dropping its binding now.

Variables that occur once are always profitable to inline (or at worst never harmful).
Again, this is true in part because we lack side effects.
We'd have to be more careful about reordering expressions if it could change the order of effects.
Our final case covers the remainder of our occurrences:

```rs
occ => {
  ctx.push(
    ( ContextEntry::Local(var, occ, body)
    , self.subst.clone()
    ));
  defn
}
```

For any occurrence other than `Once` and `Dead`, we're going to keep our let binding after inlining.
Because we know this let binding will continue to exist we need to simplify its definition.
We accomplish this by saving our var and body in `context`, allowing us to reconstruct our let binding later and return `defn` as the next term to simplify.
We also save `occ` in our `context` for convenience.
It saves us a lookup later when we reconstruct our binding.

That's all our cases, and we're done simplifying locals.
Back in `simplify`, we have one final case to look at: variables.

```rs
IR::Var(var) => 
  match self.simplify_var(
    var, 
    in_scope.clone(), 
    &ctx) 
  {
    ControlFlow::Continue(ir) => 
      ir,
    ControlFlow::Break(var) => 
      break self.rebuild(IR::Var(var), in_scope, ctx),
  },
```

Our variable case makes use of a helpful but lesser known standard library type [ControlFlow](https://doc.rust-lang.org/stable/std/ops/enum.ControlFlow.html).
As we can see from its two cases, it's either a `Continue` or a `Break`.
It allows us to encapsulate our variable logic in our helper `simplify_var` while still breaking out of the simplify loop, if need be.

### Simplify variables

`simplify_var` looks up our variable in our substitution to decide how to proceed:

```rs
fn simplify_var(
  &mut self, 
  var: Var, 
  in_scope: InScope, 
  ctx: &Context
) -> ControlFlow<Var, IR> {
  match self.subst.remove(&var.id) {
    Some(SubstRng::Suspend(payload, subst)) => {
      self.subst = subst;
      ControlFlow::Continue(payload)
    }
    Some(SubstRng::Done(payload)) => {
      self.subst = Subst::default();
      ControlFlow::Continue(payload)
    }
    None => self.callsite_inline(var, in_scope, ctx),
  }
}
```

Our first case we saw setup in `simplify_local`.
A `Once` variable is added to the substitution as a suspended IR term.
Here, we resume that suspended IR term and continue by simplifying it with its restored substitution.

Our second case covers variables we want to inline but appear more than once.
We decide to inline a variable that appears multiple times when its definition is trivial enough to be beneficial to inline in multiple places (or inside a function where work might be duplicated).
We'll see how trivial is determined when we rebuild let bindings.
Notice that when we inline a `Done` variable, we empty the substitution.
This ensures that we do not simplify our done definition any further.

If our variable does not appear in our substitution, we move on to `callsite_inline`:

```rs
fn callsite_inline(
  var: Var,
  in_scope: InScope,
  ctx: &Context,
) -> ControlFlow<Var, IR> {
  todo!()
}
```

Our first two cases of `simplify_var` covered situations where we determined a variable was worth inlining from its definition.
Failing that, it still might be worthwhile to inline a variable based on where it occurs in a term.
`callsite_inline` performs this task.

Ultimately, simplification can only glean so much from analyzing the code statically without running it.
We'll see here that we hit that boundary and have to make more arbitrary decisions than we have prior.
`callsite_inline` starts by examining the in scope value for our variable:

```rs
in_scope
  .get(&var.id)
  .map(|bind| match bind {
    Definition::BoundTo(definition, occ)
      if self.should_inline(definition, *occ, &in_scope, ctx) 
    => {
      todo!()
    }
    _ => ControlFlow::Break(var),
  })
  .expect("ICE: Unbound variable encountered in simplification")
```

We only move on if our in scope variable is defined and `should_inline` determines its definition is worth inlining.
If we decide to inline the variable, we return our definition:

```rs
self.subst = Subst::default();
if let Occurrence::OnceInFun = occ {
  self.occs.mark_dead(&var.id);
}
ControlFlow::Continue(definition.clone())
```

We handle a special case here.
If we decide to inline a `OnceInFun` variable, we know that variable only occurred once...in a fun.
Now that we've inlined it, there are no references to that variable left.
We go ahead and mark the variable as dead in that case.

Marking it dead here allows us to throwaway the now dead binding when we rebuild our term, rather than run another round of simplification to clean up the binding.
Updating occurrence info live like this gets quite complicated.
[Secrets of the GHC Inline](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/8DD9A82FF4189A0093B7672193246E22/S0956796802004331a.pdf/secrets-of-the-glasgow-haskell-compiler-inliner.pdf) remarks that they tried a couple varieties and found all of them too complicated and bug ridden to be worth pursuing.
I'm happy to believe them and learn from their experience rather than my own.
In this case, however, the frequency it arises in practice and the simplicity of implementation make it worthwhile.

How do we know when we should inline a variable, though?
We look to `should_inline` for answers:

```rs
fn should_inline(
  &self, 
  ir: &IR, 
  occ: Occurrence, 
  ctx: &Context
) -> bool {
  match occ {
    //...
  }
}
```

`should_inline`, unsurprisingly, makes it decision based on the variable's occurrence.
Our first two cases are alarming:

```rs
Occurrence::Dead | Occurrence::Once =>
  panic!("ICE: should_inline encountered unexpected dead or once occurrence. This should've been handled prior"),
```

We handled `Once` and `Dead` in `simplify_var`.
If we somehow see them in `should_inline`, we have a logic error.
Call somebody to fix the compiler.
`OnceInFun` is where we actually start making decisions:

```rs
Occurrence::OnceInFun => 
  ir.is_value()
    && self.some_benefit(ir, ctx),
```

We'll inline our variable if its definition is a value and there's some benefit to inlining it.
It makes a lot of sense to inline when there's some benefit, but I can't help but feel that's kicking the can down the road.
Before figuring out what that means, let's talk about what it means for an IR to be a value.

An IR is a value when it contains no work to be done.
For our simple language, work is just `App`.
If our IR contains an `App` it's not a value, otherwise it is.
With one caveat, `Fun`s are value regardless of their body.
Because a `Fun` body is not run until the function is applied we count them as values even if there's an `App` in their body.
`is_value` determines this with a recursive match over `IR` looking for applications:

```rs
impl IR {
  fn is_value(&self) -> bool {
    match self {
      IR::Var(_)
      | IR::Int(_)
      | IR::Fun(_, _) => true,
      IR::TyFun(_, ir) => 
        ir.is_value(),
      IR::TyApp(ir, _) => 
        ir.is_value(),
      IR::Local(_, defn, body) => 
        defn.is_value() 
          && body.is_value(),
      IR::App(_, _) => false,
    }
  }
}
```

Now that we know what a value is, we can look into `some_benefit`:

```rs
fn some_benefit(
  &self, 
  ir: &IR, 
  ctx: &Context
) -> bool {
  todo!()
}
```

`some_benefit` is where we make use of context to determine if inlining opens new opportunities for optimization.
For our current language, that means looking for contexts where we apply arguments to functions.
We begin our search by first determining how many parameters our `ir` has:

```rs
let (params, _) = ir.clone().split_funs();
```

`split_funs` is a helper that collects the parameters from chained `Fun` nodes and returns them alongside the body of our functions.
If our `IR` has no top-level `Fun` nodes, for example `IR::App(IR::Fun(...), IR::Int(3))`, we return `ir` itself and an empty vector for params.
With our parameters ready, next we prepare our arguments from `context`:

```rs
let args = ctx
    .iter()
    .rev()
    .map_while(|entry| match entry {
      (ContextEntry::App(arg), _) => Some(Arg::Val(arg)),
      (ContextEntry::TyApp(ty), _) => Some(Arg::Ty(ty)),
      _ => None,
    })
    .collect::<Vec<_>>();
```

We walk `context` in reverse order, because it's a stack, and grab all contiguous arguments.
It's important the arguments are contiguous.
If we encounter a `TyFun` or `Local` entry, any subsequent arguments aren't applied to the variable we're currently examining.

The first benefit we check is if we have enough arguments to saturate our current function.
A simple matter of comparing lengths:

```rs
if args.len() >= params.len() {
  return true;
}
```
This check accomplishes two things in one:
  * Check if we saturate all our parameters with arguments.
  * Check if we apply more arguments after saturating our functions.

Saturating all our parameters means we can drop the functions.
Functions require allocation, we'll learn why later when we emit code, dropping functions reduces allocation, a performance win.
Further, if all our call sites saturate the function, we can inline them all and drop the variable's binding entirely.

Applying more arguments than we have parameters implies that our function returns further functions after doing some work.
In this case inlining wants to reveal those further functions to the extra arguments available.
Imagine a situation such as:

```rs
let x = |y| {
  let a = (y * 10) / 2;
  |b| a * b
};
x(10)(2)
```

Rust syntax doesn't make this the most idiomatic, but we can still see the benefits of inlining `x` because we reveal `|b| a * b` to its argument `2`:

```rs
(|b| 50 * b)(2)
```

If we don't have enough arguments, there still might be value in inlining if one of arguments is interesting enough.

```rs
args
  .iter()
  .take(params.len())
  .any(|arg| match arg {
    Arg::Val(arg) => {
      !arg.is_trivial()
        || match arg {
          IR::Var(var) => 
            matches!(in_scope.get(&var.id), 
              Some(Definition::BoundTo(_, _))),
          _ => false,
        }
    }
    Arg::Ty(_) => false,
  })
```

An argument is considered interesting when it's:

  * A value, not a type
  * Non-trivial or a variable with a bound definition

As always, the rationale is that these are likely to pave the way for future optimization opportunities.
To understand what a non-trivial argument is, we ask what a trivial argument is.
An `IR` is trivial if it is a variable or an integer:

```rs
impl IR {
  fn is_trivial(&self) -> bool {
    matches!(self, IR::Var(_) | IR::Int(_))
  }
}
```

Trivial arguments generally aren't likely to open up more optimization opportunities.
It's hard to get more optimal than an integer literal.
Hence, our interest in non-trivial `IR`.
Similarly, if our variable has a known definition, inlining that variable can open the door for optimizations, so we consider that interesting as well.
That cinches `some_benefit`, back in `simplify_var` we have one final case to cover:

```rs
Occurrence::Many => {
  let small_enough =
    ir.size() <= self.inline_size_threshold;
  ir.is_value()
    && small_enough 
    && self.some_benefit(ir, in_scope, ctx)
},
```

`Many` is where we must be the most careful about inlining.
It mirrors our `OnceInFun` check but with the addition of a `small_enough` check.
Because we may inline our definition an unbounded number of times, we only want to do so if it is small.

The size of IR is a loose count of how many nodes are in the tree.
It's not strictly a node count because it makes accomodations for the runtime and inlining cost of nodes.
For example if our IR term is a naked variable, we give that a size of zero.
Type functions and applications do not impact size because they don't impact runtime.
We won't cover it here, but the full definition of `size()` can be found in the [source code](https://github.com/thunderseethe/making-a-language/tree/main/simplify/base).

Once we have size in hand we determine if it's small enough by comparing it against `inline_size_threshold`.
This is just a number picked out of a hat.
It defaults to 60, because that's what GHC uses and they seem pretty smart.
In a full compiler this would be a config option that is tunable.

That everything for our `Many` check.
Of our checks, only `some_benefit` relies on the context we're in.
The rest of these values are purely determined by `ir` and could be calculated once and cached.
If you look at GHC it actually does do this, and only `some_benefit` is calculated per occurrence.
We've omitted that caching here for simplicity, but I wanted you to know about it.

With the completion of `callsite_inline`, that's all our simplify cases.
We can move on to the second half of our equation `rebuild`.
`simplify` drilled down into our term building up context.
`rebuild` will consume that context to build up our simplified term:

```rs
fn rebuild(&mut self, mut ir: IR, in_scope: InScope, mut ctx: Context) -> IR {
  while let Some((entry, subst)) = ctx.pop() {
    self.sbust = subst;
    match entry {
      // ...
    }
  }
  ir
}
```

As we build up our term, we'll be looking for optimization opportunities.
In fact, all of our optimizations actually occur in `rebuild`. 
Simplify is solely responsible for inlining.
Each context entry restores its saved substitution.
We proceed based on the kind of entry we're examining, starting with `App`:

```rs
ContextEntry::App(arg) => {
  if let IR::Fun(var, body) = ir {
    self.saturated_fun_count += 1;
    return self.simplify(IR::local(var, arg, *body), in_scope, ctx);
  } else {
    let arg = self.simplify(arg, in_scope.clone(), vec![]);
    ir = IR::app(ir, arg);
  }
}
```

While rebuilding an `App`, we check if our current `ir` is a `Fun`.
When it is, we optimize and replace that with a local binding.
This transformation, however, might open up new opportunities for simplification, so we call simplify on our new `local` and break out of our loop.

We don't substitute our parameter into our body for the same reason we don't inline every variable in our program.
Consideration has to be given to how often and where the variable occurs before deciding to inline.
We already wrote the logic to do all that considering for locals, so we reuse that here.
If our argument is only used once, simplification will recognize that and remove our local.

If `ir` isn't a `Fun`, we construct an `App` and continue.
We didn't simplify our argument when we pushed it into our context, so we simplify our argument now.
It's important we do this with an empty context.
`simplify` will eventually call `rebuild` on our argument, and we want it to only rebuild context that's part of our argument, not our argument plus `ir`'s context.
From there we construct an `App` to use as our new `ir`.
Our next case, `TyApp`, is similar to `App`:

```rs
ContextEntry::TyApp(ty) => {
  ir = if let IR::TyFun(_, body) = ir {
    self.saturated_ty_fun_count += 1;
    subst_ty(*body, ty)
  } else {
    IR::ty_app(ir, ty)
  }
}
```

Like `App`, if `ir` is a `TyFun`, we're going to simplify it.
Unlike `App`, we are going to immediately substitute our type into our body.
Types don't have a runtime cost, so we don't worry about inlining them all over our term.
Rebuilding a `TyFun` is simple:

```rs
ContextEntry::TyFun(kind) => {
  ir = IR::ty_fun(kind, ir);
}
```

Nothing to optimize, or even simplify here, we create our node and move on.
We've saved the most interesting case for last, `Local`s:

```rs
ContextEntry::Local(var, occ, body) => {
  if ir.is_trivial() {
    self.locals_inlined += 1;
    self.subst.insert(var.id, SubstRng::Done(ir));
    return self.simplify(body, in_scope, ctx);
  } else {
    let body = self.simplify(
      body,
      in_scope.update(var.id, Definition::BoundTo(ir.clone(), occ)),
      vec![],
    );
    ir = if let Occurrence::Dead = self.occs.lookup_var(&var) {
      self.locals_inlined += 1;
      body
    } else {
      IR::local(var, ir, body)
    };
  }
}
```

When we drilled down into a `local`, we drilled into the definition not the body.
We did this so that we can make inlining decisions based on the simplified definition of the local.

If our definition is trivial, recall this means it's a variable or an integer, inlinining is free because its small and does no work.
We add our definition to our substitution and proceed by simplifying our body.
Because we've already simplified the definition, we add it to our substitution as `Done` preventing further simplification when we inline.

When our definition is nontrivial, we assume we're going to emit a binding.
We update our in-scope map with our binding and simplify body.
While simplifying our body, we might inline all occurrences of our variable.
To account for this, we check our variable's occurrence info before committing to creating a binding.
If our variable is dead, we can drop our local.

That's everything in `rebuild`.
With all our components, we can assemble our overarching entrypoint the `simplify` method:

```rs
fn simplify(
  &mut self, 
  mut ir: IR
) -> IR {
  for _ in 0..2 {
    let (_, occs) = occurrence_analysis(&ir);
    let mut simplifier = Simplifier::new(occs);
    ir = simplifier.simplify(ir, InScope::default(), vec![]);
    if simplifier.did_no_work() {
      break;
    }
  }
  ir
}
```

We generate the occurrences for `ir`, use them to construct our `Simplifier`, and then call `simplify` on `ir`.
After producing our simplified `ir` we check if we `did_no_work`:

```rs
fn did_no_work(&self) -> bool {
  self.saturated_fun_count == 0 
    && self.saturated_ty_fun_count == 0 
    && self.locals_inlined == 0
}
```

This is a helper method that checks if we performed any simplifications on our `ir`.
If we didn't, we get to go home early.

We do all this in a loop that runs a hardcoded number of times, 2 at the moment.
Two comes to us more as an art than a science.
In testing the simplifier, I could find terms that still needed work with just one pass.
But I couldn't find any that didn't settle after two passes.

Of course these terms almost certainly exist.
We have to draw the line somewhere to ensure we don't run our simplifier forever, and today we draw that line at two.
In the future, after adding more features, we might discover a different number is more appropriate.
We've come a long way from our first simplification function:

```rs
fn simplify(
  &mut self, 
  ir: IR
) -> IR {
  ir
}
```

It's time to enjoy the fruits of our labor.
Let's take a look at a really convoluted IR term:

```hs
((fun [V0]
  ((fun [V1]
    ((fun [V2]
      ((fun [V3] 
        ((fun [V4] (V4 V0 V1 V2 V3)) 
         (fun [V5, V6, V7, V8] V5))) 
        4))
      3))
    2))
  ((fun [V9] V9) 
   ((fun [V10] V10) 
    ((fun [V11] V11) 
     ((fun [V12] V12) 1)))))
```

Here we have a bunch of deeply nested functions applied to various arguments.
For all of our machinations, however, each of our variables is only used once.
After simplification, we have:

```hs
1
```

All the indirection has been cut through leaving only the heart of our program, the value `1`.
It brings me joy to see our simplifier work, even on such a contrived example.

That completes are basic simplifier.
We learned about how optimizations are a futile endeavor to guess the future of our code, and how we have to accomodate that fact by employing heuristics where static analysis fails us.
Even with this reality, our optimizer does quite well at chewing through code and improving its performance.
We can already see the difference by comparing the input and output `IR`.
This delta will only grow larger as we proceed down the compiler.
Eventually when we reach code emission, our simplified IR will produce smaller more efficient code than our initial lowered IR.
As always the full source code can be found in the [accompanying repo](https://github.com/thunderseethe/making-a-language/tree/main/simplify/base).
