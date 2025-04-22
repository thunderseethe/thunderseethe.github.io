+++
title = "Simplify[0].Base: Back to basics by simplifying our IR"
date = "2025-04-13T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Simplify"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Lowering", "Optimization", "IR"]
description = "Optimizing our base IR via inlining"
+++

* Intro Making a language
  * Put this post in context.
  * Follows lowering/base.
  * IR refresher.

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
Virtuosos, such as you and I, would never write a function immediately applied to an argument.
Rarely do programmers write such code by hand, but it arises commonly from another important step in simplification: inlining.

## Inlining 

Inlining replaces a variable by its definition.
On its own this can be a small optimization.
Removing a variable does remove a potential memory load.
The real benefit, however, lies in inlinings potential to unlock further optimizations.

To see this in action, let's look at some haskell code:

```hs
isNothing :: Maybe a -> Bool
isNothing x = 
  case x of
    Nothing -> True
    Just _ -> False

head' :: [a] -> Maybe a
head' xs =
  case xs of
    [] -> Nothing
    (x:_) -> Just x

isEmpty :: [a] -> Bool
isEmpty xs = isNothing (head' xs)
```

We like reading the definition of `isEmpty`, it composes two high level operation to describe its functionality.
But at runtime it has to make two separate function calls and perform two separate matches.
Inlining our definitions allows simplification to help us out:

```hs
isEmpty xs = 
  case (case xs of
      [] -> Nothing
      (x:_) -> Just x) of
    Nothing -> True
    Just _ -> False
```

After inlining, our code is inarguably harder to read.
Our compiler, however, can now see an optimization opportunity previously hidden behind our function calls.
It will transform our inlined code into:

```hs
isEmpty xs = 
  case xs of
    [] -> True
    (x:_) -> False
```

Far better than our intermediary step.
Inlining is the backbone these kind of optimization opportunities are built atop.
While we programmers may not directly apply an argument to a function, through inlining such cases arises quite often.
Especially in functional programming, where combining many small functions is the norm.


We have a better idea of why we simplify.
Now it's time to talk about how we simplify.
Simplification is, in many ways, the secret sauce of compilation, cutting away high level abstractions to leave performant code in its wake.
It's important to understand that simplification is fundamentally a heuristic based endeavor.
Unlike our passes in the frontend of the compiler (typechecking etc.), we don't _need_ to do anything to simplify.
A perfectly valid implementation would be:

```rs
fn simplify(ir: IR) -> IR {
  ir
}
```

Of course while valid, this is not a very satisfying implementation.
We encounter problems trying to do better:

  * Our simplifier wants to do as much work as possible in a single pass over our IR.
  * Our simplifier does not want to do so much work that it never finishes.
  * Some amount of inlining is beneficial, but only the right amount.

TODO: this is repetitive, reword.
Our simplifier wants to do as much as possible without looping infinitely.
A valiant goal shared by most computer programs.
It also wants to improve the performance of our IR.
This will involve some inlining, but we can't simply inline everything.

For example, inlining a variable that is used multiple times will often duplicate work and hurt performance.
Except when it doesn't, of course.
We'll need heuristics to help us determine when inlining is worthwhile.
Unfortunately, as is the nature of heuristics, different choices in inlining will work better or worse on different programs.
We won't be able to craft one algorithm that perfectly handles all input.

## The Algorithm

This is an ongoing issue for compilers.
We'll introduce a few knobs to tune our simplifier, but if you look at a real compiler they often have dozens if not hundreds of knobs dedicated to tuning optimizations.
These knobs will be employed by our simplifier to do as much as possible in a single pass.
Care has to be taken in constructing this pass such that it does not loop infinitely.
To accomplish that, we break simplification down into 3 components:

  * Occurrence analysis
  * Simplify
  * Rebuild

Our first step is occurrence analysis.
This walks our IR and determines how often each variable occurs.
Simplify takes this occurrence information and uses it to traverse down into our term, making inlining decisions along the way.
Once it reaches a leaf of our IR term it calls rebuild to being building back up our simplified term.

Rebuild reassembles our simplified term bottom up.
While it's doing this it looks for opportunities to optimize.
Rebuild, not simplify, is where we'll look for functions applied to arguments and convert them into locals.
Simplify drills down along a single path of our IR tree before calling rebuild.
It's up to rebuild to call simplify on the other subtrees of our IR.

Eventually we'll have simplified and rebuilt all of our IR and we're done.
The diagram below gives a high level overview of how our simplifier willl be structured:

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
Lack of side effects means we're free to reorder expressions and can always delete dead bindings.
Being strict means we don't have to worry about simplification making lazy code eager accidentally.
This is a sizeable consideration in Secrets of the GHC Inliner, so not having to worry about it reduces our complexity.

These factors mean the simplifier presented here is well... simpler then what would be found for a full-blown language.
This, however, does not mean our architecture is simpler.
The foundation we build here will scale to encompass the features we currently lack.
It does mean that we get to see that foundation more directly without all the complexity required by the features we'll eventually add.

## Occurrence Analysis

With that we can start talking about our implementation with our first step Occurrence Analysis.
Our goal is to analyse our term and determine how often variables occur.
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
When a variable is captured inside a function body we have to worry about work duplication. 
Consider some Rust code:

```rs
let x = fib(100);
|y| x + y
```

If we inline `x`, all of a sudden we'll calculate `fib(100)` once per function invocation rather than a single time.
To avoid this kind of duplicate work we specially note variables that occur once in a function.
We don't have to do this for `Many` (or `Dead`) variables because we already consider work duplication for them.
It's only a consideration for `Once` variables. 

We keep track of this information in `Occurrences`:

```rs
struct Occurrences {
  vars: HashMap<VarId, Occurrence>,
}
```

Produced by `occurrence_analysis`

```rs
fn occurrence_analysis(ir: &IR) -> (HashSet<VarId>, Occurrences) {
  match ir {
    // ...
  }
}
```

`occurrence_analysis` returns the set of free variables and the occurrences for `ir`.
We won't need the set of free variables, but it's helpful while determining occurrences.
Starting with `Var`:

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
              Occurrence::Once if free.contains(&id) => Occurrence::OnceInFun,
              occ => occ,
            })
          })
          .collect()
    }
  }
}
```

`in_fun` checks each occurrence in `self`.
If we see a free variable that occurs `Once`, we change it to `OnceInFun`.
The variable needs to be free.
We only want to mark variables captured by our `Fun`. 
Bound variables are considered `Once`.

Back in `occurrence_analysis`, we're handling `App`:

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
`merge` combines two `Occurrences` updating variable occurrences accordingly:

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

For each variable in `other.vars`, we attempt to insert it into `self.vars`
If it's not present, we can simply insert it as is.
If it is present, we match on our two occurrences to combine them:

If one of our occurrences is `Dead`, we take the other occurrence.
```rs
(Occurrence::Dead, occ) | (occ, Occurrence::Dead) => occ,
```
`0 + x` (or `x + 0`) gives us `x`.
Similarly, if one of our occurrences is `Many`, the result is `Many`:
```rs
(Occurrence::Many, _) | (_, Occurrence::Many) => Occurrence::Many,
```

When two `Once` occurrences meet, we mark that variable `Many`:

```rs
(Occurrence::Once, Occurrence::Once) => Occurrence::Many,
```

We can cover the rest of our cases with one branch, marking the variable `Many`.

```rs
(Occurrence::Once, _) | (Occurrence::OnceInFun, _) => Occurrence::Many,
```

Technically this covers the `(Occurrence::Once, Occurrence::Once)` case as well, but I wanted to call that one out specifically.
That's all our cases.
Back to `occurrence_analysis`, to cover `TyFun` and `TyApp`:

```rs
IR::TyFun(_, ir) => occurrence_analysis(ir),
IR::TyApp(ir, _) => occurrence_analysis(ir),
```

Type function and applications don't affect occurrences because they won't show up at runtime.
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

Our `occurrence_analysis` output is used to construct a new struct `Simplifier` that exposes our main method `simplify`.

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

Before we dive into the implementation, we have to talk about all the new datatypes we're going to need to simplify.
`IR` we're familiar with from lowering.
`InScope` tracks the set of in-scope variables for the `ir` term we're considering:

```rs
type InScope = im::HashMap<VarId, Definition>;
```

For each in scope variable, `Definition` tells us if it's bound to a known term or an unknown parameter:

```rs
enum Definition {
  Unknown,
  BoundTo(IR, Occurrence),
}
```

A variable will be unknown if it's a function parameter.
A local variable will be bound to a known `IR` term.

Our last datatype, `Context`, tracks the surrounding context of our current term `ir`.
As we're simplifying we drill down into `ir`.
`Context` tracks the path we've drilled down, so we can reconstruct our final term when we finish.
Consider an `IR` term:

```rs
IR::Local(
  Var(...), <defn>,
  IR::App(
    IR::TyApp(
      IR::TyFun(...), 
      <ty>), 
    <arg>)
  )
```

We're going to drill down and eventually reach the `TyFun`.
When we do, our context will be the path we took to get there:

```rs
vec![IR::Local(Var(...), <defn>, _), IR::App(_, <arg>), IR::TyApp(_, <ty>)]
```

Where `_` represents a hole where we can put an `IR` term later.
Once we're done optimizing our `TyFun` we walk context and put our final IR back together.
This will be the job of rebuild.

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
What we're doing with `context` is more complicated than that, but it buys us something important.
When we drill into `fun`, `context` remembers that it's surrounded by an `App`.
We'll make use of this information to make better inlining decisions.

Concretely, `Context` is actually a vector:

```rs
type Context = Vec<(ContextEntry, Subst)>;
```
  * `ContextEntry` is our `IR` with a hole:

```rs
enum ContextEntry {
  App(IR),
  TyApp(Type),
  Local(Var, Occurrence, IR),
}
```
When we see a `ContextEntry::App(arg)`, we can combine it with some `ir` to reconstruct an `IR::App(ir, arg)`.
Alongside each entry, we store a `Subst`.
`Subst` is a substitution that stores variables that we've decided to inline:

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
`Suspend` is an `IR` term that has yet to be optimized.
When we inline it, we also call `simplify` on it to optimize using the context of its inlining.
Only variables that occur `Once` are suspended, so we don't have to worry about simplifying the same term multiple times.

`Done` is an `IR` term that has already been optimized.
When we inline it, we should not `simplify` it.
In fact, simplifying it could trigger exponential runtimes.
This is for variables that occur more than once (which includes `OnceInFun`).

`Context` stores a `Subst` for each entry that is restored when rebuilding a term.
We'll also see `Subst` used with `Occurrences` in our `Simplifier` struct:

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

Fundamentally, `simplify` is a recursive algorithm.
We've written it as an iterative algorithm here, though.
This is a matter of preference.
Where Rust supports mutation, I find it easier to express our algorithm iteratively with mutation.
If we were to write this in Haskell, the recursive algorithm would be more attractive.

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
We don't do anything, yet, when we encounter an `App` or a `TyApp`.
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

There's nothing more to optimize about an integer.
We break out of our loop by rebuilding our integer, wrapping it back up in the surrounding context.

Next up is functions:

```rs
IR::Fun(var, body) => {
  let body = 
    self.simplify(*body, in_scope.update(var.id, Definition::Unknown), vec![]);
  break self.rebuild(IR::fun(var, body), in_scope, ctx);
}
```

We might imagine this case would look like:

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
Effectively, our code accomplishes the but with an extra recursive call and an extra break.
We do it this way, so that we can update our `in_scope` set.

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
We look up the occurrence info of our defined `var` to determine what to do with this binding.
First up is a `Dead` variable:

```rs
Occurrence::Dead => {
  self.locals_inlined += 1;
  body
}
```

These are straightforward.
The variable isn't ever referenced, so we simply throw it away by returning `body` as is.
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
Variables that occur once are always profitable to inline (or at least never harmful).
Again, this is true in part because we lack side effects.
We'd have to be more careful about reordering expressions if it could change the order of effects.

We know our variable only occurs once within body.
It's important we delay simplifying the definition of our variable until we inline it, so we store it in our substitution as a `Suspend`.
[Secrets of the GHC Inliner](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/8DD9A82FF4189A0093B7672193246E22/S0956796802004331a.pdf/secrets-of-the-glasgow-haskell-compiler-inliner.pdf) calls this example out specifically due to its unintuitive behavior.
Even if we simplify our definition now, we still want to simplify it where we inline it.
We might discover new optimization opportunities we want to exploit upon inlining.

If we were to simplify now and upon inlining, that would only be two simplifications, what's the harm?
The harm is this can expand exponentially.
Our definition could contain its own local variables, which we would also simplify twice.
Each of those local definitions may contain their own locals, and so on, and so on.

Pretty soon we're performing 2^n simplifications and the programmer is sigkill-ing our compilation process.
To avoid that, we suspend our definition unsimplified and simplify it at it's callsite.
Note that we save our current substitution.
When we do simplify our term, we want to do so under the substitution as it appears at the definition site not at the callsite.
TODO: Figure out why we need to save our substitution here and explain it.

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
Because we know this let binding will continue to exist we go ahead and simplify its definition now.
We accomplish this by saving our var and body in `context` allowing us to reconstruct our let binding later and return `defn` as the next term to simplify.
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

The real action starts in `simplify_var`:

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

`simplify_var` looks up our variable in our substitution to decide how to proceed.
Our first case we saw setup in `simplify_local`.
A `Once` variable is added to the substitution as a suspended IR term.
Here, we resume that suspended IR term and continue by simplifying it with its restored substitution.

Our second case is for variables we've decided to inline but appear more than once.
We decide to inline a variable that appears multiple times when its definition is trivial enough to be beneficial to inline in multiple places (or inside a function where work might be duplicated).
We'll see how we determine that when we rebuild let bindings.
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

Our first two cases of `simplify_var` covered situations where we determined a variable was worth inlining at its definition.
Failing that, it still might be worthwhile to inline a variable based on where it occurs in a term.
`callsite_inline`'s job is to determine when it's beneficial to inline a variable.
At its core there is always a heuristic to decide this.

Ultimately, simplification can only glean so much from analyzing the code statically without running it.
We'll see here that we hit that boundary and have to make more arbitrary decisions than we have prior.
Our function starts by examining the in scope value for our variable:

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

We only move on if our in scope variable is defined and `should_inline` determines that definition is worth inlining.
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

Marking it dead here will allow us to throwaway the dead binding when we rebuild our term.
Otherwise, we'd have to run another round of simplification to determine the binding was now dead and throw it away.
Updating occurrence info live like this gets quite complicated.
[Secrets of the GHC Inline](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/8DD9A82FF4189A0093B7672193246E22/S0956796802004331a.pdf/secrets-of-the-glasgow-haskell-compiler-inliner.pdf) remarks that they tried a couple varieties and found all of them too complicated and bug ridden to be worth pursuing.
I'm happy to believe them and learn from their experience rather than my own.
In this case, however, the frequency it arises in practice and the simplicity of implementation make it worthwhile.

How do we know when we should inline a variable though?
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

`should_inline`, unsurprisingly, makes it decision based on how the variable's occurrence.
Our first two cases are alarming:

```rs
Occurrence::Dead | Occurrence::Once =>
  panic!("ICE: should_inline encountered unexpected dead or once occurrence. This should've been handled prior"),
```

We handled `Once` and `Dead` in `simplify_var`.
If we somehow see them in `should_inline`, we have a logic error.
Call somebody to fix the compiler.

`OnceInFun` is where we actually start deciding if we should inline:

```rs
Occurrence::OnceInFun => 
  ir.is_value()
    && self.some_benefit(ir, ctx),
```

We'll inline our variable if its definition is a value and there's some benefit to inlining it.
I don't know when there's some benefit to inlining, but sounds great on paper!
Before figuring out what that means, let's talk about what it means for an IR to be a value.

An IR is a value when it contains no work to be done.
For our simple language, this is just `App`.
If our IR contains an `App` it's not a value, otherwise it is.
With on caveat, `Fun`s are value regardless of their body.
Because a `Fun` body is not run until the function is applied we count them as values and not work.
`is_value` is then a recursive match over `IR` looking for applications:

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

Now that we know what a value is, we can look into `some_benefit`

```rs
fn some_benefit(
  &self, 
  ir: &IR, 
  ctx: &Context
) -> bool {
  todo!()
}
```

Its beneficial to inline a variable when it opens up new opportunities for optimization.
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

The first benefit we look for is any argument that might justify inlining.
That is an argument that is either nontrivial or a variable with a known definition:

```rs
if args
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
{
  return true;
}
```

An `IR` is trivial if it is a variable or an integer:

```rs
impl IR {
  fn is_trivial(&self) -> bool {
    matches!(self, IR::Var(_) | IR::Int(_))
  }
}
```

Trivial arguments generally aren't going to open up more optimization opportunities.
It's hard to get more optimal than an integer literal.
Correspondingly, non-trivial expressions can open the door to more optimizations.
If we can saturate a parameter with a non-trivial expression this is likely to lead to further optimizations, so we want to do it.
Similarly, if our variable has a known definition, it can open the door for optimizations if we decide to inline that variable.

After checking for interesting arguments, we check if we have more arguments than parameters:

```rs
args.len() >= params.lens()
```
This check accmpolishes two things in one:
  * Check if we saturate all our parameters with arguments.
  * Check if we apply more arguments after saturating our functions.

Saturating all our parameters means we can drop the function nodes by inlining.
Function nodes require allocation, we'll learn why later when we emit code.
Ergo dropping our function by inlining reduces allocation, a performance win.
Further, if all our callsites saturate the function, we can inline them all and drop the variable's binding entirely.

When we saturate all our parameters and still have arguments to spare, it's likely inlining will create more opportunities for optimization.
That cinches `some_benefit`, back in `simplify_var` we have one final case to cover:

```rs
Occurrence::Many => {
  let size = ir.size();
  let no_size_increase = size == 0;
  let small_enough = size <= self.inline_size_threshold;
  ir.is_value()
    && (no_size_increase
        || (small_enough && self.some_benefit(ir, ctx)))
},
```

`Many` is where we must be the careful about inlining.
Correspondingly, it has the most involved check to decide inlining is worthwhile.
It is also where we do the most speculative reasoning.
We gesture to vague notions such as `no_size_increase` and `small_enough` to make our decision.

The "size" of an IR term is somewhat surprising:

```rs
impl IR {
  fn size(&self) -> usize {
    match self {
      IR::Var(_) | IR::Int(_) => 0,
      IR::Fun(_, body) => 10 + body.size(),
      IR::App(fun, arg) => arg.size() + size_app(fun, 1),
      IR::TyFun(_, ir) => ir.size(),
      IR::TyApp(ir, _) => ir.size(),
      IR::Local(var, defn, body) => {
        defn.size()
          + body.size()
          + (if var.ty.is_stack_alloc() { 0 } else { 10 })
      }
    }
  }
}
```

Rather than the number of nodes in our IR term, as we might expect, `size` gives us size modified to account for runtime performance and keenness to inline.
An example of this can be found in our variable and integer cases.
We give them size 0 because we're quite eager to inline them, even in multiple places.
Type functions and applications return the size of their underlying term because they do not impact runtime, and so we do not want them impact inlining decisions.

I must profess to not fully understanding our size calculation here, though.
For example, why are all our sizes multiplied by 10?
I do not know.
Our `size` function comes to us from GHC, which is my bible when it comes to compiler construction.
You can find their version [here](https://hackage-content.haskell.org/package/ghc-lib-parser-9.8.5.20250214/docs/src/GHC.Core.Unfold.html#sizeExpr).
What we have here is a stripped down version that applies to the subset of our language.

Tangent aside, locals consider the runtime performance as well.
The size of a local is the definition size plus the body size.
If the type of our local is a stack allocated, we add zero to our size, otherwise we add one.
A type is stack allocated if it's `Type::Int`.

Our stack/heap distinction in size is guided by runtime performance.
A stack allocated local requires no allocation, and so does not contribute to size.
Moving on, our application case doles out to `size_app`:

```rs
fn size_app(ir: &IR, arg_count: usize) -> usize { 
  match ir {
    IR::App(fun, arg) => arg.size() + size_app(fun, args + 1),
    IR::TyApp(ir, _) => size_app(ir, args),
    ir => ir.size() + 10 * (1 + args),
  }
}
```

`size_app` counts the number of non-type arguments until we reach the head of our application.
The number of arguments plus the size of our head determines the size of our application.
Now that we know how to size our terms, we can make more sense of our `Many` case:

```rs
Occurrence::Many => {
  let size = ir.size();
  let no_size_increase = size == 0;
  let small_enough = size <= self.inline_size_threshold;
  ir.is_value()
    && (no_size_increase
        || (small_enough && self.some_benefit(ir, ctx)))
},
```

`no_size_increase` is for cases like variables or integers.
It costs us nothing to replace a variable by another variable, so we might as well do it.

`small_enough` is the main inlining heuristic.
We pick a number, `inline_size_threshold` in this case which defaults to 60, and any term below that size we'll consider inlining.
Picking the right number is tricky and very much contextual based on the codebase being compiled.
There's interesting research into picking better numbers for inling that we won't cover here.
Our number is by no means gospel, it's just the default GHC uses.

With our newfound understanding, we can read off our final conditional.
Our IR has to be a value, we never want to duplicate work by inlining.
We'll then inline if either:
  * It doesn't increase our size (a variable or an integer for example).
  * There's some benefit to inlining and our definition is small.

  * TODO: Figure out how to note this.
Only `some_benefit` relies on the context we're in.
The rest of these values are purely determined by `ir` and could be calculated once and cached.

That's all our simplify cases.
We can move on to the second half of our equation `rebuild`.
`simmplify` drilled down into our term building up context.
`rebuild` will consume that context to build up our simplified term.
As we build up our term, we'll still be looking for optimization opportunities.
  * TODO: transition to the code.

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

For each context entry, we restore our saved substitution.
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

Why do we turn a function applied to its argument into a local?
Couldn't we simply substitute the argument into the body directly?
Of course, we could, but it's not a given that doing so would be beneficial.
By creating a local for our argument and simplifying it, we accomplish two goals:

* We remove the `Fun`, reducing allocations
* We reuse all our logic for inlining locals

If our argument is only used once, simplification will pick that up and remove our local.

If `ir` isn't a `Fun`, we construct an `App` and continue.
We didn't simplify our argument when we pushed it into the context, so we simplify our argument now.
It's important we do this with an empty context.
`simplify` will eventually call `rebuild` on our arg, and we want it to only rebuild context that's part of our argument, not our argument plus `ir`'s context.
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
Types are going to have a runtime cost, so we don't have to worry about inlining them.
There are some memory concerns in creating exponential instances of a type like this.
But these are relegated to the compiler, not the produced program, and we won't worry about it (for now atleast).

Rebuilding a `TyFun` is simple:

```rs
ContextEntry::TyFun(kind) => {
  ir = IR::ty_fun(kind, ir);
}
```

Nothing to optimize or even simplify here, we create our node and move on.
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
We did this so that we canc make inlining decisions based on the simplified definition of the local.
After deciding what to do, we still have to simplify the body of our local.

First we check if our definition is trivial, recall this means it's a variable or an integer.
These are always free to inline because they're small and do no work.
We add our definition to our substitution and proceed by simplifying our body.
Because we've already simplified the definition, we add it to our substitution as `Done` preventing further simplification when we inline.

When our definition is nontrivial, we assume we're going to emit a binding.
We update our in-scope map with our binding and simplify body.
While simplifying our body, we might inline all occurrences of our variable.
To account for this, we check our variable's occurrence info before comitting to creating a binding.
If our variable is dead, we can drop our local, otherwise we construct our local.

With all our components, we can finally assemble our main entrypoint the `simplify` method.

```rs
fn simplify(&mut self, mut ir: IR) -> IR {
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

This is a helper method that checks if we performed any simplifications to our `ir`.
If we didn't, we exit our loop early.

We do all this in a loop that runs a hardcoded number of times, 2 at the moment.
Simplification is a fixpoint operation.
We do as much as we can in one pass, but we can't do everything.
For example, if we inline every occurrence of a `Many` variabale, we won't clean up the now dead binding during that pass.

We could run our simplifier until a true fixpoint by looping infinitely instead of for 2 iterations.
For our base language, this would be sufficient.
Base is simple enough that we could have confidence we would reach that fixpoint.
Later features however will complicate this story, recursion in particular can allow us to construct degenerate cases that do not reach a fixpoint.

In the interest of future proofing, we setup a fixed number of iterations now.
For reference GHC has a similar setup.
There simplification can run up to 4 times.
OCaml's FLambda2 backend does everything in a single pass, which if you think about is really a single iteration loop.

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
Let's take a look at a really convoluted IR:


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
