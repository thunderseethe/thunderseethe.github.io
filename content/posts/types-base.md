+++
title = "Pushing Past the First Error During Type Inference"
date = "2025-11-10T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Type Inference"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Type Inference", "Bidirectional Typechecking", "Unification", "Union-Find", "Constraint Solving"]
description = "Improving our type inference to support multiple errors and interactive IDE use cases"
+++

{{< accessory title="Making a Language" >}}
This post is part of the [making a language series](/series/making-a-language).
A series that teaches you how to implement a programming language using Rust.

Today's post builds on the previous [type inference post](/posts/unification/) for the base language.
{{< /accessory >}} 

[Last we left off](/posts/unification), we had a complete and working type inference system.
An admirable achievement and at the time our completed product.
I went off to write the remainder of the making a language series, and you went off to read it (I hope).
As I've written more of the language, my ambitions have focused.
Originally this series set out to write a compiler, pretty much any compiler would do and be challenging enough.
Now, however, it won't be any old compiler, but a query-based compiler, supporting the [Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/).

I think writing your compiler with the intent for users to call it while editing is mandatory in the modern era.
Gone are the days of assembling punch cards ahead of time to feed to the computer all at once.
People can scribble code on their phones, and they expect you to tell them where the type errors are as they do it.
Achieving our dream, however, is going to require some changes to type inference.

Our type inference isn't yet ready to handle interactive usage.
Interactive usage requires we can handle partially constructed inputs gracefully.
If the user is in the middle of typing a function, we still want to type check the remaining functions in the file.
We'll need three improvements to our type inference in support of that goal:

  * Add a `NodeId` to `Ast`
  * Add a new `Ast` node `Hole`
  * Report multiple errors

`NodeId` is a unique identifier for each node in our tree.
Uniquely identifying nodes helps us track metadata about our AST.
With `NodeId`, we can have hashmaps such as `HashMap<NodeId, Type>` that track the type of each node.
Or maybe, as we construct the frontend of our compiler, we want a `HashMap<NodeId, Range<usize>>` to track the source location of each AST node.

We'll also need a new `Ast` case: `Hole`.
A `Hole` represents a missing portion of the Ast.
Passes prior to type inference will produce a `Hole` when they encounter erroneous input.
Type inference needs to handle `Hole` such that it does not block type inference but also does not change its outcome.
An error in the `Ast` shouldn't impact the inference of correct types.

`Hole` helps us achieve those things by treating parts of the program we've determined to be invalid as a black box.
`Hole`s can have types, but they are always ascribed a type variable to be unified with anything else.
They never create a type of their own and any unification with them will succeed.

Finally, we want to report multiple errors, rather than stopping at the first error.
Care must be taken while doing this.
It's easy to have one error spawn a cascade of unhelpful errors that are all caused by the initial infraction.
We want to provide a single error for any particular invalid piece of our Ast, but support reporting multiple distinct invalid parts of our Ast.

## Adding Ast Identification

Adding NodeId is straight forward in type inference because we're not responsible for creating the IDs.
Just like `Var`, we assume some prior pass has correctly assembled `NodeId` and fed it to us.
The `Var` similarties don't stop there.
A `NodeId` is also just a wrapper around a `u32`:

```rs
struct NodeId(u32);
```

Our new `NodeId` is embedded into each of our `Ast` cases:

```rs
#[derive(Debug, Eq, Clone)]
pub enum Ast<V> {
  /// A local variable
  Var(NodeId, V),
  /// An integer literal
  Int(NodeId, i32),
  /// A function literal (lambda, closure).
  Fun(NodeId, V, Box<Ast<V>>),
  /// Function application
  App(NodeId, Box<Ast<V>>, Box<Ast<V>>),
}
```

{{< accessory title="That seems reptitive" >}}
If `NodeId` is on every node, can't we factor it out into its own field?

```rs
struct Ast<V> {
  id: NodeId,
  kind: AstKind<V>
}
enum AstKind<V> {
  Var(V),
  Fun(V, Box<V>),
  // ...the rest of our cases
}
```
Yes we can! In fact, this is the norm in many production compilers.
It turns out people love attaching metadata to every node.
We've opted to elide this representation for our usecase because it makes pattern matching more verbose, and we're focused on legibility. 
I wanted to avoid having to write code like:

```rs
Ast {
  id: ast.id,
  kind: match ast.kind {
    //...
  }
}
```

But that's purely a style preference.
Do what you like in your language.

{{</ accessory >}}

With that our nodes are identifiable.
We can start attaching metadata to our AST.
As we'll see later, we're going to use this to attach errors to our tree with `HashMap<NodeId, TypeError>`.

Is `NodeId` really the best solution to this problem though?
Could we not use a `HashMap<Ast, TypeError>` instead and save ourselves the trouble?
One immediate issue arises, 
`Ast` has no sense of identity.
If we have an Ast with two separate instances of `Int(3)`, we might want to attach an error to one but not the other.
But both of them are the hash to the same key in our hashmap.
`NodeId` provides a unique `u32` for the two different instances of `Int(3)`, providing different keys in our hashmap.

We also have a more pedestrian concern about performance.
A hashmap keyed by node is both copying the entire node acting as our key and hashing it.
Performance is by no means our first consideration, but this will get quite expensive for large trees.
`NodeId` hashes a `u32` regardless of the node size it represents.

It doesn't suffice to key our side tables by `Ast`.
We want to track the content _and_ the position of a node in the `Ast`.
Okay so we need to track position, but couldn't we do that by attaching metadata to `Ast` directly?
We already create multiple instances of a node as part of constructing our `Ast`.
If each node holds its own metadata, we've got instanced metadata for free.
It'd be easy to embed our metadata directly in `Ast`:

```rs
struct Ast<V> {
  kind: AstKind<V>,
  error: Option<TypeError>,
  type: Type,
}
enum AstKind<V> {
  Var(V),
  Fun(V, Box<V>),
  // ...the rest of our cases
}
```

This approach works, make no mistake, putting it ahead of `Ast` keys.
We avoid this approach because it doesn't provide good separation of concerns.
Now, when we construct an `Ast` node, we have to provide all the metadata we're ever going to care about for every pass:

```rs
Ast {
  kind: Var(Var(0)),
  error: None,
  ty-
}
```

Actually shoot, what do we put for our type?
We haven't type checked our node yet, do we just guess a type knowing it will be replaced?
And how do we replace the `type` anyways?
Until now our `Ast` has enjoyed being immutable.
I guess we gotta make it mutable.

Bundling all our metadata forces us to contend with all of it all stages of compilation, which I find makes for more brittle code with invariants easier to mess up.
Keeping metadata in a `HashMap<NodeId, Metadata>` allows for carrying the relevant metadata for each use case.
Keeping just the data we need and decoupling it from the `Ast` itself allows us better decoupling between our compiler passes.

{{< notice note >}}

I'm frowning on embedding metadata in favor of hashmaps here, but it is a tradeoff.
Carrying around extra hashmaps is not free, and performing the hash lookups on your node IDs is not free.
In real compilers you'll find a mix of approaches, where particularly hot data is kept in the tree because it's needed often enough (or it's small enough) that keeping it in a separate collection degrades performance meaningfully.

{{< /notice >}}

## Holy Ast, Batman!

Next on our list of renovations is adding a new case to our Ast:

```rs
enum Ast<V> {
  // ...our existing cases
  Hole(NodeId, V),
}
```

`Hole` is all about error correction.
Our compiler will see code in a half constructed state.
Hole represents portions of our `Ast` that aren't valid (at the moment).
Essentially unblocking our `Ast` by treating invalid portions as a black box.

We can imagine a user constructs a partial function with a correct parameter but an invalid body.
We could represent that as:

```rs
Fun(Var(0), Hole(Var(1)))
```

It's worth pondering why a `Hole` holds a variable `V` and not just a `NodeId`.
One reason is its convenient for diagnostics to be able to attach names to our holes.
But more importantly it so we can attach a type to `Hole` nodes via `TypedVar`.

The goal of inferring types for `Hole` isn't to accurately determine their type.
We know that's not possible, since they represent an invalid section of program.
Holes support type inference so they can unblock type inference for the valid portions of our program.
Our rationale is that an invalid `Ast` can happily inhabit any type.

The [principle of explosion](https://en.wikipedia.org/wiki/Principle_of_explosion) at work.
An invalid `Ast` contradicts our invariants, so we're free to derive anything we like about it.
The same way a function that loops infinitely can be treated as returning any type.
No matter the type we pick, the function will never return, so it is vacuously true.

Holes _can_ be ascribed any type, but we want to pick our type carefully.
Technically we can give every hole the `Int` type, but this will cause our Hole to have undue impact on type inference.
Any type variable that encounters a hole will unify with `Int` and that will meaningfully change the outcome of type inference.
Our goal in typing holes is to get them out of the way, so we can get back to the important work of typing the valid portions of our Ast.

The idea of using typed holes comes to use from [Hazel](https://hazel.org/)'s [Total Type Error Localization and Recovery with Holes](https://dl.acm.org/doi/full/10.1145/3632910).
A paper I highly recommend.
They lay out how to introduce type holes and use a splash of gradual typing to allow them to get out of the way of typer inference.
Each hole is assigned an error type (spelled `?`) that unifies with any other type.
By making `?` equate to any other type, holes never cause their own type error and don't influence the type inference of the valid Ast.

We'll employ a similar idea, but reuse an existing concept that serves our purposes: the type variable.
Holes will always be assigned a fresh type variable.
Like Hazel's error type, a fresh type variable will always unify with a type.
Unlike error type, after that first unification our type variable is solved providing the possibility that our hole fails to unify if it encounters another type.
In practice, I've found this not to be an issue, but our language also lacks a branching construct right now, so I'm prepared to eat my words.

Our actual modifications to type inference for `Hole` are minimal.
We add one case in `infer`:

```rs
Ast::Hole(id, v) => {
  let var = self.fresh_ty_var();
  (
    GenOut::new(vec![], Ast::Hole(id, TypedVar(v, Type::Var(var)))),
    Type::Var(var),
  )
}
```

We do not check holes.
That's it.

## Multitudes of Errors

Our final amendment is to support reporting multiple errors rather than stopping at the first.
The first pass of type inference, `infer` and `check`, always produce a list of constraints.
We don't have to worry about any error reporting from them.
We'll want to modify constraints to better support error reporting, but our efforts will focus on unification.

First, we need a way to store multiple errors.
We add a new field to `TypeInference`:

```rs
struct TypeInference {
  // ... our previous fields
  errors: HashMap<NodeId, TypeError>,
}
```

Another idea from Hazel, we're marking up our tree with type errors, accomplishing two tasks: any given node has at most one error and the tree's hierarchy provides a hierarchy for our errors.
If we see an error on a given node, we can ignore any errors attached to subnodes.
We know they are superfluous.
We are, of course, still free to report the subnode errors, if we think they're useful.
The important part is being able to prevent a cascade of errors caused by one root issue.

As part of reporting multiple errors, we're going to improve the errors we report.
Which means we're going to need some new cases in our `TypeError`:

```rs
enum TypeError {
  InfiniteType {
    type_var: TypeVar,
    ty: Type,
  },
  UnexpectedFun {
    expected_ty: Type,
    fun_ty: Type,
  },
  AppExpectedFun {
    inferred_ty: Type,
    expected_fun_ty: Type,
  },
  ExpectedUnify {
    checked: Type,
    inferred: Type,
  },
}
```

Two of our cases, `InfiniteType` and `ExpectedUnify`, are familiar but renamed.
These are the two cases we previously had in `TypeError`.
Our two new cases are structurally the same as `ExpectedUnify`: two types.
These two cases are so similar in fact, that unification actually treats them the same.
`unify_ty_ty` produces a `UnificationError` that looks just like our old `TypeError`:

```rs
enum UnificationError {
  TypeNotEqual(Type, Type),
  InfiniteType(TypeVar, Type),
}
```

`unification` turns `UnificationError` into `TypeError`.
It's able to do this because `UnexpectedFun` and `AppExpectedFun` encode the same information as `TypeNotEqual`.
They just specify where the inequality occurred, allowing us to report a more specific error message.
By the time we've reached `unification` we've thrown away our `Ast` that tells us where our error arose.
We rely on `Constraint` to propagate this information from the `Ast` to unification.
Constraints do this using a new type called provenance:

```rs
enum Provenance {
  UnexpectedFun(NodeId),
  AppExpectedFun(NodeId),
  ExpectedUnify(NodeId),
}
```

Gee, I wonder how that's going to map onto `TypeError`.
Each `NodeId` points at the erroneous Ast node.
Another handy feature of uniquely identifying nodes.
We embed our new type in `Constraint`:

```rs
enum Constraint {
  // TODO: NodeId might be better represented by some kind of like provenance. Not sure yet.
  TypeEqual(Provenance, Type, Type)
}
```

Turns out that comment had uncanny foresight.
A valuable lesson in how documentation bitrots, even for blogs.
We now need to provide a provenance where ever we emit a constraint.

This is actually a larger change than it sounds like.
For the sake of error reporting, we're not just going to amend each constraint.
We're going to change how we infer types to better report errors.
Our first change, in `App`, is to infer function types first rather than argument types:

```rs
Ast::App(id, fun, arg) => {
  let fun_id = fun.id();
  let (fun_out, supposed_fun_ty) = self.infer(env.clone(), *fun);
  let mut constraint = fun_out.constraints;
  let (arg_ty, ret_ty) = match supposed_fun_ty {
    Type::Fun(arg, ret) => (*arg, *ret),
    ty => {
      let arg = self.fresh_ty_var();
      let ret = self.fresh_ty_var();

      constraint.push(Constraint::TypeEqual(
        Provenance::AppExpectedFun(fun_id),
        ty,
        Type::fun(Type::Var(arg), Type::Var(ret)),
      ));

      (Type::Var(arg), Type::Var(ret))
    }
  };

  let arg_out = self.check(env, *arg, arg_ty);
  constraint.extend(arg_out.constraints);
  (
    GenOut::new(
      constraint,
      Ast::app(id, fun_out.typed_ast, arg_out.typed_ast),
    ),
    ret_ty,
  )
}
```

We now call `infer` on our function and check if the result is a function type.
If it's not, we add a constraint against a fresh function type with one of our new provenance's `AppExpectedFun`.
Originally we inferred the argument type and used it to check our function type.
An approach that makes great use of our type variables, but makes it difficult to determine our argument has been applied to something that isn't a function.

Our next provenance is a modification to `check`:

```rs
(Ast::Fun(id, arg, body), ty) => {
  let mut constraints = vec![];
  let (arg_ty, ret_ty) = match ty {
    Type::Fun(arg, ret) => (*arg, *ret),
    ty => {
      let arg = self.fresh_ty_var();
      let ret = self.fresh_ty_var();

      constraints.push(Constraint::TypeEqual(
        Provenance::UnexpectedFun(id),
        ty,
        Type::fun(Type::Var(arg), Type::Var(ret)),
      ));

      (Type::Var(arg), Type::Var(ret))
    }
  };
  let env = env.update(arg, arg_ty.clone());
  let body_out = self.check(env, *body, ret_ty);
  constraints.extend(body_out.constraints);
  GenOut {
    constraints,
    typed_ast: Ast::fun(id, TypedVar(arg, arg_ty), body_out.typed_ast),
  }
}
```

Previously, our match arm was `(Ast::Fun(...), Type::Fun(...))`, only firing when a function meets a function type.
It now matches any `ty` and when that type isn't a function type, we emit a constraint using `UnexpectedFun`.
With the previous match arm, we would fall down to the `(ast, expected_ty)` and rely on it to emit our constraint for non-function types.
Now that we want to specify our provenance, we need to handle it in the function case.

Finally, our most modest change is in `(ast, expected_ty)`:

```rs
(ast, expected_ty) => {
  let id = ast.id();
  let (mut out, actual_ty) = self.infer(env, ast);
  out.constraints.push(Constraint::TypeEqual(
    Provenance::ExpectedUnify(id),
    expected_ty,
    actual_ty,
  ));
  out
}
```

This case is effectively the same, but we mark our constraint with provenance `ExpectedUnify`.
Our constraints carry their provenances into `unification` where they construct errors:

```rs
if let Err(kind) = self.unify_ty_ty(left, right) {
  let (node_id, mark) = match kind {
    UnificationError::InfiniteType(type_var, ty) => (
      provenance.id(), 
      TypeError::InfiniteType { type_var, ty }
    ),
    UnificationError::TypeNotEqual(left, right) => match provenance {
      Provenance::UnexpectedFun(node_id) => (
        node_id,
        TypeError::UnexpectedFun {
          expected_ty: left,
          fun_ty: right,
        },
      ),
      Provenance::AppExpectedFun(node_id) => (
        node_id,
        TypeError::AppExpectedFun {
          inferred_ty: left,
          expected_fun_ty: right,
        },
      ),
      Provenance::ExpectedUnify(node_id) => (
        node_id,
        TypeError::ExpectedUnify {
          checked: left,
          inferred: right,
        },
      ),
    },
  };
  self.errors.insert(node_id, mark);
}
```

We can see `UnificationError` and `Provenance` are combined to produce a `TypeError`. 
Our new error marks our `node_id` in `errors`.
With that we've done it.
We can report multiple errors in our type inference.

With great power, comes a change in our `type_infer` signature.
Now that we could have multiple errors it no longer makes sense to return a `Result<(Ast<TypedVar>, TypeScheme), TypeError>`.
Boy that's a mouthful.
We could return a `Result<(Ast<TypedVar>, TypeScheme), HashMap<NodeId, TypeError>>`, but rather than further burden our already sagging return type, we're going to return both our typed ast and it's errors:

```rs
struct TypeInferOut {
  ast: Ast<TypedVar>,
  scheme: TypeScheme,
  errors: std::collections::HashMap<NodeId, TypeError>,
}
```

`type_infer` returns a `TypeInferOut` containing both our typed ast and our errors.
Our change to return both our errors and Ast is motivated by supporting interactive usage.
Rather than throwing out our entire Ast at the first sign of trouble, we want to salvage what we can from the Ast even when it contains errors.
It also makes it easier to thread our errors through our other frontend passes to report diagnostics if we include our Ast so we can match them up with the Ast's in our other passes.
We call this property of error tolerance resilience.

In the interest of interactivity, we'll want all our frontend passes to be resilient.
That's a problem for our future posts though.
For now, we've achieved resilient type inference.
From here we can either continue on to [lower our typed ast](TODO), or continue type inference with [row types](TODO)
