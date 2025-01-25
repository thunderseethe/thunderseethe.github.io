+++
title = "Part 5a: Lowering our AST into an intermediate representation"
date = "2025-01-27T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Lowering"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Lowering", "Elaboration", "IR", "System F", "DeBruijn Index"]
description = "Lowering our typed AST into a System-F based IR"
draft = true
+++

We've been in type checking so long it's becoming a tar pit deep enough to rival picking a parser.
Our only hope of escape is to delve deeper.
Lest we find ourselves fretting over the endlessly enticing type checker features available to adjoin.
We're always free to return to our type checker older and wiser.
But this series is called making a language, not type check until our motivation evaporates.

Fortunately delving deeper is exactly what our next compiler pass is all about: Lowering.
Lowering is the process of converting our typed AST into an [intermediate representation](https://en.wikipedia.org/wiki/Intermediate_representation) (IR).
It marks a fundamental shift in our compiler from frontend to backend.

## Lowering

The AST used in the frontend of our compiler makes a lot of concessions for the user writing code.
Users don't want to write out obvious metadata, like types, and the AST gets that.
Any types they leave out, it'll infer on their behalf.
If the AST sees an error, it's probably because the user gave us invalid code, and it'll tell them that with a nice diagnostic.

All that falls away with the move to an IR.
We are now much more concerned with representations that are helpful to the compiler, not the user.
The IR sits between the frontend and machine code emission (hence the name intermediate).
It exists to accommodate optimizations before being translated to machine code.

To this end, IR makes metadata very explicit. 
Metadata that would be a slog for users to write out but ease the compiler applying optimizations.
The IR is where we start caring about memory layout and calling conventions.
Alongside this shift, our mentality around errors changes.
During lowering, we know our AST has successfully type checked.
The type checker's approval gives us a strong guarantee.

We're guaranteed if we see something unexpected, it's now due to an internal compiler error, not a user error.
Accordingly, we no longer need the recoverable error handling of `Result`.
When we encounter an error, we'll immediately `panic!` (and maybe cry).
A panic indicates we have a bug in our compiler to fix.
In practice, our compiler shouldn't actually panic.
Robust compilers would perform more graceful error handling.
But for our purposes, panicking suffices due to its simple implementation.

Enlightened with the right mindset, let's delve into implementing lowering.
Our pass is encapsulated by the `lower` function:

```rs
fn lower(
  ast: Ast<TypedVar>, 
  scheme: ast::TypeScheme
) -> (IR, Type) {
  todo!()
}
```

`lower` takes the output of our typechecker, an `Ast<TypedVar>` and a `TypeScheme`, and converts them into an `IR` and a `Type` respectively.

### IR

`IR` is the new tree datatype that represents our intermediate representation:

```rs
enum IR {
  Var(Var),
  Int(isize),
  Fun(Var, Box<Self>),
  App(Box<Self>, Box<Self>),
  TyFun(Kind, Box<Self>),
  TyApp(Box<Self>, Type),
}
```

`IR` looks almost exactly like our `Ast`.
This is because our IR is based on [System F](https://en.wikipedia.org/wiki/System_F).
System F is another calculus, like the lambda calculus, often called the polymorphic lambda calculus.
Named such because it is the lambda calculus plus two new nodes: type function and type application.

Given our AST is the lambda calculus, it makes a lot of sense our System F-based IR looks like `Ast` but with two new nodes.
As the name polymorphic lambda calculus implies, our two new nodes deal with polymorphism:

  * `TyFun` - type function, a function that binds a type variable in a term.
  * `TyApp` - type application, applies a type to a type function to produce a term.

These nodes mirror `Fun` and `App`.
Where functions take a term and produce a term, type functions take a type and produce a term.
`TyFun` and `TyApp` work together to implement generics in our `IR`.
Each type variable from our `TypeScheme` will become bound by a `TyFun` node in our `IR`.

We would expect `TyFun` to contain a `TypeVar`, same as `Fun` contains a `Var`.
The reason it doesn't lies in our IR's representation of type variables.
Rather than normal names, like `Var` in `Fun`, our type variables use [DeBruijn Indices](https://en.wikipedia.org/wiki/De_Bruijn_index).
Using DeBruijn Indices allows us to efficiently check if two types are equal. 
Don't worry if you don't know what DeBruijn Indices are. 
We'll discuss it more when we talk about our `Type`s.

When we want to instantiate a generic, we use a `TyApp` to apply a type to our `TyFun`.
Applying a type removes the `TyFun` node leaving us with the underlying term, but every instance of our bound type variable has been replaced by our applied type.
The same way `App` works on `Fun` for values.

Representing generics as nodes of our IR makes it very easy to see where polymorphism occurs.
Correspondingly, it also makes it easy to see where it does not occur, which is invaluable for knowing when certain classes of optimizations apply.
A lot has been said about System F; it's well tread in the realm of theory.
We won't cover it here, but if you are interested check out:
  
  * [TAPL, Chapter 23](https://www.cis.upenn.edu/~bcpierce/tapl/)
  * [Lecture 8: Polymorphism and System F](https://www.cs.utexas.edu/~bornholt/courses/cs345h-24sp/lectures/8-system-f/)
  * [Into the Core - Squeezing Haskell into Nine Constructors](https://www.youtube.com/watch?v=uR_VzYxvbxg)

Our use of System F is motivated by its handling of polymorphism.
Its theoretical underpinnings are just a nice to have.

The next difference in our `IR` is variables.
`Var` is similar to AST's `TypedVar`:

```rs
struct Var {
  id: VarId,
  ty: Type,
}
```

We no longer have untyped variables so no need to distinguish them.
`VarId` is our familiar integer counter:

```rs
struct VarId(u32);
```

It comes with all the usual uniqueness guarantees: all `VarId`s in a term are unique.

### Explicitly Typed

Our `IR` is noteworthy for being typed.
Many `IR`s targeted by lowering are untyped.
We've already checked that all the types line up.
What's the point of keeping them around?
Part of this choice is motivated by our use of System F.
`TyFun` and `TyApp` wouldn't have a lot to do if we had no types.
Another part of the decision is motivated by keeping me (and hopefully you) sane.

We're going to be transforming our `IR` a lot in the coming compiler passes.
Types allow us to sanity check that after we've mangled our `IR` it still means what we think it means.
In writing the tests for lowering, type checking the `IR` has squashed bugs I introduced.

Fortunately for us, we don't have to spend a lot of time type checking our `IR`.
We already did the hard work of figuring out the types during unification.
Our `IR` saves all the work unification did, so we can reconstruct types without having to do any inference.

Let's take a look at our IR `Type` and see how we can quickly construct it for an `IR` term using `type_of`:

```rs
enum Type {
  Int,
  Var(TypeVar),
  Fun(Box<Self>, Box<Self>),
  TyFun(Kind, Box<Self>),
}
```

`Type` mirrors our `Ast` type with one new friend: `TyFun`.
`TyFun` is the type of `IR::TyFun`, same as `Fun` is the type of `IR::Fun`.
Instead of a type, it takes something called a `Kind`.
`Kind` makes sure we're passing the right _kind_ of type to our `TyFun`. 
The same way types make sure we're passing the right _type_ of value to our `Fun`.
Our usage of `Kind` will be quite boring:

```rs
enum Kind {
  Type,
}
```

Every `Type` has kind `Type`.
Later on this will be more interesting. 
We'll introduce a `Row` kind.
For now, rest easy knowing we can't mess up our `Kind`s.

### type_of()

Now that we know what `Type` looks like, we can construct it for our `IR`:

```rs
impl IR {
  fn type_of(&self) -> Type {
    match self {
      // ...
    }
  }
}
```

Our first two cases are simple:

```rs
IR::Var(v) => v.ty.clone(),
IR::Int(_) => Type::Int,
```

Int's have type Int.
Our IR `Var`s always have their type, thanks unification, so all we have to do is return it.
Next up is functions:

```rs
IR::Fun(arg, body) => Type::fun(arg.ty.clone(), body.type_of()),
IR::TyFun(kind, body) => Type::ty_fun(*kind, body.type_of()),
```

Both function nodes are typed similarly.
Use the `type_of` body and the argument to construct the respective function type.
`App` is our first case that has to do some work to get a type:

```rs
IR::App(fun, arg) => {
  let Type::Fun(fun_arg_ty, ret_ty) = fun.type_of() else {
    panic!("ICE: IR used non-function type as a function")
  };
  if arg.type_of() != *fun_arg_ty {
    panic!("ICE: Function applied to wrong argument type");
  }
  *ret_ty
}
```

Because we know our code type checks, we can assume `fun` has a function type, and panic if it doesn't.
We can also assume our function's `fun_arg_ty` is equal to `type_of` arg type, panicking when it's not.
If that all goes well, our `App`'s type is the return type of our function.

#### Type Equality

It's worth pausing for a moment to consider `arg.type_of() != *fun_arg_ty` in more depth.
As we stated earlier, our `TypeVar`s are using something called DeBruijn Indices.
They're supposed to make checking types for equality faster.
If you're unfamiliar with DeBruijn Indices, I've written about how they work [here](/posts/debruijn-indices).
We'll just explain the problem they solve here.

To exemplify our problem, let's imagine an alternative `TyFun` that, like `Fun`, takes a `TypeVar`: `TyFun(TypeVar, Box<Self>)`.
Consider two types `foo` and `bar` using this alternate `TyFun`:

```rs
let a = TypeVar(1493);
let foo = TyFun(a, Fun(Var(a), Var(a)));

let b = TypeVar(762);
let bar = TyFun(b, Fun(Var(b), Var(b)));
```

`foo` and `bar` are not equal because `a` does not equal `b`.
But they should be.
It's true these type variables are syntactically different, but for all intents and purposes they are the same.
We'd like to ignore this frivolous difference in names.
Names only exist to track where we substitute types when we apply our type function.

`a` and `b` are substituted the same way in `foo` and `bar`, so `foo` and `bar` should be equal.
We can `TyApp` any type to both `foo` and `bar`, and we always get equal types back.
`foo` may not equal `bar` but `TyApp(foo, Int)` equals `TyApp(bar, Int)`.
That's enough for us to consider their types equal.
Equating things in this way is called [alpha equivalence](https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence).

Ignoring this difference in names turns out to be tricky in practice.
If we can find a substitution for our names to make two types equal, we know they're alpha equivalent.
We could solve `a` to `b` and update `foo` to replace all `a`s with `b`s.
We could also solve `b` to `a` and update `bar` to replace all `b`s with `a`s.

That sounds awful familiar.
Where else do we try to find a substitution for our type variables to make things equal?
Unification! That would just be unification in disguise.
We were supposed to leave that behind in the type checker.
You can make this approach work, albeit it's expensive and error prone, but we want something faster for our `type_of` check.

DeBruijn Indices solve the problem by picking equal `TypeVar`s for types that are alpha equivalent.
Making equality testing trivial.
Compare the type for syntactic equality; If they're syntactically equal, they're alpha equivalent.
With that tangent wrapped up, let's return to our final case of `type_of()`:

```rs
IR::TyApp(ty_fun, ty) => {
  let Type::TyFun(_, ret_ty) = ty_fun.type_of() else {
    panic!("ICE: Type applied to a non-forall IR term");
  };

  ret_ty.subst(ty.clone())
}
```

Like our sibling application, we assume `ty_fun` has type `TyFun`.
Unlike `App`, we finish by calling `subst` on `ret_ty`.
`subst` replaces every occurrence of the type variable bound by `TyFun` with `ty`.
`subst` just takes a `Type` though, not the `TypeVar` that it should replace with `ty`.
Because our types use DeBruijn Indices, we always know what type variable to substitute.
`subst` always starts off replacing `TypeVar(0)`.
Which makes sense, there are no `TyFun` nodes between `TyFun(_, ret_ty)` and `ret_ty`.

We substitute `ty` for `TypeVar(0)` in `ret_ty`.
But `ret_ty` itself may contain `TyFun` nodes. 
Fortunately, `subst` handles adjusting the `TypeVar` we're substituting correctly in that case.
We're not going to cover the details of `subst` to save on time.
Its implementation can be found in the [full code](TODO).

With that we've completed our `type_of()` function.
We're talking about `type_of()` like it's a type checker, but it's really more of a lint.
Nothing forces us to call `type_of()`.
Our transformation passes could mangle `IR` however we like and simply not check `type_of()`.
In fact, GHC has its own `type_of()`, as one of the few compilers using a typed IR, and they do precisely that.
GHC enables `type_of()` for debugging builds but turns it if off for release builds.
`type_of()` is far more lightweight than our actual type checker.

With that we've covered everything we need to know about lower-  
What's that?  
We didn't write a single line of code?  
Our `lower` function is still a giant to-do?

```rs
fn lower(
  ast: Ast<TypedVar>, 
  scheme: ast::TypeScheme
) -> (IR, Type) {
  todo!("Remember me?")
}
```

Whelp, we don't have time to fix that now.
But next time we have our work cut out for us.
We'll use our new understanding of `IR` and `Type` to finally write some gosh darn code.
