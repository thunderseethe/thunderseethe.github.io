+++
title = "Part 5: Lowering our Base Typed AST"
date = "2024-12-08T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Lowering"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Lowering", "Elaboration", "IR", "System F", "DeBruijn Index"]
description = "Lowering our typed AST into an IR"
+++

We've been in type checking so long, it's becoming a tar pit deep enough to rival picking a parser.
Our only hope of escape is to delve deeper.
Lest we find ourselves fretting over endlessly enticing features available to append to our type checker.
We're always free to return to our type checker, older and wiser.
But this series is called making a language, not type check everything under the sun.

Fortunately delving deeper is exactly what our next compiler pass is all about: Lowering.
Lowering is the process of converting our typed AST into an [intermediate representation](https://en.wikipedia.org/wiki/Intermediate_representation) (IR).
It marks a fundamental shift in our compiler from frontend to backend.

The AST used in the frontend of our compiler makes a lot of concessions for the user writing code.
Users don't want to write out obvious metadata, like types, and the AST gets that.
Any types they leave out, it'll infer on their behalf.
If we see an error, it's probably because the user gave us invalid code, and we should tell them that with a nice diagnostic.

All that falls away with the move to an IR.
We are now much more concerned with representations that are helpful to the compiler, not the user.
The IR sits between the frontend and generating the executable machine code (hence the name intermediate).
It exists to accommodate optimizations before being translated to machine code.

To this end, IR makes metadata very explicit. 
Metadata that would be a slog for users to write out, but ease the compiler applying optimizations.
The IR is where we start caring about things like memory layout and calling conventions.
Alongside this shift, our mentality around errors changes.
During lowering we know our AST has successfully type checked.
The type checker's seal of approval gives us a strong guarantee.

If we see something unexpected, it's now due to an internal compiler error, not a user error.
Accordingly, we no longer need the recoverable error checking of `Result`.
When we encounter an error, we'll immediately `panic!` (and maybe cry).
A panic indicates we have a bug in our compiler to fix.
In practice, our compiler shouldn't actually panic.
Robust compilers would perform more graceful error handling.
But for our purposes, panicking suffices due to its simple implementation.

## Lowering

Equipped with the right mindset, let's delve into implementing lowering.
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

When we want to instantiate a generic, we use a `TyApp` to apply a type to our `TyFun`.
Applying a type removes the `TyFun` node leaving us with the underlying term, but every instance of our bound type variable has been replaced by our applied type.
The same way `App` works on `Fun` for values.

Representing generics as nodes of our IR makes it very easy to see where polymorphism occurs.
Correspondingly, it also makes it easy to see where it does not occur, which is invaluable for knowing when certain classes of optimizations apply, which can be invaluable for knowing when certain classes of optimizations apply.
A lot has been said about System F, it's well tread in the realm of theory.
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

We no longer have untyped variables, so no need to distinguish them.
`VarId` is our familiar integer counter:

```rs
struct VarId(u32);
```
It comes with all the usual uniqueness guarantees: all `VarId`s in a term are unique.

### Explicitly Typed

Our `IR` is noteworthy for being typed.
Many `IR`s targeted by lowering are untyped.
We've already checked all the types line up.
What's the point of keeping them around?
Part of this choice is motivated by our use of System F.
`TyFun` and `TyApp` wouldn't have a lot to do if we had no types.
Another part of the decision is motivated by keeping me (and hopefully you) sane.

We're going to be transforming our `IR` a lot in the coming compiler passes.
Types allow us to sanity check that after we've mangled our `IR` it still means what we think it means.
Just in writing the tests for lowering, type checking the `IR` has squashed bugs I introduced.

Fortunately for us, we don't have to spend a lot of time type checking our `IR`.
We already did the hard work of figuring out the types during unification.
Our `IR` saves all the work unification did, so we can reconstruct types without having to do any inference.

Let's take a look at our IR `Type`, and see how we can quickly construct it for an `IR` term using `type_of`:

```rs
#[derive(Debug, PartialEq, Eq, Clone)]
enum Type {
  Int,
  Var(TypeVar),
  Fun(Box<Self>, Box<Self>),
  TyFun(Kind, Box<Self>),
}
```

`Type` mirrors our `Ast` type, with one new friend: `TyFun`.
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
Later on this will be more interesting, we'll introduce a `Row` kind.
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
Our IR `Var`s always have their type, thanks unification, so we just have to return it.
Next up is functions:

```rs
IR::Fun(arg, body) => Type::fun(arg.ty.clone(), body.type_of()),
IR::TyFun(kind, body) => Type::ty_fun(*kind, body.type_of()),
```

Both functions are typed similarly.
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

#### Type equality

It's worth pausing for a moment to consider how we check two types are equal.
During unification, we had to contend with the possibility that any type variable could stand for any type.
Thankfully, we no longer have to deal with that.
Our type variables are much more benign now.
Unfortunately, they are not totally devoid of problems.

Notice that our `IR::TyFun` doesn't take a `TypeVar` (unlike `Fun` which takes a `Var`).
It only takes a `Kind`.
Let's imagine an alternative `TyFun` that, like `Fun`, takes a `TypeVar`: `TyFun(TypeVar, Box<Self>)`.
Consider two types `foo` and `bar` using this alternate `TyFun`:

```rs
let a = TypeVar(1493);
let foo = TyFun(a, Fun(Var(a), Var(a)));

let b = TypeVar(762);
let bar = TyFun(b, Fun(Var(b), Var(b)));
```

This `TyFun` has a problem.
`foo` and `bar` are not equal because `a` does not equal `b`.
But they really should be.
It's true these type variables are literally different, but for all intents and purposes they are the same.
We can `TyApp` any type to both `foo` and `bar`, and we always get equal types back.
`foo` may not equal `bar`, but `TyApp(foo, Int)` equals `TyApp(bar, Int)`. 

We'd like to ignore this frivolous difference in names.
Our variables `a` and `b` are used the same way in their respective bodies. 
That's enough for us to consider their types equal.
Equating things in this way is called [alpha equivalence](https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence).

We could accomplish this with a custom `Eq` implementation for `Type`.
If we can find a substitution for our names to make two types equal then we know they're alpha equivalent.
For `foo` and `bar`, we could either substitute `a` for `b` in `foo` or vice versa in `bar`.
Hang on, finding substitutions sounds an awful lot like unification.
We're supposed to leave unification behind in the type checker.
This approach works, but we want something faster for our `type_of` check.

Tackling the problem from another direction proves more fruitful.
If we want to check two vectors contain the same elements, it can be done naively in `O(n^2)`.
For each element in one vector, iterate over the second vector and check that element exists.
We can, however, speed up our check by first sorting our vectors.
Checking sorted vectors contain the same elements becomes a linear operation.

We'll apply a similar idea when picking our type variables during type construction.
Rather than assigning a unique type variable using a counter, we'll select type variables via a standard scheme.
By normalizing how we assign type variables, constructing two equivalent types will select the same type variables.
Allowing us to rely on the fast derived `Eq` implementation for type equality.

Our method for selecting type variables is called [DeBruijn Indices](https://en.wikipedia.org/wiki/De_Bruijn_index).
Many a tutorial has been written on DeBruijn Indices.
I'll only provide a surface level understanding here.
For a more detailed account see:

  * [TAPL, Chapter 6](https://www.cis.upenn.edu/~bcpierce/tapl/)
  * [Lecture 19: Nameless lambda expressions (DeBruijn)](https://vesely.io/teaching/CS4400f20/l/19/19.pdf)

When selecting a type variable, count how many `TyFun` nodes appear between it and its binding `TyFun` node.
Use that count as the value of our type variable.
If we apply this method to our examples `foo` and `bar` we get:

```rs
let a = TypeVar(0);
let foo = TyFun(a, Fun(Var(a), Var(a)));

let b = TypeVar(0);
let bar = TyFun(b, Fun(Var(b), Var(b)));
```

There are 0 `TyFun` nodes between `a` (and `b`) and their binding `TyFun` node.
We assign them type variable `TypeVar(0)` accordingly.
Voilà! `foo` and `bar` are now equal.

Now that we are systematically assigning type variables, we don't need to list the `TypeVar` bound by a `TyFun`.
Each type variable tells us how many `TyFun` nodes to skip over to reach its binder.
This is why our `TyFun` in `IR` only includes a `Kind` and not a `TypeVar`.

We actually can't include a type variable in `TyFun`.
Because type variables list how many `TyFun` nodes to skip over to reach their binder, type variables with different values can refer to the same binder.
Consider a type that uses nested `TyFun`s, without DeBruijn indices to start:

```rs
let a = TypeVar(12);
let b = TypeVar(64);
let nested = TyFun(a, Fun(Var(a), TyFun(b, Fun(Var(b), Var(a)))))
```

When we convert to DeBruijn indices, `a` is represented by two different `TypeVar`s:

```rs
let nested = 
  TyFun(Kind::Type,
    Fun(Var(TypeVar(0)), // a
        TyFun(Kind::Type,
              Fun(Var(TypeVar(0)), // b
                  Var(TypeVar(1)) // a
                 ))));
```

Here both `TypeVar(0)` and `TypeVar(1)` refer to `a` depending on how many `TyFun` we're inside.
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
Unlike `App`, we call `subst` on `ret_ty`.
`subst` replaces every occurrence of the type variable bound by `TyFun` with `ty`.
Because our types use DeBruijn indices, we always know what type variable to substitute.
There are no `TyFun` nodes between `TyFun(_, ret_ty)` and `ret_ty`.
So the type variable bound by `ty_fun` must be `TypeVar(0)`.

We substitute `ty` for `TypeVar(0)` in `ret_ty`.
`ret_ty` itself may contain `TyFun` nodes. 
`subst` handles adjusting the `TypeVar` we're substituting correctly in that case.
`subst` implementation can be found in the [full code](TODO).
It's omitted here for brevity.

With that we've completed our `type_of()` function.
We're talking about `type_of()` like it's a type checker, but it's really more of a lint.
Nothing forces us to call `type_of()`.
Our transformation passes could mangle `IR` however we like and simply not check `type_of()`.
GHC has its own `type_of()`, as one of the few compilers using a typed IR, and they do something like this.
GHC enables `type_of()` for debugging builds, but turn if off for release builds.
`type_of()` is far more lightweight than our actual type checker.

Armed with the knowledge of `IR` and `Type`, and a couple of other details, we can finally return to our `lower` function:

```rs
fn lower(
  ast: Ast<TypedVar>, 
  scheme: ast::TypeScheme
) -> (IR, Type) {
  let (ir_ty, types) = lower_ty_scheme(scheme);

  todo!();
}
```

One line down.
That's great progress!
Now that we've explained our `IR` and `Type` we'll pick up the pace.

## Lowering Type Schemes

The first step in lowering is to lower our `TypeScheme`.
We start with the `TypeScheme` because it informs how we lower types.
Types show up both in the `TypeScheme` itself and the `Ast`, so until we know how to lower types we can't lower our `Ast`.
`lower_ty_scheme`'s signature is:

```rs
fn lower_ty_scheme(
  scheme: ast::TypeScheme
) -> (Type, LowerTypes) {
  todo!()
}
```

`lower_ty_scheme` starts by handling bound type variables.
Recall that our type scheme tracks all the generalized type variables that appear in our AST's type.
The type scheme is the only place these type variables can be introduced.
Each of these AST `TypeVar`s becomes an IR `TypeVar` bound by a `TyFun` node.
Which means we have to convert from AST `TypeVar` to DeBruijn Indices.

Fortunately, this is straightforward.
Our type scheme has all the type variables we're ever going to see and in the order we're going to see them.
We convert them to DeBruijn Indices once, and we know that will work for every type we need to lower in `Ast`:

```rs
let ty_env = scheme
  .unbound
  .into_iter()
  .rev()
  .enumerate()
  .map(|(i, tyvar)| (tyvar, TypeVar(i)))
  .collect();
```

For each type variable in `scheme.unbound`, we grab its index and use that as our DeBruijn Index.
Importantly, we do this in reverse order.
The output of this is `ty_env`, a `HashMap<ast::TypeVar, TypeVar>`.
If we have a type scheme:

```rs
let a = TypeVar(4);
let b = TypeVar(23);
let c = TypeVar(149);
let foo = TypeScheme {
  unbound: btree_set![a, b, c],
  ty: Type::fun(Type::Var(a), Type::fun(Type::Var(b), Type::Var(c)))
};
```
Our type env will be:

```rs
// Excuse the made up `hash_map!` macro.
let ty_env = hash_map![
  a: TypeVar(2),
  b: TypeVar(1),
  c: TypeVar(0)
];
```

Remember that because DeBruijn Indices count how many `TyFun`s we skip over, `a` gets `TypeVar(2)` not `TypeVar(0)`.
This is why we reverse `scheme.unbound` when constructing `ty_env`.
Back in `lower_ty_scheme`, we use `ty_env` to lower the `type` field of our `TypeScheme`.

```rs
let lower = LowerTypes { env: ty_env };
let lower_ty = lower.lower_ty(scheme.ty);
```

`LowerTypes` is a helper struct to hold our type environment while we recurse over a type:

```rs
struct LowerTypes {
  env: HashMap<ast::TypeVar, TypeVar>,
}
```

It defines one method `lower_ty`, which is so short we don't even need to break it up:

```rs
impl LowerTypes {
  fn lower_ty(&self, ty: ast::Type) -> Type {
    match ty {
      ast::Type::Int => Type::Int,
      ast::Type::Var(v) => Type::Var(self.env[&v]),
      ast::Type::Fun(arg, ret) => {
        let arg = self.lower_ty(*arg);
        let ret = self.lower_ty(*ret);
        Type::fun(arg, ret)
      }
    }
  }
}
```

`self.env[&v]` could panic if it encounters a `v` absent from `env`.
We're okay with panicking, but if we wanted to be more graceful we'd use `get` and call `expect` to provide a nice error message.
If we ever do panic, we have a type containing a variable not listed in `scheme.unbound`.
That's a compiler bug.

Note `lower_ty` never produces a `Type::TyFun`.
`ast::Type` can't ever introduce a type variable, so it never needs to bind a type variable with `TyFun`. 
As we know, type variables are only introduced by `TypeScheme`.

Leaving us with one final task in `lower_ty_scheme`: introduce `TyFun`s for our type variables.

```rs
let bound_lower_ty = 
  (0..lower.env.len())
    .fold(lower_ty, |ty, _| Type::ty_fun(Kind::Type, ty));
```

For each type variable bound by our type scheme, we wrap `lower_ty` in a `TyFun`.
In our AST we distinguish between `ast::TypeScheme` and `ast::Type`.
As we can see, that distinction disappears in lowering.
Both `ast::TypeScheme` and `ast::Type` turn into IR `Type`.
As an example, lowering our `foo` type scheme produces IR type:

```rs
TyFun(Kind::Type, 
TyFun(Kind::Type, 
TyFun(Kind::Type, 
  Fun(
    Var(TypeVar(2), 
    Fun(TypeVar(1), TypeVar(0)))))))
```

A `TyFun` for each variable in `foo.unbound`, and our `TypeVar`'s have been converted to DeBruijn indices.
Putting it all together, our `lower_ty_scheme` is:

```rs
fn lower_ty_scheme(scheme: ast::TypeScheme) -> (Type, LowerTypes) {
  let ty_env = scheme
    .unbound
    .into_iter()
    .rev()
    .enumerate()
    .map(|(i, tyvar)| (tyvar, TypeVar(i)))
    .collect();

  let lower = LowerTypes { env: ty_env };
  let lower_ty = lower.lower_ty(scheme.ty);
  let bound_lower_ty = (0..lower.env.len()).fold(lower_ty, |ty, _| {
    Type::ty_fun(Kind::Type, ty)
  });
  (bound_lower_ty, lower)
}
```

Time to add another line to `lower`:

```rs
fn lower(ast: Ast<TypedVar>, scheme: ast::TypeScheme) -> (IR, Type) {
  let (ir_ty, types) = lower_ty_scheme(scheme);

  // New!
  let mut lower_ast = LowerAst {
    supply: VarSupply::default(),
    types,
  };
  let ir = lower_ast.lower_ast(ast);
  todo!()
}
```
We got a couple lines this time. Oh, boy!
`LowerAst` shares a purpose with `LowerTypes`:

```rs
struct LowerAst {
  supply: VarSupply,
  types: LowerTypes,
}
```

It holds state for the recursive AST functions we're going to write.
`VarSupply` exists to map AST variables into IR variables and generate new IR variables.
Its internals are uninteresting, but can be found in the [full code](TODO).
Suffice to say, it supports one method:

```rs
impl VarSupply {
  fn supply_for(&mut self, var: ast::Var) -> VarId {
    // boring...
  }
}
```

`supply_for` caches internally, so if we pass the same `ast::Var` we receive the same `VarId`.
Moving on, `LowerAst` supports one method `lower_ast`:

```rs
impl LowerAst {
  fn lower_ast(&mut self, ast: Ast<TypedVar>) -> IR {
    match ast {
      //...
    }
  }
}
```

We're well versed in this pattern by now.
Match on the `ast` and produce an `IR` term for each case.
First up is variables:

```rs
Ast::Var(TypedVar(var, ty)) => IR::Var(Var::new(
  self.supply.supply_for(var),
  self.types.lower_ty(ty),
)),
```
A `TypedVar` turns into an IR `Var`.
Thanks to all that code we wrote earlier, converting is very easy.
We convert our `ast::Var` into a `Var` using `VarSupply` and lower our `ast::Type` using `LowerTypes`.
Because `self.types` uses the same `ty_env` as in `lower_ty_scheme`, we can be confident it will lower types equivalently.

```rs
Ast::Int(i) => IR::Int(i),
```

Once again `Int` holds us down by being a consistent freebie.
`Fun` isn't much more complicated:

```rs
Ast::Fun(TypedVar(var, ty), body) => {
  let ir_ty = self.types.lower_ty(ty);
  let ir_var = self.supply.supply_for(var);
  let ir_body = self.lower_ast(*body);
  IR::fun(Var::new(ir_var, ir_ty), ir_body)
}
```

More code, but there are no surprises here.
We deconstruct our `Fun` node, lower all its parts, and recombine them into an IR `Fun`.
Last but not least, `App`:

```rs
Ast::App(fun, arg) => {
  let ir_fun = self.lower_ast(*fun);
  let ir_arg = self.lower_ast(*arg);
  IR::app(ir_fun, ir_arg)
}
```

Straightforward again, we lower our part and recombine them into an IR `App`.

It feels almost like we've done nothing at all.
Each of our cases turns an AST node into an IR node of the same name.
This is rooted in our AST being lambda calculus and our IR being System F.
Translating the lambda calculus into System F, a superset of the lambda calculus, requires little work.

With `lower_ast` complete, we return to `lower`.
One task remains.
Just as we wrapped our lowered type in `TyFun` nodes, we have to wrap our lowered `Ast` in `TyFun` nodes:

```rs
fn lower(ast: Ast<TypedVar>, scheme: ast::TypeScheme) -> (IR, Type) {
  let (ir_ty, types) = lower_ty_scheme(scheme);
  let mut lower_ast = LowerAst {
    supply: VarSupply::default(),
    types,
  };
  let ir = lower_ast.lower_ast(ast);

  // New!
  let bound_ir = 
    (0..lower_ast.types.env.len())
      .fold(ir, |ir, _| IR::ty_fun(Kind::Type, ir));
  (bound_ir, ir_ty)
}
```

The most important section of lowering resides in this innocuous bit of code.
Terms in the AST have no concept of types.
They show up purely in type schemes and types.
In our IR, by contrast, we have a term that binds types at the term level.

The consequences of representing types in our term are far-reaching.
If an optimization removes `ty_fun`s from a term, we know we're removing polymorphism.
If we see a term with no `ty_fun`, we know that term can't be generic.
Determining where polymorphism resides is instrumental to [monomorphization](https://en.wikipedia.org/wiki/Monomorphization) and emitting machine code.
Two passes we'll visit before completing our simple compiler.

Our `lower` function now translates our implicitly typed AST into our explicitly typed IR.
Lowering may look ceremonial here, but rest assured it will get more interesting as we add more features from our [fancier type checkers](/posts/row-types).
Even without fancy features, we've taken another step towards completing our compiler.
