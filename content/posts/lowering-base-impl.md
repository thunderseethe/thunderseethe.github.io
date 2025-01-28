+++
title = "Part 5b: Lowering our AST, an Implementation"
date = "2025-02-03T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Lowering"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Lowering", "Elaboration", "IR", "System F", "DeBruijn Index"]
description = "Implementation of our lowering function"
draft = true
+++

Armed with the knowledge of `IR` and `Type` from [last time](/posts/lowering-base-ir), we can finally write some code:

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
That's great progress (especially compared to last post)!

{{< accessory title="Refresher on Base AST" >}}
It's been awhile since we worked with our base typechecker.
I don't really remember what all was in there, so let's go over it to get back up to speed.

Recall that our base AST is:

```rs
enum Ast<V> {
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

Where our `V` generic is the type of variables. It starts out as `Var`:

```rs
struct Var(usize);
```

By the end of type checking, we've graduated it to `TypedVar`:

```rs
struct TypedVar(Var, Type);
```

Naturally, our next question is "What's `Type`?":

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

Alongside the `Ast<TypedVar>`, `lower` also takes the `TypeScheme` produced by typechecking.
The `TypeScheme` binds all the unsolved type variables that appear in our typed AST:

```rs
struct TypeScheme {
  unbound: BTreeSet<TypeVar>,
  ty: Type,
}
```

With that we know everything we'll see from the typechecker, back to lowering.

{{< /accessory >}}

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
Each of these AST `TypeVar`s becomes an IR `TypeVar` bound by a `TyFun` node which means we have to convert from AST `TypeVar` to DeBruijn Indices.

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

It defines one method `lower_ty` which is so short we don't even need to break it up:

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
In our AST, we distinguish between `ast::TypeScheme` and `ast::Type`.
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

A `TyFun` for each variable in `foo.unbound`, and our `TypeVar`'s have been converted to DeBruijn Indices.
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

## Lowering Ast

`LowerAst` shares a purpose with `LowerTypes`:

```rs
struct LowerAst {
  supply: VarSupply,
  types: LowerTypes,
}
```

It holds state for the recursive AST functions we're going to write.
`VarSupply` exists to map AST variables into IR variables and generate new IR variables.
Its internals are uninteresting but can be found in the [full code](https://github.com/thunderseethe/making-a-language/tree/main/lowering/base).
Suffice to say, it supports one method:

```rs
impl VarSupply {
  fn supply_for(&mut self, var: ast::Var) -> VarId {
    // boring...
  }
}
```

`supply_for` caches internally. 
If we pass the same `ast::Var`, we receive the same `VarId`.
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
Thanks to all that code we wrote earlier converting is very easy.
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

## Lowering Totality

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
