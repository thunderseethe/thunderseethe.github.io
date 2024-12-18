+++
title = "Part 5: Lowering our Base Typed AST"
date = "2024-12-08T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Lowering"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Lowering", "Elaboration", "IR", "System F", "Debruijn Indices"]
description = "Lowering our typed AST into an IR"
+++


## Goals of Lowering

### Why Lower at all?

### Intro IR/IR Type

* IR not that different from AST 

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

* Var is similar to AST's `TypedVar`:

```rs
struct Var {
  id: VarId,
  ty: Type,
}
```

* We no longer have untyped variables, so we don't need to distinguish them.
* `VarId` is our familiar integer counter:

```rs
struct VarId(u32);
```

* It comes with all the usual guarantees.
* All variables in a term are unique.
* `Type` is our IR type.
* It looks quite similar to our AST type:

```rs
#[derive(Debug, PartialEq, Eq, Clone)]
enum Type {
  Int,
  Var(TypeVar),
  Fun(Box<Self>, Box<Self>),
  Forall(Kind, Box<Self>),
}
```

* New IR nodes: TyFun and TyApp.
* As the name implies, these are like `Fun` and `App`.

* Type also not that different (yet)
* New IR type: Forall
* Explain IR handles generics more explicitly via Forall, TyFun, and TyApp

### Typed IR

* Our IR has explicit types, rather than being typed.
* Makes it easier to catch bugs in the compiler implementation.
* In the process of writing this lowering code having types caught bugs in my lowering logic, and we're not doing any complex yet.

* Types don't have to be checked. 
* We already made sure they were right during type inference.
* Type checking acts more like a linter.
* We can call it after transforming the IR to make sure everything still makes sense.

* Because we've already performed unification and solved all our types, we can reconstruct our IR types quickly.
* 

```rs
fn type_of(&self) -> Type {
  match self {
    IR::Var(v) => v.ty.clone(),
    IR::Int(_) => Type::Int,
    IR::Fun(arg, body) => Type::fun(arg.ty.clone(), body.type_of()),
    IR::App(fun, arg) => {
      let Type::Fun(fun_arg_ty, ret_ty) = fun.type_of() else {
        panic!("ICE: IR used non-function type as a function")
      };
      let arg_ty = arg.type_of();
      if arg_ty != *fun_arg_ty {
        panic!("ICE: Function applied to wrong argument type");
      }
      *ret_ty
    }
    IR::TyFun(kind, body) => Type::forall(*kind, body.type_of()),
    IR::TyApp(body, ty) => {
      let Type::Forall(_, body_ty) = body.type_of() else {
        panic!("ICE: Type applied to a non-forall IR term");
      };

      body_ty.subst(ty.clone())
    }
  }
}
```

## Bird's Eye View of Lowering

```rs
fn lower(
  ast: Ast<TypedVar>, 
  scheme: ast::TypeScheme
) -> (IR, Type) {
  todo!()
}
```
* Turn `Ast<TypedVar>` into an `IR`
* Turn `ast::TypeScheme` into a `Type`
* Start with `TypeScheme`
  * Why?

```rs
fn lower(
  ast: Ast<TypedVar>, 
  scheme: ast::TypeScheme
) -> (IR, Type) {
  let (ir_ty, types) = lower_ty_scheme(scheme);
  let mut lower_ast = LowerAst {
    supply: VarSupply::default(),
    types,
  };
  let ir = lower_ast.lower_ast(ast);
  let ir = (0..lower_ast.types.env.len()).fold(ir, |ir, _| IR::ty_fun(Kind::Type, ir));
  (ir, ir_ty)
}
```

## Lowering Type Schemes

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
  let lower_ty = (0..lower.env.len()).fold(lower.lower_ty(scheme.ty), |ty, _| {
    Type::forall(Kind::Type, ty)
  });
  (lower_ty, lower)
}
```
  * What is `ty_env`?
  * What is `LowerTypes`?
  * Explain `Type` before this
