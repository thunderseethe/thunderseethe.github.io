+++
title = "Lowering[1].Rows[1]: The Types of Lowered Rows"
date = "2025-02-18T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Lowering"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Lowering", "Elaboration", "IR", "Evidence Passing", "Witness", "Row Types", "Abstracting Extensible Datatypes"]
description = "Lowering row evidence from our type scheme"
+++

[Last time](/posts/lowering-rows-intro) we learned how we're going to lower row types and made some row addendums for `IR` and `Type`.
Today, we'll update `lower_ty_scheme`, using those addendums, to generate types for `Evidence`.

{{< accessory title="Quick Refresher" >}}
Here's a quick rundown of the stuff we'll need from [types/rows](/posts/row-types).

First up is our `Type`:

```rs
enum Type {
  Int,
  Var(TypeVar),
  Fun(Box<Self>, Box<Self>),
  Prod(Row),
  Sum(Row),
  Label(Label, Box<Self>),
}
```
This is our base `Type` plus `Prod`, `Sum`, and `Label`.

  * `Prod` wraps a row making it a product type.
  * `Sum` wraps a row making it a sum type.
  * `Label` is how we create a singleton row from a type.

`Row`, our star of the show, is defined as:

```rs
enum Row {
  Open(RowVar),
  Closed(ClosedRow),
}
```

Rows can either be open or closed.
Open rows represent unsolved rows, like variable type `Type::Var`.
Recall that `RowVar` is just like `TypeVar` but is solved to a `Row` (instead of a `Type`).

A `ClosedRow` represents a solved row using a mapping from fields to types:

```rs
struct ClosedRow {
  fields: Vec<Label>,
  values: Vec<Type>,
}
```

We can think of a `ClosedRow` as a `HashMap<Label, Type>`.
We don't store it that way so that we can maintain a specific order of `fields`, and to easily compare `fields` without `values`.
As we'll see today, storing them separately also makes it easy to erase labels.

Alongside our `Row` enhanced `Type`, `TypeScheme` also receives some enhancements:

```rs
struct TypeScheme {
  unbound_rows: BTreeSet<RowVar>,
  unbound_tys: BTreeSet<TypeVar>,
  evidence: Vec<Evidence>,
  ty: Type,
}
```

Our `Row` enhanced type scheme tracks unsolved row variables in `unbound_rows`.
It also contains our old friends from base `TypeScheme`: `unbound_tys` and `ty`.
`unbound_tys` tracks unsolved type variables, like `unbound_rows`.
`ty` is our `Type`; the core of our scheme.

`evidence`, our other addition, stores unsolved row combinations.
With the introduction of row constraints, we introduce the possibility to fail solving row constraints.
When this occurs, we turn them into evidence and stash them in the type scheme, just like row and type variables.
Accordingly, `Evidence` is:

```rs
enum Evidence {
  RowEquation { 
    left: Row, 
    right: Row, 
    goal: Row 
  },
}
```

That's everything we need to lower. 
Back to your regularly scheduled programming.

{{< /accessory >}}

## A short visit from `lower`

All our books are kept, in order even, we'll crack open `lower` briefly to see how `lower_ty_scheme` is used now:

```rs
fn lower(ast: Ast<TypedVar>, scheme: ast::TypeScheme) -> (IR, Type) {
  let lowered_scheme = lower_ty_scheme(scheme);

  todo!("We'll cover the rest later.");
}
```

## What's a `lowered_scheme`

That doesn't seem too bad.
`lower_ty_scheme` used to return a tuple, now it returns whatever `lowered_scheme` is:

```rs
fn lower_ty_scheme(scheme: ast::TypeScheme) -> LoweredTyScheme {
  // You already know this isn't gonna look the same
}
```

`lowered_scheme` is a `LoweredTyScheme` that makes sense.
But I don't know what a `LoweredTyScheme` is:

```rs
struct LoweredTyScheme {
  scheme: Type,
  lower_types: LowerTypes,
  kinds: Vec<Kind>,
  ev_to_ty: BTreeMap<ast::Evidence, Type>,
}
```

Here we go. 
Our first clue of the changes we need to make.
Previously, we only returned a `(Type, LowerTypes)`.
We have two new return values: `ev_to_ty` and `kinds`.
And let me just say, it was smart of you to name them instead of just tossing them in a bigger tuple.
That's what I would've done.

`ev_to_ty` tracks the `Type` we generate for each evidence in our scheme.
This will be the function parameter type we add per unsolved evidence.
`kinds` tracks the `Kind` of each `TypeVar` produced by lowering our `ast::TypeScheme`.
Technically we could have done this last time, but it was pointless when we only had one `Kind`.
With two `Kind`s, the kind of each `TypeVar` actually matters.

## Updates to `lower_ty_scheme`

Constructing our `ty_env` remains steadfast as the first thing we do in `lower_ty_scheme`:

```rs
fn lower_ty_scheme(scheme: ast::TypeScheme) -> LoweredTyScheme {
  let mut kinds = todo!();
  let ty_env = todo!("Use kinds to do this");

  todo!();
}
```

`kinds` is a `Vec<Kind>` that allows us to track our Kinds more carefully.
Because our `TypeScheme` tells us how many variables to expect, we can pre-allocate a `Kind` for each variable:

```rs
let mut kinds = vec![Kind::Type; scheme.unbound_tys.len() + scheme.unbound_rows.len()];
```

`kinds` defaults to `Kind::Type`, and we update each entry as we build our `ty_env`.
`ty_env` is constructed the same way as last time, but now we handle both row and type variables:

```rs
let ty_env = scheme
  .unbound_tys
  .into_iter()
  .map(AstTypeVar::Ty)
  .chain(scheme.unbound_rows.into_iter().map(AstTypeVar::Row))
  .rev()
  .enumerate()
  .map(|(i, tyvar)| {
    kinds[i] = tyvar.kind();
    (tyvar, TypeVar(i))
  })
  .collect();
```

We have to pick an order for our type variables and row variables.
Either row then type or type then row.
The ordering we pick isn't important, but we must maintain the same order here and in `lower`.
The order we'll choose is types and then rows.

`ty_env` needs to hold two types of variables now: `ast::TypeVar` and `ast::RowVar`.
`AstTypeVar` is a simple wrapper enum to allow this (like `TyApp`):

```rs
enum AstTypeVar {
  Ty(ast::TypeVar),
  Row(ast::RowVar),
}
```
It provides a single helper method `kind()`:

```rs
impl AstTypeVar {
  fn kind(&self) -> Kind {
    match self {
      AstTypeVar::Ty(_) => Kind::Type,
      AstTypeVar::Row(_) => Kind::Row,
    }
  }
}
```

We found this method used in the construction of `ty_env`.
`kinds[i]` is set to the kind of each `AstTypeVar`.
At the end of this, we have our `ty_env` and our `kinds` vector.
Our next step, lowering our type, is unchanged on the surface:

```rs
let lower = LowerTypes { env: ty_env };

let lower_ty = lower.lower_ty(scheme.ty);
```

### Diving into `lower_ty`

If we dive into `lower_ty`, we encounter some new changes.
Before the new, let's remember the basic structure of `lower_ty`:

```rs
fn lower_ty(&self, ty: ast::Type) -> Type {
  match ty {
    // ...some cases
  }
}
```

We add a case to lower each of our `ast` row types.
Let's start with `Label` to ease us in:

```rs
ast::Type::Label(_, ty) => self.lower_ty(*ty),
```

Easy enough. 
We erase the label and then lower `ty`.
Next, we tackle `Prod` and `Sum` as a duo:

```rs
ast::Type::Prod(row) => Type::prod(self.lower_row_ty(row)),
ast::Type::Sum(row) => Type::sum(self.lower_row_ty(row)),
```

They both call out to `lower_row_ty`. 
`Prod` wraps it in an IR `Prod` type.
`Sum` wraps it in an IR `Sum` type.
Fair enough, but --

### What's `lower_row_ty`?

`lower_row_ty`, as we can hopefully glean from the name, lowers an `ast::Row` into `Row`:

```rs
fn lower_row_ty(
  &self, 
  row: ast::Row
) -> Row {
  match row {
    ast::Row::Open(var) => Row::Open(self.env[&AstTypeVar::Row(var)]),
    ast::Row::Closed(closed_row) => Row::Closed(self.lower_closed_row_ty(closed_row)),
  }
}
```
If we have:

  * An `Open` row, we look up it's index and turn it into a `TypeVar`.
  * A `Closed` row, we call out to another new helper `lower_closed_row_ty`.

We still haven't figured everything out though.
We have to go deeper.

### One Level Deeper into `lower_closed_row_ty`

Context clues tell us that `lower_closed_row_ty` turns a `ClosedRow` into its IR compatriot.
Peering into its innards reveals no surprises:

```rs
fn lower_closed_row_ty(
  &self, 
  closed_row: ast::ClosedRow
) -> Vec<Type> {
  closed_row
    .values
    .into_iter()
    .map(|ty| self.lower_ty(ty))
    .collect()
}
```

We erase the `fields` of our `ClosedRow`, implicitly, and lower each type in `values` to produce a `Vec` of IR types.
It took some diving, but that's all are changes in `lower_ty`, and it's row related descendants.

We'll pass through `lower_ty_scheme`, briefly, on our way to `lower_ev_ty`, do not collect $200:

```rs
fn lower_ty_scheme(scheme: ast::TypeScheme) -> LoweredTyScheme {
  // ...picking up where we left off.
  let lower_ty = lower.lower_ty(scheme.ty);

  let mut ev_to_ty = BTreeMap::new();
  let ev_tys = scheme
    .evidence
    .into_iter()
    .map(|ev| {
      let ty = lower.lower_ev_ty(ev.clone());
      ev_to_ty.insert(ev, ty.clone());
      ty
    })
    .collect::<Vec<_>>();
  
  todo!()
}
```

Each evidence of our scheme is lowered into a type using `lower_ev_ty`.
These types serve two roles:

  * We map our evidence to this type in `ev_to_ty`.
  * We collect this type into `ev_tys`, a `Vec<Type>`.

### `lower_ev_ty` Avenue

Finally, we reach the main thrust of our work today.
As we learned last time, each evidence becomes associated with an `IR` term in lowering.
`lower_ev_ty` doesn't generate this term itself, but the type of this term.

Unsolved evidence becomes a function parameter of our lowered `IR` term.
We don't have to provide a value for the function parameter (that's the caller's job), but we do have to provide our parameter a type.
`lower_ev_ty`'s signature is modest:

```rs
fn lower_ev_ty(
  &self, 
  evidence: ast::Evidence
) -> Type {
  todo!()
}
```

The final returned type mirrors the shape of evidence's term:

```rs
fn lower_ev_ty(
  &self, 
  evidence: ast::Evidence
) -> Type {
  todo!("We have some work before we return that type");

  Type::prod(Row::Closed(vec![
    concat,
    branch,
    Type::prod(Row::Closed(vec![
      prj_left, 
      inj_left
    ])),
    Type::prod(Row::Closed(vec![
      prj_right, 
      inj_right
    ])),
  ]))
}
```

Before we can generate that `Type`, we have to assemble a super team of `Type`s to help us:

```rs
let ast::Evidence::RowEquation { left, right, goal } = evidence;

let left = self.lower_row_ty(left);
let (left_prod, left_sum) = (
  Type::prod(left.clone()), 
  Type::sum(left));

let right = self.lower_row_ty(right);
let (right_prod, right_sum) = (
  Type::prod(right.clone()), 
  Type::sum(right));

let goal = self.lower_row_ty(goal);
let (goal_prod, goal_sum) = (
  Type::prod(goal.clone()), 
  Type::sum(goal));
```

We lower each row in `evidence`.
Each of those rows is then wrapped in both a `Prod` and `Sum` type.
Our lowered rows are used to type each component of our evidence, starting with `concat`:

```rs
let concat = 
  Type::funs(
    [ 
      left_prod.clone(), 
      right_prod.clone()
    ], 
    goal_prod.clone());
```

Pretty straightforward. `concat` takes in a `left_prod` and a `right_prod` returning a `goal_prod`.
Probably by concatenating `left_prod` and `right_prod`, but that's none of our business right now.
`branch` is more involved:

```rs
let branch = {
  let a = TypeVar(0);
  Type::ty_fun(
    Kind::Type,
    Type::funs(
      [
        Type::fun(left_sum.clone().shifted(), Type::Var(a)),
        Type::fun(right_sum.clone().shifted(), Type::Var(a)),
        goal_sum.clone().shifted(),
      ],
      Type::Var(a),
    ))
};
```

`branch` is complicated because it takes in two functions, and returns a third function.
Not only does this complicate the types of our inputs, but we're burdened with ensuring all three of our functions return the same type.
We don't know our return type ahead of time. 
We're forced to introduce a type variable to ensure all our functions return the same type.

`ty_fun` usage like this is never before seen. 
All our previous usages were at the top level of a `Type`, thanks to `TypeScheme` coalescing all our type variables.
When we have a `ty_fun` inside other `Type`s, like in `branch`, we must take some precautions.
This is because our types are using [Debruijn Indices](https://en.wikipedia.org/wiki/De_Bruijn_index).

With Debruijn Indices, when we place a type inside a `ty_fun` we have to adjust the indices within.
Our indices now have to skip over one extra `ty_fun` node to reach their binding `ty_fun`.
`shifted()` is responsible for this adjustment of type variables. 
It increases each type variable by one.

We shift each type embedded within `branch`'s type to account for the `ty_fun`.
With `branch` out of the way, `prj_left` and `inj_left` are much simpler:

```rs
let prj_left = 
  Type::fun(goal_prod.clone(), left_prod);
let inj_left = 
  Type::fun(left_sum, goal_sum.clone());
```

`prj_left` takes in our `goal_prod` and projects it into a `left_prod`.
`inj_left` goes the opposite direction taking a `left_sum` and injecting it into the larger `goal_sum`.
Their compliments, `prj_right` and `inj_right`, exhibit a symmetry:

```rs
let prj_right = 
  Type::fun(goal_prod, right_prod);
let inj_right = 
  Type::fun(right_sum, goal_sum);
```

Every member of our super team is present, we can assemble our final evidence type:

```rs
fn lower_ev_ty(
  &self, 
  evidence: ast::Evidence
) -> Type {
  // ...We did some work before we return that type
  Type::prod(Row::Closed(vec![
    concat,
    branch,
    Type::prod(Row::Closed(vec![
      prj_left, 
      inj_left
    ])),
    Type::prod(Row::Closed(vec![
      prj_right, 
      inj_right
    ])),
  ]))
}
```
That maps directly onto our example layout.
Evidence is a big tuple with a function for each operation we want to perform on a row.
We'll add some tests, but surely it wouldn't map that directly if it was wrong.

### Returning to `lower_ty_scheme`

Returning to `lower_ty_scheme`, we'll use our evidence types to add parameters to our overall term type:

```rs
fn lower_ty_scheme(scheme: ast::TypeScheme) -> LoweredTyScheme {
  // We saw this earlier
  let ev_tys = ...;
  
  // But now we use it to add params to our lower_ty.
  let evident_lower_ty = Type::funs(ev_tys, lower_ty);
  todo!()
}
```

Here is where we put the passing in Evidence Passing.
In the AST evidence is passed implicitly, but in lowering we're more explicit, turning each unsolved evidence into a function parameter.
We need to pay attention to the order of our function parameter types.
They have to be in the same order as `scheme.evidence`, so callers pass evidence terms to the right types.

Once our type has all its evidence parameters, it needs `TyFun`s for its variables:

```rs
let bound_lower_ty = kinds
  .iter()
  .fold(evident_lower_ty, |ty, kind| Type::ty_fun(*kind, ty));
```

This hasn't changed a lot since last time, but `Kind`s matter now.
Accordingly, we use our `kinds` vector to tell us what `Kind` each `TyFun` should have.
Finally, we package up everything we need in `lower` and return it:

```rs
LoweredTyScheme {
  scheme: bound_lower_ty,
  lower_types: lower,
  kinds,
  ev_to_ty
}
```

### Returning to `lower`

Once we're in `lower`, we immediately make use of our `ev_to_ty` field:

```rs
fn lower(ast: Ast<TypedVar>, scheme: ast::TypeScheme) -> (IR, Type) {
  let lowered_scheme = lower_ty_scheme(scheme);
  let mut supply = VarSupply::default();
  let mut params = vec![];
  let ev_to_var = lowered_scheme.ev_to_ty
    .into_iter()
    .map(|(ev, ty)| {
      let param = supply.supply();
      let var = Var::new(param, ty);
      params.push(var.clone());
      (ev, var)
    })
    .collect();
  
  todo!()
}
```

Each evidence from our scheme gets a `Var`.
Unlike other `Var`s, these aren't tied to any instance of `ast::Var`.
They're unique to the `IR`.
This is noteworthy because it means we need a new function on `VarSupply`: `supply`.

`supply` generates a new `Var` without requiring an `ast::Var`, unlike `supply_for`.
Its implementation is still uninteresting, but can be found in the [full code](https://github.com/thunderseethe/making-a-language/tree/main/lowering/rows)

Our generated `Var`s are collected into a vector `params` and stored in map from `Evidence` to the `Var`.
`params` is how we're going to add a `Fun` node for each evidence to our final `IR` term.
`ev_to_var` tells us the variable to reference for an evidence while lowering the `AST`.
We use `ev_to_var` to construct our new and improved `LowerAst`:

```rs
let mut lower_ast = LowerAst {
  supply,
  types: lowered_scheme.lower_types,
  ev_to_var,
  solved: vec![],
};
let ir = lower_ast.lower_ast(ast);
```

Just like `LoweredTyScheme`, it's got some new fields as well.
We're familiar with `ev_to_var`.
We'll use `solved` for solved evidence during `lower_ast`, more on that [next time](/posts/lowering-rows-ast).
