+++
title = "Part 3: Row Types"
date = "2023-08-12T00:00:00Z"
draft = true
author = "thunderseethe"
tags = ["Programming Languages", "Type Inference"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Type Inference", "Bidirectional Typechecking", "Unification", "Constraint Solving", "E-Unification", "Row Types", "Structural Typing"]
description = "Row, row, row your types"
+++

[Last season](/posts/unification), we assembled a simple type inference engine end to end.
Powerful enough to check both functions _and_ integers, without any annotations at all.
Perfect as it is, our type inference (and language) lacks one teensy feature.
It doesn't have any data types.
We'll rectify that today by adding support for row types.

A row is a mapping from a field name to a type, for example:
```
( x : Int, y : Int )
```
This is a row with two fields `x` and `y` both mapped to type `Int`.
Rows are used in two ways.
We can use this row as a product type, where it will be a struct with two members `x` and `y`.
Or we can use this row as a sum type, where it will be an enum with two variants `x` and `y`.

Row types behave structurally, in contrast to more traditional data types, such as Algebraic Data Types (ADTs) or Classes, that behave nominally.
Each data type in a nominal system introduces a new name and names equate with itself and no other name.
Our structural row types have a more generous definition of equality.
Any two types are equal if they have the same fields and map them to the same types.
Stated another way, we determine two types are equal by looking at their structure, not their name.

A ton of different implementations have been devised for row systems over the decades they've been watching on the sidelines.
Our implementation is founded on the ideas detailed in [Abstracting Extensible Data Types](https://dl.acm.org/doi/10.1145/3290325).
I like this implementation because it uses a trait-like mechanism to represent row combinations.
This allows for representing row operations very efficiently, they're all a special case of row combination.

As long as two rows don't have any common fields,
we can combine them to produce a new row:
```
( x : Int, y : Int ) + ( f : String ) = ( f : String, x : Int, y : Int )
```
Even better, we can introduce row variables and combine them with other rows:
```
ρ + ( x : Int, y : Int ) = α
```
The utility of this is not immediately apparent.
We'll get into the details, for now suffice to say combining row variables like this lets us work with rows generically.
We can relate row variables to each other through their combinations to describe the type of functions that work on generic rows.
The same way we use traits and type variables to describe type of functions that work on generic types.

This allows our data to describe our problem domain precisely.
In a nominal system we would rely on runtime assertions to ensure we only use a valid subset of fields.
But with row types we can slice up our row, so the valid fields are the only ones available.

Compared to ADTs and Classes,
row types are a rather underexplored way to handle data types in a language.
They can only be found in the corners of OCaml or languages that retrofit static types onto JavaScript (PureScript, Elm, etc.).
There is a sprawling undergrowth of research about rows in academia, but very little of it has sprouted into the mainstream.
I think this is a shame, and it just so happens I'm designing a language, so I'm in a prime position to do something about it.

Now, that we're ideologically motivated, let's look at how we'll extend our type inference to infer rows.
Our steps are pleasantly similar to our initial implementation:

  1. Create AST Nodes for row constructors
  2. Generate row type constraints from our nodes
  3. Unify row constraints to solve row types

Before we start on our implementation, a quick refresher on what we've already built.

{{< accessory title="Refresher" >}}

Cover what we've done up till now:

  * AST has 4 nodes: Int, Var, Func, App. It is the lambda calculus plus int literals.
  * Types are either Int, Function, or Variable.
  * Type checking is split into two phases constraint generation and constraint solving.
    * Constraint generation is done with a bidirectional type system
    * Constraint solving is done with unification using union-find
  * Hindley-Milner type system with no frills

{{< /accessory >}}

# AST (WIP Title)
Six new nodes are required to support rows. 
That might seem like a lot (it increases our AST cases 250%!) 
However, remember these nodes are going to support all our data needs forever.
When you say it like that six nodes is a steal.

Our new nodes come in 3 pairs. 
Each pair comprises a constructor and a destructor.
One pair deals with product types, one with sum types, and our final one converts normal terms into and out of row terms.
Our product pair is:

```rs
enum Direction {
  Left,
  Right,
}

enum Ast<V> {
  // ...
  Concat(Box<Self>, Box<Self>),
  Project(Direction, Box<Self>),
```
`Concat` combines two product types into a bigger product type.
`Project` maps a product into a smaller product made from a subset of its fields.
Our next pair is sum types:
```rs
  Inject(Direction, Box<Self>),
  Branch(Box<Self>, Box<Self>),
```
`Inject` maps a small sum into a bigger sum containing the smaller sum's cases
`Branch` combines two destructors for sum types into a big destructor for the combination of the two sum types
This is our row-ified version of a match statement.

Our new product and sum nodes don't include a way to make a new row, 
only ways to build bigger and smaller rows out of existing rows.
Haskell might be okay with treating an infinite tree of `Concat` nodes as data, but we're stricter than that.
Fortunately we still have 2 more nodes (that wouldn't you know it solve that exact problem):

```rs
  Label(Label, Box<Self>),
  Unlabel(Box<Self>, Label),
}
``` 
`Label` turns a term into a singleton row with label as its field.
`Unlabel` turns a singleton row back into a normal term by removing the field specified by its label.
`Unlabel` looks like it could work on any row (not just a singleton).
We're going to require it's used on a singleton row.
It makes type inference simpler.
If we want to unlabel any row we can first project it down to a singleton row and then unlabel it.

This completes our suite of new nodes.
We can lift any term into a singleton row and then concat or inject it into a larger row.
This is all we need in theory, in practice having to build every row as a series of `Concats` is very verbose.
A real implementation would support constructing rows with multiple fields alongside singleton rows.

How do we actually use these nodes to do the all the stuff we love to do with our data?
Let's look at an example that combines two rows and then gets one of their members:

```rs
Ast::unlabel(
  Ast::project(
    Left, 
    Ast::concat(
      Ast::label("x", Ast::Int(4)),
      Ast::label("y", Ast::Int(3)),
    )
  ),
  "x"
)
```
Label is used twice here to create two singleton rows `x : Int` and `y : Int`.
These are combined to form one product row `{ x : Int, y : Int }`.
That product row is projected out into the product row `{ x : Int }`.
Which is finally unlabeled to give us back our int `4`.

# Row Types

Same as our original type inference, we can use our new AST nodes to inform what new types we need.
Each new constructor produces a value that requires a type:

  * `Concat` - `Product` types
  * `Inject` - `Sum` types
  * `Label` - a type labeled with a field, we'll call it `Label`

Notice that none of our new types are a row type.
In fact, I've been lying to you this entire time.

There are **no** row types, there _never_ were.
Rows can't be types themselves. 
They only become a type once you wrap them as a product or a sum.
We see this reflected in our new `Type` nodes:
```rs
enum Type {
  // ... our previous cases
  /// A product type
  Prod(Row),
  /// A sum type
  Sum(Row),
  /// Type of singleton rows
  Label(Label, Box<Self>),
}
```
Okay so if rows aren't types, what are they?
```rs
struct RowVar(u32);

enum Row {
  Open(RowVar),
  Closed(ClosedRow),
}
```
That's not super helpful.
A row is either open and a row variable, or closed and whatever a `ClosedRow` is.
We can see `RowVar` is just an int, that won't help.
Let's see if `ClosedRow` can help us:

```rs
struct ClosedRow {
  // Sorted lexographically
  fields: Vec<Label>,
  // One type for each field in fields
  values: Vec<Type>,
}
```
That's starting to look familiar. 
A closed row is a map from fields to types, stored as sorted vectors.

We might expect a closed row would be a HashMap (or at least a BTreeMap).
During unification, it's helpful to access just a row's fields for fast equality checks.
We'll see the details later, but rest assured there's a method to the madness.

The last piece of type machinery we need is our new row constraint.
Our new constraint is a row combination:
```rs
enum Constraint {
  // ... our previous cases
  RowCombine(RowCombination),
}
struct RowCombination {
  left: Row,
  right: Row,
  goal: Row,
}
```
It is not always valid thing to combine rows, if `left` and `right` have an overlapping field we can't combine them.
This is one of the things we'll have to check to prove our row constraint is true.

That's it though! We only need one new constraint to support rows.
That seems too good to be true, we needed 6 new AST nodes and 3 new types for rows.
Let's look at how we use our row constraint to type our new AST nodes:

| Node    |  Type |
| ------- |  ---- |
| Concat  | `(l + r = g) => {l} -> {r} -> {g}` |
| Project | `(_ + r = g) => {g} -> {r}` |
| Inject  | `(_ + r = g) => <r> -> <g>` |
| Branch  | `(l + r = g) => (<l> -> a) -> (<r> -> a) -> (<g> -> a)` |

Wow, it really typed them all.
I was putting up a strong front as the author, but I was just as skeptical as you were.
Those [Abstract Extensible Data Types](https://dl.acm.org/doi/10.1145/3290325) are really onto something.

Okay so we're convinced this one constraint is all we need. 
Let's look at how we're going to generate it.

# Row Constraint Generation
Before we get to the heart of generation, we have to do some boilerplate in our `TypeInference` struct:
```rs
struct TypeInference {
  // ... previous fields
  row_unification_table: InPlaceUnificationTable<RowVar>,
  partial_row_combs: BTreeSet<RowCombination>,
}
// We'll add some helper methods while we're here
impl TypeInference {
  // ... earlier stuff
  /// Create a unique row variable
  fn fresh_row_var(&mut self) -> RowVar {
      self.row_unification_table.new_key(None)
  }

  /// Create a row equation with fresh row variables
  fn fresh_row_combination(&mut self) -> RowCombination {
      RowCombination {
          left: Row::Open(self.fresh_row_var()),
          right: Row::Open(self.fresh_row_var()),
          goal: Row::Open(self.fresh_row_var()),
      }
  }
  // ...later stuff
}
```

We have a new kind of variable for rows, so we have a new unification table.
Our row variables are solved to `ClosedRow`s.
Alongside this we have a set of partial row combinations, we'll need that later during unification.
These are row combinations we don't know enough about to solve, 
and will solve as we learn more row information during unification.

## Infer
Okay enough bureaucracy let's look at our `infer` cases for rows, starting with our `Label` case:

### Labels
```rs
Ast::Label(label, value) => {
  let (out, value_ty) = 
      self.infer(env, *value);
  (
    out.with_typed_ast(|ast| 
      Ast::label(
        label.clone(), 
        ast
      )
    ),
    Type::label(
      label, 
      value_ty
    ),
  )
}
```

Our label case is straightforward.
We infer a type for our value, and wrap whatever we infer as a new `Label` type using our provided `label`.
Easy, on to the `Unlabel` case:
```rs
Ast::Unlabel(value, label) => {
  let value_var = 
    self.fresh_ty_var();
  let expected_ty = 
    Type::label(label, Type::Var(value_var));
  (
    self.check(env, *value, expected_ty), 
    Type::Var(value_var)
  )
}
```
Our unlabel case is more interesting.
We get to make use of our check mode to exploit the extra info `Unlabel` provides.
We know the value of an `Unlabel` has to be a `Label` type, and that type has to have field `label`.
So we construct a `Label` with a fresh type variable and check our `value` against it.

### Products
We're getting the hang of it. 
`Concat` will make use of our new row combination constraint:
```rs
Ast::Concat(left, right) => {
  let row_comb = 
    self.fresh_row_combination();

  // Concat combines two smaller rows into a larger row.
  // To check this we check that our inputs have the types of our smaller rows left
  // and right.
  let left_out = 
    self.check(env.clone(), *left, Type::Prod(row_comb.left.clone()));
  let right_out = 
    self.check(env, *right, Type::Prod(row_comb.right.clone()));

  // If they do, then our output type is our big row goal
  let out_ty = 
    Type::Prod(row_comb.goal.clone());
  let mut constraints =
    left_out.constraints;
  constraints.extend(right_out.constraints);
  // Add a new constraint for our row equation to solve concat
  constraints.push(Constraint::RowCombine(row_comb));

  (
    InferOut {
      constraints,
      typed_ast: Ast::concat(left_out.typed_ast, right_out.typed_ast),
    },
    out_ty,
  )
}
```
We start by creating a fresh row combination.
The goal of this row combination will be our inferred product type.
Our `left` and `right` nodes have to be product types.
We exploit this knowledge to check them against Product types made from row combination's `left` and `right` rows.
A similar approach works for `Project`:
```rs
Ast::Project(dir, goal) => {
  let row_comb = self.fresh_row_combination();
  // Based on the direction of our projection,
  // our output row is either left or right
  let sub_row = match dir {
    Direction::Left => row_comb.left.clone(),
    Direction::Right => row_comb.right.clone(),
  };
  // Project transforms a row into a subset of its fields, so we check our goal ast
  // node against our goal row (not our sub_row)
  let mut out = self.check(env, *goal, Type::Prod(row_comb.goal.clone()));
  // Add our row equation constraint to solve our projection
  out.constraints.push(Constraint::RowCombine(row_comb));
  (
    out.with_typed_ast(|ast| Ast::project(dir, ast)),
    // Our sub row is the output type of the projection
    Type::Prod(sub_row),
  )
}
```
One notable difference, the goal of our row combination is our input type.
Our output type is either the `left` or `right` of our row combination, based on direction.
Direction solely exists to allow us to toggle whether `left` or `right` is used.
This will come in handy for our future plans around rows. 
But has little bearing on our current row usage, so we'll skim past it.

Inferring sum types is very similar to inferring product types (the magic of symmetry). 

{{% accessory title="Sums" %}}
Thanks for stopping by!

```rs
Ast::Branch(left, right) => {
  let row_comb = 
    self.fresh_row_combination();
  let ret_ty = 
    self.fresh_ty_var();

  // Branch expects it's two inputs to be handling functions
  // with type: <sum> -> a
  // So we check that our left and right AST both have handler function types that
  // agree on return type
  let left_out = self.check(
    env.clone(),
    *left,
    Type::fun(Type::Sum(row_comb.left.clone()), Type::Var(ret_ty)),
  );
  let right_out = self.check(
    env,
    *right,
    Type::fun(Type::Sum(row_comb.right.clone()), Type::Var(ret_ty)),
  );

  // If they do the overall type of our Branch node is a function from our goal row
  // sum type to our return type
  let out_ty = Type::fun(Type::Sum(row_comb.goal.clone()), Type::Var(ret_ty));
  // Collect all our constraints for our final output
  let mut constraints = left_out.constraints;
  constraints.extend(right_out.constraints);
  constraints.push(Constraint::RowCombine(row_comb));

  (
    InferOut {
      constraints,
      typed_ast: Ast::branch(left_out.typed_ast, right_out.typed_ast),
    },
    out_ty,
  )
}
```

Almost exactly the same as our `Concat` case. 
Except, branch expects its `left` and `right` nodes to be functions.
We have to do more bookkeeping to ensure the return types of all our functions line up.

```rs
Ast::Inject(dir, value) => {
  let row_comb = self.fresh_row_combination();
  // Like project, inject works in terms of sub rows and goal rows.
  // But inject is _injecting_ a smaller row into a bigger row.
  let sub_row = match dir {
    Direction::Left => row_comb.left.clone(),
    Direction::Right => row_comb.right.clone(),
  };

  let out_ty = Type::Sum(row_comb.goal.clone());
  // Because of this our sub row is the type of our value
  let mut out = self.check(env, *value, Type::Sum(sub_row));
  out.constraints.push(Constraint::RowCombine(row_comb));
  (
    out.with_typed_ast(|ast| Ast::inject(dir, ast)),
    // Our goal row is the type of our output
    out_ty,
  )
}
```
Almost exactly the same as our `Project` case.
The big difference is `Inject` maps a smaller row into a bigger row.
So goal is used as our output type instead of input type.
Direction here determines if our input row is `left` or `right` (opposite of direction in `Project`).
{{% /accessory %}}

Each row case creates a fresh row combination and uses it to infer a type for each of our row terms.
TODO: transition

## Check
We've been through this [once](/posts/bidirectional-constraint-generation/) before, so we know the deal. 
After `infer` we `check` our new types.
We'll see that `check` has to do some more work to decide if we can check a node against its type.
But before that we need to check labels.

### Labels
```rs
(Ast::Label(ast_lbl, val), Type::Label(ty_lbl, ty)) 
    if ast_lbl == ty_lbl => {
  self.check(env, *val, *ty)
}
```
Much like our `Int` and `Fun` cases, a `Label` node checks against a `Label` type.
Unlike those cases, a `Label` node only checks against a `Label` type if their `label`s are equal.
Somewhat surprisingly, `Concat` and `Project` also check against a `Label` type:
```rs
(ast @ Ast::Concat(_, _), Type::Label(lbl, ty))
| (ast @ Ast::Project(_, _), Type::Label(lbl, ty)) => {
  // Cast a singleton row into a product
  self.check(env, ast, Type::Prod(Row::single(lbl, *ty)))
}
```
`Branch` and `Inject` can check against a `Label` type as well:
```rs
(ast @ Ast::Branch(_, _), Type::Label(lbl, ty))
| (ast @ Ast::Inject(_, _), Type::Label(lbl, ty)) => {
  // Cast a singleton row into a sum
  self.check(env, ast, Type::Sum(Row::single(lbl, *ty)))
}
```
The `Label` type is quite accommodating.
Because `Label`s are singleton rows, and rows aren't types, they act as an intermediary between Product and Sum types.
We're free to treat it as a product type or a sum type.
This means it can check against anything a product or sum type checks against.
It's always unambiguous whether a particular `Label` is a product or sum, 
we just don't know which until we see it in context.
With that out of the way, we move on to checking `Unlabel`:

```rs
(Ast::Unlabel(term, lbl), ty) => {
  self.check(env, *term, Type::label(lbl, ty))
}
```
An `Unlabel` checks against any type by constructing a `Label` type and checking it against `val`.
Thankfully, far fewer cases than we have for `Label`.
Next we're going to check our Product nodes.

### Products
We got an early start on checking `Concat` nodes against `Label` types. 
The bulk of our work is in checking them against `Product` types:
```rs
(Ast::Concat(left, right), Type::Prod(goal_row)) => {
  let left_row = Row::Open(self.fresh_row_var());
  let right_row = Row::Open(self.fresh_row_var());

  let left_out = self.check(env.clone(), *left, Type::Prod(left_row.clone()));
  let right_out = self.check(env, *right, Type::Prod(right_row.clone()));

  // Merge our subconstraints
  let mut constraints = left_out.constraints;
  constraints.extend(right_out.constraints);
  // Add a row combination for our goal row
  constraints.push(Constraint::RowCombine(RowCombination {
    left: left_row,
    right: right_row,
    goal: goal_row,
  }));

  InferOut {
    constraints,
    typed_ast: Ast::concat(left_out.typed_ast, right_out.typed_ast),
  }
}
```
In contrast to our `infer` case for `Concat`, our `check` case already has a goal row. 
The expected type acts as the goal row of our equation, and we just have to invent some rows to be the `left` and `right` of the equation.
We know `left` and `right` have to have `Prod` types.
All we have to do is construct `Prod` types with fresh row variables and use them to `check`.
Same as our `infer` case, we merge all our sub-constraints and add our new row combination as a new constraint.
Next up is `Project`:

```rs
(Ast::Project(dir, goal), Type::Prod(sub_row)) => {
  let goal_row = Row::Open(self.fresh_row_var());

  let (left, right) = match dir {
    Direction::Left => (sub_row, Row::Open(self.fresh_row_var())),
    Direction::Right => (Row::Open(self.fresh_row_var()), sub_row),
  };

  let mut out = self.check(env, *goal, Type::Prod(goal_row.clone()));
  out.constraints.push(Constraint::RowCombine(RowCombination {
    left,
    right,
    goal: goal_row,
  }));

  out.with_typed_ast(|ast| Ast::project(dir, ast))
}
```

Same as `Concat`, `Project` constructs a new row combination using our expected type.
`Project` uses our expected type as either our `left` or `right` row, depending on the direction.
Naturally then, our term is checked against the `goal` of our combination.
We add our row combination as a constraint, and we're on our way.

For time, I've omitted the Sum types again. 
They're fun because we check against a function type, but otherwise very similar to what we do for Product types.

{{< accessory title="Sums" >}}
Oh hey! I didn't see you there.
I was just checking our `Branch` node against a `Fun` type:

```rs
(Ast::Branch(left_ast, right_ast), Type::Fun(arg_ty, ret_ty)) => {
  let mut constraints = vec![];
  let goal = match arg_ty.deref() {
    Type::Sum(goal) => goal.clone(),
    _ => {
      let goal = self.fresh_row_var();
      constraints.push(Constraint::TypeEqual(*arg_ty, Type::Sum(Row::Open(goal))));
      Row::Open(goal)
    }
  };
  let left = Row::Open(self.fresh_row_var());
  let right = Row::Open(self.fresh_row_var());

  let left_out = self.check(
    env.clone(),
    *left_ast,
    Type::fun(Type::Sum(left.clone()), ret_ty.deref().clone()),
  );
  let right_out = self.check(
    env,
    *right_ast,
    Type::fun(Type::Sum(right.clone()), *ret_ty),
  );

  constraints.extend(left_out.constraints);
  constraints.extend(right_out.constraints);
  constraints.push(Constraint::RowCombine(RowCombination { left, right, goal }));

  InferOut {
    constraints,
    typed_ast: Ast::branch(left_out.typed_ast, right_out.typed_ast),
  }
}
```

If we could, we would actually match our expected type against `Fun(Sum(arg), ret)`.
Alas, Rust won't let us pattern match through a `Box` ([yet](https://github.com/rust-lang/rust/issues/29641)).
Instead, we begin our `check` by constraining our `arg_ty` to be a Sum and grabbing its row.

Once we've done that, checking is very similar to checking `Concat`.
Unlike `Concat`, the types we check `left` and `right` against are function types returning `ret_ty`.
That difference aside, we check our subnodes, merge our subconstraints, add our new combination, and we're done.
Onto `Inject`:

```rs
(Ast::Inject(dir, value), Type::Sum(goal)) => {
  let sub_row = self.fresh_row_var();
  let mut out = self.check(env, *value, Type::Sum(Row::Open(sub_row)));
  let (left, right) = match dir {
    Direction::Left => (sub_row, self.fresh_row_var()),
    Direction::Right => (self.fresh_row_var(), sub_row),
  };
  let row_comb = RowCombination {
    left: Row::Open(left),
    right: Row::Open(right),
    goal,
  };
  out.constraints.push(Constraint::RowCombine(row_comb));
  out.with_typed_ast(|ast| Ast::inject(dir, ast))
}
```
Armed with the context of all our other `check` cases, `Inject` is our simplest case yet.
We check our term against a fresh row variable.
Based on `dir`, we use that row variable as either `left` or `right` in our row combination.
To complete our case, we add the row combination to our constraints.

{{< /accessory >}}

# Row Constraint Generation Example
Show how row equations are generated by our new AST nodes

# Row Unification
Define row unification as equation solving. Modify unification to do row solving.

The only modifications we need to make to unification is handling our new `RowCombination` constraint.
While it's only one new constraint, this turns out to require some extensive changes to our unification algorithm.
The fundamental reason for this is that our equality of constraints has changed.
Before we added rows we could compare terms syntactically and unify equal terms.
If they weren't equal we could be confident no unification existed.
This is no longer the case, consider two row constraints:
```
(x: Int) + (f: Bool, y: Int) = (f: Bool, x: Int, y: Int)
(f: Bool, y: Int) + (x: Int) = (f: Bool, x: Int, y: Int)
```
These look different syntactically, but are actually the same!
Our row combinations are commutative, so we have to recognize these are the same equation.
That's easy in this simple example, but things get trickier once our combinations have variables:
```
a0 + b = c
b + a1 = c
```
If we squint, we actually have enough information to learn `a0 = a1` from this combination.
How are we going to know when it's valid to equate these type variables and when two combinations can't be unified?

One final straw, our equations will generally always be solved to a concrete equation we can dispatch.
But not always, like type variables, some combinations might be left unsolved at the end of unification.
We solve this, also like type variables, by generalizing the combination and adding it to our final type.

We begin in our top level `unification` function again:
```rs
fn unification(
  &mut self,
  constraints: Vec<Constraint>,
) -> Result<(), TypeError> {
  for constr in constraints {
    match constr {
      // ...
      Constraint::RowConcat(row_comb) => 
        self.unify_row_comb(row_comb)?,
    }
  }
  Ok(())
}
```
We've added a new case for our new constraint, that immediately calls `unify_row_eqn`:
```rs
fn unify_row_comb(&mut self, row_comb: RowCombination) -> Result<(), TypeError> {
  let left = self.normalize_row(row_comb.left);
  let right = self.normalize_row(row_comb.right);
  let goal = self.normalize_row(row_comb.goal);
  match (left, right, goal) {
    // ...
  }
}
```
A plethora of parallels are apparent immediately.
`unify_row_comb` has the same structure as `unify_ty_ty`.
`normalize_row` is `normalize_ty` but for rows.
It only makes sense we proceed as `unify_ty_ty` does, with a pattern match.
The cases of our match can be categorized by their row variable multiplicity:

- 0 variables - we combine our left and right rows and unify the combination with our goal row
- 1 variable - solve our 1 variable based on our two known rows, then unify our 3 known rows
- 2+ variables - we don't know enough to solve this equation, unify it against our partial equation set

Our first two cases are straightforward. 
We have enough info to produce a solved row combination, so we...do.
With two or more row variables, we don't have enough information to solve.
We have to save our row combination in our partial combination set for later.
With that, let's look at our 0 variable case:
```rs
(Row::Closed(left), Row::Closed(right), goal) => {
    let calc_goal = ClosedRow::merge(left, right);
    self.unify_row_row(Row::Closed(calc_goal), goal)
}
```
Our first case actually covers two cases.
If `goal` is closed we have 0 variables.
If `goal` is open we have 1 variable.
Thanks to our great abstractions we can handle both cases the same way.
Combine our left and right row into a new row, and unify that row against our goal row.
[ClosedRow::merge](TODO: Link to merge method) is a helper to merge closed rows.
It appends the two row's fields and sorts them while maintaining the original mapping from field to type.
We're going to be unifying a lot of rows, so we also introduce a helper [`unify_row_row`](TODO: Link to method):
```rs
fn unify_row_row(&mut self, left: Row, right: Row) -> Result<(), TypeError> {
  let left = self.normalize_row(left);
  let right = self.normalize_row(right);
  match (left, right) {
    (Row::Open(left), Row::Open(right)) => todo!(),
    (Row::Open(var), Row::Closed(row)) 
    | (Row::Closed(row), Row::Open(var)) => todo!(),
    (Row::Closed(left), Row::Closed(right)) => todo!(),
  }
}
```
A row can be open or closed. We have two rows, so there are 4 possibilities.
But two of our possibilities are handled the same way, so we only have to consider 3 cases:
```rs
(Row::Open(left), Row::Open(right)) => 
    self
      .row_unification_table
      .unify_var_var(left, right)
      .map_err(TypeError::RowsNotEqual),

```
When two row variables meet we unify them in our Union-Find. 
Easy, just like we do for type variables.
Next is our double case, a row variable and a closed row:
```rs
(Row::Open(var), Row::Closed(row)) 
| (Row::Closed(row), Row::Open(var)) => {
  self.row_unification_table
      .unify_var_value(var, Some(row.clone()))
      .map_err(TypeError::RowsNotEqual)?;
  self.dispatch_any_solved(var, row)
}
```
When a row variable meets a closed row, we can solve our variable.
To solve, we unify our variable with the closed row in our Union-Find.
This also looks a lot like our type case.
Once we've done that we call another helper `dispatch_any_solved`.
We'll talk about the details of this function later.
Its main job is to check if solving a row variable solves any of our partial row equations, and unify them if it does.
Our final case is two closed rows:

```rs
(Row::Closed(left), Row::Closed(right)) => {
  // Check that the rows we're unifying are actually unifiable
  if left.fields != right.fields {
    return Err(TypeError::RowsNotEqual((left, right)));
  }

  // If they are, our values are already in order so we can walk them and unify each
  // type
  let left_tys = left.values.into_iter();
  let right_tys = right.values.into_iter();
  for (left_ty, right_ty) in left_tys.zip(right_tys) {
    self.unify_ty_ty(left_ty, right_ty)?;
  }
  Ok(())
}
```
If two closed rows meet we can decompose them (similar to if two function types unify).
First we check they have the same fields.
If not, there's no way we can unify them, and we stop early with an error.
If their fields are equal, we zip their types together and iterate over them unifying each pair.
Now that we know how to unify rows, let's return to unifying row combinations with our 1 variable cases:
```rs
(Row::Open(var), Row::Closed(sub), Row::Closed(goal))
| (Row::Closed(sub), Row::Open(var), Row::Closed(goal)) => {
  let diff_row = self.diff_and_unify(goal, sub)?;
  self.unify_row_row(Row::Open(var), Row::Closed(diff_row))
}
```
Regardless of where our one variable resides, we handle it the same way.
Create a new row from the difference of our goal row and our sub closed row.
Unify our variable with the created row using `unify_row_row`.

An important detail lives after the 'and' in `diff_and_unify`. 
While calculating the difference between `goal` and `sub`, we also have to unify their types.
It's never happened to me, but I hear if you forget to do this your type checker silently gives you the wrong results, and you scratch your head at why for days.
Fortunately we read this article, so we're saved from that embarrassingly time-consuming mistake.
Our last case is 2+ variables:
```rs
(left, right, goal) => {
  let new_comb = RowCombination { left, right, goal };
  // Check if we've already seen an combination that we can unify against
  let poss_uni = self.partial_row_combs.iter().find_map(|comb| {
    if comb.is_unifiable(&new_comb) {
      Some(comb.clone())
    // Check the commuted row combination
    } else if comb.is_comm_unifiable(&new_comb) {
      // Commute our combination so we unify the correct rows later
      Some(RowCombination {
        left: comb.right.clone(),
        right: comb.left.clone(),
        goal: comb.goal.clone(),
      })
    } else {
      None
    }
  });

  match poss_uni {
    // Unify if we have a match
    Some(match_comb) => {
      self.unify_row_row(new_comb.left, match_comb.left)?;
      self.unify_row_row(new_comb.right, match_comb.right)?;
      self.unify_row_row(new_comb.goal, match_comb.goal)?;
    }
    // Otherwise add our combination to our list of 
    // partial combinations 
    None => {
      self.partial_row_combs.insert(new_comb);
    }
  }
  Ok(())
}
```
This case covers combinations with 2+ variables. 
We know the least about them.
We can't solve and unify them the way we could the other two cases.
Instead, we need to add them to our partial combination set until we learn enough to solve them.

Alas, we can't just insert them in our set and move on.
If we did, we'd miss another possible unification like we almost did with `diff_and_unify`.
First we have to check our existing combinations and see if our new row combination can unify with any of them.
Just because we can't solve our row combination, doesn't mean we can't learn anything new from it.

Before adding the combination we look for unifiable combinations already in our set.
How do we know when two combinations are unifiable?
Two row combinations are unifiable when two of its components are unifiable.
Since our components are rows, we know they're unifiable either when two open row variables are equal or two closed rows have equal fields.
This check is performed by helpers `is_unifiable` and `is_comm_unifiable`.
Row combinations commute so we have to check if a commuted combination could unify as well.

If such an combination exists we don't have to add our row combination to our set at all.
Instead, we unify each component of our combination against the unifiable combination already in our set.
Only once we can't find a unifiable combination do we add our combination to our set.
This ensures we learn the most about our row variables from each combination, and keeps our partial combination set minimal.

We're now successfully solving row combinations.
We have one last detail we brushed over earlier that we'll talk about now that we're older and wiser.
When a row variable unifies with a closed row we have a call to `dispatch_any_solved`.
Let's take a look at what `dispatch_any_solved` entails:

```rs
fn dispatch_any_solved(&mut self, var: RowVar, row: ClosedRow) -> Result<(), TypeError> {
  let mut changed_combs = vec![];
  self.partial_row_combs = std::mem::take(&mut self.partial_row_combs)
    .into_iter()
    .filter_map(todo!())
    .collect();

  for row_comb in changed_combs {
    self.unify_row_comb(row_comb)?;
  }
  Ok(())
}
```

`dispatch_any_solved` is called whenever we solve a row variable to its closed row.
It iterates over all our partial equations looking for any that contain our variable.
If an equation contains our variable we remove it from the partial set, replace the variable by its solution and add it to `changed_eqns`
We can see how that's done in the callback passed to `filter_map`:

```rs
|comb| match comb {
  RowCombination { left, right, goal }
    if left == Row::Open(var) => {
    changed_combs.push(RowCombination {
      left: Row::Closed(row.clone()),
      right,
      goal,
    });
    None
  }
  RowCombination { left, right, goal }
    if right == Row::Open(var) => {
    changed_combs.push(RowCombination {
      left,
      right: Row::Closed(row.clone()),
      goal,
    });
    None
  }
  RowCombination { left, right, goal }
    if goal == Row::Open(var) => {
    changed_combs.push(RowCombination {
      left,
      right,
      goal: Row::Closed(row.clone()),
    });
    None
  }
  comb => Some(comb),
}
```

After we've found all our changed equations we unify them with `unify_row_eqn`, and we've finished unification.
That involved a lot more machinery than our `unify_ty_ty`.
Rows no longer being syntactically equal turned out to have far-reaching consequences.
For the curious, theory has an explanation as to why we've accrued this complexity.
We've left the world of [syntactic unification](https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm) and entered the realm of [E-unification](https://en.wikipedia.org/wiki/Unification_(computer_science)#E-unification).
Albeit we've only dipped our toes into the sea of E-unification.

[E-unification](https://en.wikipedia.org/wiki/Unification_(computer_science)#E-unification) is equational unification.
Instead of comparing terms syntactically, you compare terms using a set of equalities.
For us the set only contains one equality: commutativity.
But in general, the set can contain any number of equalities.
Regardless of what set you select, they all share one commonality.
You lose the near linear runtime of syntactic unification once you allow any equalities.

We can see that's the case for us as well (although I've tried my best to hide it).
When we unify a row combination we walk our partial row combination.
Even worse we might trigger a cascade of iterations through continued calls of `dispatch_any_solved`.
In practice, we expect our partial combinations sets to be small for any given item.
But it's important to recognize the tradeoff we're making.


# Row unification example
Walkthrough an example unification of constraints that shows how row equation solving works.


# Shoring up our rowing
Can you see the light at the end of the tunnel?
We're so close I can almost taste the sand.
Only one thing stands between us and the beckoning shore of row types: unsolved row combinations.

Much like type variables, we'll sometimes have unsolved row combinations.
So alike in fact, that we handle them the same way.
Unsolved type variables got added to our final type as part of our [type scheme](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Polytypes).
We'll do the same with unsolved row combinations.
Quite handy that we can just foist our unsolved problems off onto someone else.
With just one caveat for row combinations, we only add an unsolved combination to our type scheme if it references a row variable used in our type.
Project and Inject both generate row combinations with unused row variables, these shouldn't show up in our final type even though they're unsolved.

With that final stroke we've reached the coveted shore.
Our language supports data types, and quite powerful ones thanks to row types.

