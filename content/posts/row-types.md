+++
title = "Rowing Afloat Datatype Boats"
date = "2023-10-21T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Type Inference"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Type Inference", "Bidirectional Typechecking", "Unification", "Constraint Solving", "E-Unification", "Row Types", "Structural Typing"]
description = "Row, row, row your types"
+++

{{< accessory title="Making a Language" >}}
This post is part of the [making a language series](/series/making-a-language).
A series that teaches you how to implement a programming language using Rust.

This post covers type inference for row types, a method for adding data types to our language.
It extends the base language with support for row types and their constructs.
{{</ accessory >}}

[Last episode](/posts/unification) we assembled a simple type inference engine end to end.
Powerful enough to check both functions _and_ integers, without any annotations at all.
Perfect as it is, our type inference (and language) lacks one teensy feature.
It doesn't have any data types.
We'll rectify that on today's episode by adding support for row types.

A row type is a mapping from labels to types, for example:

```
( x : Int, y : Int )
```

This is a row with two labels `x` and `y` both mapped to type `Int`.
Rows are used in two ways:

* As a product type where it will be a struct with two fields `x` and `y`.
* As a sum type where it will be an enum with two variants `x` and `y`.

Unlike more traditional _nominal_ data types like Algebraic Data Types (ADTs) and Classes, row types are _structural_.
Each data type in a nominal type system introduces a new name that only equals itself.
Our structural row types have a more generous definition of equality.
Two row types are equal if they have the same labels and map them to equal types.
Stated another way, we determine two types are equal by looking at their structure, not their name.

A multitude of different implementations have been devised for row systems.
Our implementation is founded on the ideas detailed in [Abstracting Extensible Data Types](https://dl.acm.org/doi/10.1145/3290325).
You couldn't ask for stronger foundations to build atop.
This implementation stands out for its use of a trait-like mechanism to represent combinations of rows.
Their mechanism maps naturally onto our parametric polymorphic type system.
Other row type systems employ either subtyping or presence polymorphism which are harder to map onto our type system.

So what's a row combination?
As long as two rows don't have any common fields,
we can combine them to produce a new row:
```
( x : Int ) + ( f : String, y: Int ) = ( f : String, x : Int, y : Int )
```
Even better, we can introduce row variables and combine them with other rows:
```
ρ + ( x : Int, y : Int ) = α
```
The utility of this is not immediately apparent.
Combining row variables like this provides a way to work with rows generically.
We can relate row variables to each other through their combinations to describe the type of functions that work on generic rows.
The same way we use traits and type variables to describe type of functions that work on generic types.

This allows our data to describe our problem domain precisely.
In a nominal system we would rely on runtime assertions to ensure we only use a valid subset of fields.
But with row types we're free to slice up our row til valid fields are the only ones available.

Compared to ADTs and Classes,
row types are a rather underexplored way to handle data types in a language.
They can only be found in the corners of OCaml or languages that retrofit static types onto JavaScript (PureScript, TypeScript, etc.).
There is a sprawling undergrowth of research about rows in academia, but very little of it has sprouted into the mainstream.
I think this is a shame, and it just so happens I'm [designing a language](/series/making-a-language/), so I'm in a prime position to do something about it.

Now that we're brimming with ideological motivation, let's plan the extension to our type inference to infer row types.
There are 3 modifications required to support row types:

  1. Create AST Nodes for row constructors
  2. Generate row type constraints from our nodes
  3. Unify row constraints to solve row types

# New AST Nodes
Six new nodes are required to support rows. 
That might seem like a lot (it increases our AST cases 250%!),
but remember these nodes are going to support all our data needs forever.
Six nodes is a bargain for all of our data types ad infinitum.

Our new nodes come in 3 pairs; each pair comprises a constructor and a destructor:

* Product types
* Sum types
* Label types -- converts normal terms to and from row terms

Our product pair is:

```rs
// We're gonna need this later
// And I didn't have a good spot to put it
// I'm sorry :(
enum Direction {
  Left,
  Right,
}

// Gotta squeeze one more in there
type Label = String
// Now onto the good stuff

enum Ast<V> {
  // ...
  Concat(NodeId, Box<Self>, Box<Self>),
  Project(NodeId, Direction, Box<Self>),
```

`Concat` combines two product types into a bigger product type.  
`Project` maps a product type into a smaller product type made from a subset of its fields.  
Our next pair is sum types:

```rs
  Inject(NodeId, Direction, Box<Self>),
  Branch(NodeId, Box<Self>, Box<Self>),
```

`Inject` maps a small sum type into a bigger sum type containing the smaller sum's cases.  
`Branch` combines two destructors for sum types into a big destructor for the combination of the two sum types.  
This is our row-ified version of a match statement.
We won't cover what that looks like in practice, but [THE paper](https://dl.acm.org/doi/10.1145/3290325) has a section devoted to it if you're interested.

Our new product and sum nodes don't include a way to make a new row,
only ways to build bigger and smaller rows out of existing rows.
Haskell might permit an infinite tree of `Concat` nodes as data, but we're stricter than that.
Fortunately, we still have 2 nodes left to solve that exact problem:

```rs
  //...
  Label(NodeId, Label, Box<Self>),
  Unlabel(NodeId, Box<Self>, Label),
}
``` 
`Label` turns a term into a singleton row.
`Unlabel` turns a singleton row back into a normal term by removing the specified label.
`Unlabel` looks like it could work on any row (not just a singleton).
While it's true it could be, we're going to require `Unlabel` is only applied to a singleton row.
It'll make more sense when we look at constraint generation. 
Requiring a single label in `Unlabel` helps us infer accurate row combinations.
When we want to unlabel a multi-label row, we first project it down to a singleton row and then unlabel it.

That's our suite of new nodes.
We can lift any term into a singleton row and then concat or inject it into a larger row.
This is all we need in theory, but in practice having to build every row as a series of `Concat`s is very verbose.
A real implementation would support constructing rows with multiple labels alongside singleton rows.

How do we employ these nodes to do all the data stuff we love to do?
Let's look at an example that combines two rows and accesses one of their members:

```rs
Ast::unlabel(
  ...,
  Ast::project(
    ...,
    Left,
    Ast::concat(
      ...,
      Ast::label(
        ...,
        "x", 
        Ast::Int(..., 4)),
      Ast::label(
        ...,
        "y", 
        Ast::Int(..., 3)),
    )
  ),
  "x"
)
```
Label is used twice here to create two singleton rows `x : Int` and `y : Int`.
These are concatenated to form one product row: `{ x : Int, y : Int }`.
That product row is projected out into a new product row: `{ x : Int }`, which is finally unlabeled to give us back our int: `4`.

# Row Types

We can use our new AST nodes to inform what new types we need, just like our base type inference.
Each new constructor that produces a value requires a type:

  * `Concat` - `Product` types
  * `Inject` - `Sum` types
  * `Label` - a type annotated with its label, we'll call it a `Label` type

Notice that none of our new types are a row type.  
Even worse, I've been lying to you this entire time.

There are **no** row types; there _never_ were.
Rows can't be types by themselves. 
They only become a type once you wrap them in a product or a sum.
We see this reflected in our new `Type` nodes:
```rs
enum Type {
  // ... our previous cases
  // I didn't hide a row type up there I promise
  /// A product type
  Prod(Row),
  /// A sum type
  Sum(Row),
  /// Type of singleton rows
  Label(Label, Box<Self>),
  // No row types here ¯\_(ツ)_/¯
}
```
Okay, trust shattered, if rows aren't types, what are they?
```rs
struct RowVar(u32);

enum Row {
  Open(RowVar),
  Closed(ClosedRow),
}
```
That's not super helpful.
A row is either open and a row variable or closed and whatever a `ClosedRow` is.
We can see `RowVar` is just an Int -- that won't help.
Hopefully `ClosedRow` can help us:
```rs
struct ClosedRow {
  // Sorted lexographically
  fields: Vec<Label>,
  // One type for each field in fields
  values: Vec<Type>,
}
```
That's starting to look familiar. 
A closed row is a map from fields to types stored as sorted vectors.

We might expect a closed row would be a HashMap (or at least a BTreeMap).
During unification, it's helpful to compare a row's fields for fast equality checks.
To make this easy we store a row's fields separate from its values.
We'll see the details later, but rest assured there's an order to the entropy.

The last piece of type machinery we need is our new row constraint.
Our new constraint is a row combination:
```rs
enum Constraint {
  // ... our previous cases
  RowCombine(NodeId, RowCombination),
}
struct RowCombination {
  left: Row,
  right: Row,
  goal: Row,
}
```
This is a constraint (and not a fact) because it is not always valid to combine rows. 
If `left` and `right` have an overlapping field, they can't be combined.
During unification, we'll work to convince ourselves we can combine the rows as stated.
If we can't, then we have a type error.

That's it though! We only need one new constraint to support rows.
That seems too good to be true given we needed **six** new AST nodes and **three** new types.
To ensure one constraint is all we need, let's look at how we use our row constraint to type our new row nodes:

| Node    | Combo |  Type |
| ------- | ----- |  ---- |
| Concat  | `l + r = g` | `{l} -> {r} -> {g}` |
| Project | `_ + r = g` | `{g} -> {r}` |
| Inject  | `_ + r = g` | `<r> -> <g>` |
| Branch  | `l + r = g` | `(<l> -> a) -> (<r> -> a) -> (<g> -> a)` |

Wow, it really typed them all.
I was putting up a strong front as the author, but I was just as skeptical as you were.
Those [Abstract Extensible Data Types](https://dl.acm.org/doi/10.1145/3290325) are really onto something.

Okay so we're convinced this one constraint is all we need. 
All we have to do is generate it.

# Row Constraint Generation
Before we get to the meat of generation, we need some boilerplate in our `TypeInference` struct:
```rs
struct TypeInference {
  // ... previous fields
  row_unification_table: 
    InPlaceUnificationTable<RowVar>,
  partial_row_combs: 
    BTreeSet<RowCombination>,
  row_to_combo: 
    HashMap<NodeId, RowCombination>,
  branch_to_ret_ty:
    HashMap<NodeId, Type>,
}
// We'll add some helper methods while we're here
impl TypeInference {
  // ... earlier stuff
  /// Create a unique row variable
  fn fresh_row_var(&mut self) 
      -> RowVar {
    self.row_unification_table.new_key(None)
  }

  /// Create a row equation with fresh row variables
  fn fresh_row_combination(&mut self) 
      -> RowCombination {
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
These are row combinations we don't know enough about to solve and will solve as we learn more row information during unification.

Our final new members, `row_to_combo` and `branch_to_ret_ty`, store output for the end of type inference.
`row_to_combo` maps our Row AST's `NodeId` to the row combination we'll generate for each of them.
`branch_to_ret_ty` maps Branch `NodeId`s to the return type we infer for the node.
We'll use these in later passes, so we save them during type inference.

Each row node may give rise to any number of possible row combinations.
A `Project` can return any subset of fields from its input row.
We can rely on type inference to figure out which particular row combination is being used at any given row node.
Unlike our variables, however, we can't later figure out which row combination was picked by simply reconstructing types.
Instead, we save our row combination allowing us to know what selection type inference made in later passes.

## Infer
Okay, enough bureaucracy, let's look at our `infer` cases for rows starting with our `Label` case:

### Labels
```rs
Ast::Label(id, label, value) => {
  let (out, value_ty) = 
    self.infer(env, *value);
  (
    out.with_typed_ast(|ast| 
      Ast::label(
        id,
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
We infer a type for our value and wrap whatever type we infer as a new `Label` type using the provided `label`.
Easy, onto the `Unlabel` case:
```rs
Ast::Unlabel(id, value, label) => {
  let value_var = 
    self.fresh_ty_var();
  let expected_ty = 
    Type::label(label, Type::Var(value_var));
  let out = 
    self.check(env, *value, expected_ty);
  (
    out.with_typed_ast(|ast|
      Ast::unlabel(id, ast, label)),
    Type::Var(value_var)
  )
}
```
Our unlabel case is more interesting.
`check` allows us to exploit the extra info `Unlabel` provides.
We know the value of an `Unlabel` has to be a `Label` type, and that type must contain field `label`.
We make use of this by constructing a `Label` with a fresh type variable and check our `value` against it.

### Products
We're getting the hang of it. 
`Concat` will make use of our new row combination constraint:
```rs
Ast::Concat(id, left, right) => {
  let row_comb = 
    self.fresh_row_combination();

  // Concat combines two smaller rows into a larger row.
  // To check this we check that our inputs have the types of our smaller rows left
  // and right.
  let left_out = 
    self.check(env.clone(), *left, 
      Type::Prod(row_comb.left.clone()));
  let right_out = 
    self.check(env, *right, 
      Type::Prod(row_comb.right.clone()));

  // If they do, then our output type is our big row goal
  let out_ty = 
    Type::Prod(row_comb.goal.clone());
  let mut constraints =
    left_out.constraints;
  constraints.extend(right_out.constraints);
  // Add a new constraint for our row equation to solve concat
  constraints.push(Constraint::RowCombine(row_comb.clone()));
  self.row_to_ev.insert(id, row_comb);

  let typed_ast = Ast::concat(
    id,
    left_out.typed_ast,
    right_out.typed_ast,
  );

  (
    InferOut {
      constraints,
      typed_ast,
    },
    out_ty,
  )
}
```
We start by creating a fresh row combination.
The goal of this row combination will be our inferred product type.
We know our `left` and `right` nodes have to be product types.
Again we can exploit this knowledge to check them against Product types made from our combination's `left` and `right` rows.

This pattern is loosely shared by all our row nodes.
We'll see each of them store their generated `RowCombination`.
A similar approach works for `Project`:

```rs
Ast::Project(id, dir, goal) => {
  let row_comb = 
    self.fresh_row_combination();

  let sub_row = match dir {
    Direction::Left => 
      row_comb.left.clone(),
    Direction::Right => 
      row_comb.right.clone(),
  };

  // We check against our goal row
  let mut out = 
    self.check(env, *goal, Type::Prod(row_comb.goal.clone()));

  out
    .constraints
    .push(Constraint::RowCombine(row_comb.clone()));
  self.row_to_ev.insert(id, row_comb);
  (
    out.with_typed_ast(|ast| 
      Ast::project(id, dir, ast)),
    // Our sub row is the output type of the projection
    Type::Prod(sub_row),
  )
}
```

One notable difference, the goal of our row combination is our input type this time.
Our output type is either the `left` or `right` of our row combination based on direction.
Direction exists solely to allow us to toggle whether `left` or `right` is used.
It has little bearing on our current row usage, so we'll skim past it (as always details in [the paper](https://dl.acm.org/doi/10.1145/3290325)).

Inferring sum types is very similar to inferring product types (the magic of symmetry). We'll omit it for time, but feel free to take a look if you're interested:

{{< accessory title="Sums" >}}
Thanks for stopping by!

```rs
Ast::Branch(id, left, right) => {
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
  constraints
    .extend(right_out.constraints);
  constraints
    .push(Constraint::RowCombine(row_comb.clone()));
  self.row_to_ev
      .insert(id, row_comb);
  self.branch_to_ret_ty
      .insert(id, Type::Var(ret_ty));

  (
    InferOut {
      constraints,
      typed_ast: Ast::branch(
        id,
        left_out.typed_ast, 
        right_out.typed_ast
      ),
    },
    out_ty,
  )
}
```

This is almost exactly the same as our `Concat` case. 
Except branch expects its `left` and `right` nodes to be functions.
We have to do more bookkeeping to ensure the return types of all our functions line up.

```rs
Ast::Inject(id, dir, value) => {
  let row_comb = 
    self.fresh_row_combination();

  let sub_row = match dir {
    Direction::Left => 
      row_comb.left.clone(),
    Direction::Right => 
      row_comb.right.clone(),
  };

  let out_ty = 
    Type::Sum(row_comb.goal.clone());

  let mut out = 
    self.check(env, *value, Type::Sum(sub_row));
  out
    .constraints
    .push(Constraint::RowCombine(row_comb.clone()));
  self.row_to_ev.insert(id, row_comb);
  (
    out.with_typed_ast(|ast| 
      Ast::inject(id, dir, ast)),
    // Our goal row is the type of our output
    out_ty,
  )
}
```

This one is almost exactly the same as our `Project` case.
The big difference is `Inject` maps a smaller row into a bigger row.
So goal is used as our output type instead of input type.
Direction here determines if our input row is `left` or `right` (opposite of direction in `Project`).
{{< /accessory >}}

Each row case creates a fresh row combination and uses it to infer a type for each of our row terms.
As we'll soon discover, `check` takes the same approach to check row types.

## Check
We've been through this [once](/posts/bidirectional-constraint-generation/) before. We're experts now. 
After `infer`, we `check` our new types.
We'll see that `check` has more work to do to decide if a node checks against its type.
But before any of that, we need to check labels:

### Labels
```rs
( Ast::Label(id, ast_lbl, val)
, Type::Label(ty_lbl, ty)) 
    if ast_lbl == ty_lbl => {
  self.check(env, *val, *ty)
    .with_typed_ast(|term| Ast::label(id, ast_lbl, term))
}
```
Like our `Int` or `Fun` case, a `Label` node checks against a `Label` type.
Unlike those cases, a `Label` node only checks against a `Label` type if their `label`s are equal.
Perhaps more surprisingly, `Concat` and `Project` also check against a `Label` type:
```rs
(ast @ Ast::Concat(_, _), Type::Label(lbl, ty))
| (ast @ Ast::Project(_, _), Type::Label(lbl, ty)) => {
  // Cast a singleton row into a product
  self.check(env, ast, 
    Type::Prod(Row::single(lbl, *ty)))
}
```
As we might imagine, `Branch` and `Inject` check against a `Label` type as well:
```rs
(ast @ Ast::Branch(_, _), Type::Label(lbl, ty))
| (ast @ Ast::Inject(_, _), Type::Label(lbl, ty)) => {
  // Cast a singleton row into a sum
  self.check(env, ast, 
    Type::Sum(Row::single(lbl, *ty)))
}
```
The `Label` type is certainly accommodating.
Because `Label` types act like singleton rows, and rows aren't types, they act as an intermediary between Product and Sum types.
Both a product and a sum could wrap a label type by converting it to a row.
But until a product or sum coerces our label, we don't know which it will be.
So we have to account for all possibilities in our checking logic.
Moving along, next is our much more succinct `Unlabel` case:
```rs
(Ast::Unlabel(id, term, lbl), ty) => {
  self.check(env, *term, 
    Type::label(lbl.clone(), ty))
      .with_typed_ast(|term| Ast::unlabel(id, term, lbl))
}
```
An `Unlabel` checks against any type by constructing a `Label` type and checking it against `val`.
Thankfully, far fewer cases than we have for `Label`.
Next we're going to check our Product nodes.

### Products
We got an early start on checking `Concat` nodes against `Label` types. 
The bulk of our work is in checking them against `Product` types:
```rs
(Ast::Concat(id, left, right), Type::Prod(goal_row)) => {
  let left_row = 
    Row::Open(self.fresh_row_var());
  let right_row = 
    Row::Open(self.fresh_row_var());

  let left_out = 
    self.check(env.clone(), *left, 
      Type::Prod(left_row.clone()));
  let right_out = 
    self.check(env, *right, 
      Type::Prod(right_row.clone()));

  // Merge our subconstraints
  let mut constraints = left_out.constraints;
  constraints.extend(right_out.constraints);
  // Add a row combination for our goal row
  let row_comb = RowCombination {
    left: left_row,
    right: right_row,
    goal: goal_row.clone(),
  };

  constraints
    .push(Constraint::RowCombine(row_comb.clone()));
  self.row_to_ev
      .insert(id, row_comb);

  let typed_ast = Ast::concat(
    id,
    left_out.typed_ast,
    right_out.typed_ast,
  );

  InferOut {
    constraints,
    typed_ast,
  }
}
```
In contrast to our `infer` case for `Concat`, our `check` case already has a goal row. 
The expected type acts as the goal row of our equation. 
We just have to invent some rows to be the `left` and `right` of the equation.
Same as `infer`, we know `left` and `right` must have `Prod` types.
All we have to do is construct `Prod` types with fresh row variables and use them to `check`.
Finally, we merge all our sub-constraints and add our new row combination as a new constraint.
Next up is `Project`:

```rs
(Ast::Project(id, dir, goal), Type::Prod(sub_row)) => {
  let goal_row = Row::Open(self.fresh_row_var());

  let (left, right) = match dir {
    Direction::Left => (sub_row, Row::Open(self.fresh_row_var())),
    Direction::Right => (Row::Open(self.fresh_row_var()), sub_row),
  };

  let mut out = self.check(env, *goal, Type::Prod(goal_row.clone()));
  let row_comb = RowCombination {
    left,
    right,
    goal: goal_row,
  };
  out.constraints
     .push(Constraint::RowCombine(row_comb.clone()));

  self.row_to_ev.insert(id, row_comb);
  out.with_typed_ast(|ast| 
    Ast::project(id, dir, ast))
}
```

Just like `Concat`, `Project` constructs a new row combination using our expected type.
But `Project` uses our expected type as either the `left` or `right` row, depending on direction, not as our goal row.
Naturally then, our term is checked against the `goal` of our combination.
We add our row combination as a constraint, and we're on our way.

I've omitted the Sum types again. 
They're fun because we check against a function type, but otherwise very similar to what we do for Product types.

{{< accessory title="Sums" >}}
Oh hey! I didn't see you there.
I was just checking our `Branch` node against a `Fun` type:

```rs
(Ast::Branch(id, left_ast, right_ast), Type::Fun(arg_ty, ret_ty)) => {
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
  let row_comb = RowCombination { left, right, goal };
  constraints
    .push(Constraint::RowCombine(row_comb.clone()));
  self.row_to_ev.insert(id, row_comb);
  self.branch_to_ret_ty.insert(id, *ret_ty);

  InferOut {
    constraints,
    typed_ast: Ast::branch(
      id,
      left_out.typed_ast, 
      right_out.typed_ast
    ),
  }
}
```

If we could, we would actually match our expected type against `Fun(Sum(arg), ret)`.
Alas, Rust won't let us pattern match through a `Box` [yet](https://github.com/rust-lang/rust/issues/29641).
Instead, we begin our `check` by constraining our `arg_ty` to be a Sum and grabbing its row.

Once we've done that, checking is very similar to checking `Concat`.
Unlike `Concat`, the types we check `left` and `right` against are function types returning `ret_ty`.
That difference aside, we check our subnodes, merge our subconstraints, add our new combination, and we're done.
Onto `Inject`:

```rs
(Ast::Inject(id, dir, value), Type::Sum(goal)) => {
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
  out.constraints
    .push(Constraint::RowCombine(row_comb.clone()));
  self.row_to_ev.insert(id, row_comb);
  out.with_typed_ast(|ast| 
    Ast::inject(id, dir, ast))
}
```
Armed with the context of all our previous `check` cases, `Inject` is our simplest case yet.
We check our term against a fresh row variable.
Based on `dir`, we use that row variable as either `left` or `right` in our row combination.
To complete our case, we add the row combination to our constraints.

{{< /accessory >}}

# Row Unification

The only modifications we need to make to unification is handling our new `RowCombination` constraint.
While it's only one new constraint, this turns out to require some extensive changes to unification.
The fundamental reason for this is that our equality of constraints has changed.
Before we added rows, we could compare terms syntactically and unify equal terms.
If they weren't equal, we could be confident no unification existed.
That's no longer the case, consider these two row constraints:
```
(x: Int) + (f: Bool, y: Int) = (f: Bool, x: Int, y: Int)
(f: Bool, y: Int) + (x: Int) = (f: Bool, x: Int, y: Int)
```
These look different syntactically yet are still equal.
Our row combinations are commutative, so we have to recognize these are the same equation.
That's easy in this simple example, but things get trickier once our combinations have variables:
```
a0 + b = c
b + a1 = c
```
If we squint, we actually have enough information to learn `a0 = a1` from this combination.
How are we going to know when it's valid to equate these type variables and when two combinations can't be unified?

One final straw, our equations will usually be solved to a concrete equation we can dispatch.
But not always, like type variables, some combinations might be left unsolved at the end of unification.
We solve this, like type variables, by generalizing the combination and adding it to our final type.
With the problems presented, we begin back in our top level `unification` function:
```rs
fn unification(
  &mut self,
  constraints: Vec<Constraint>,
) -> Result<(), TypeError> {
  for constr in constraints {
    match constr {
      // the other constraint
      Constraint::RowConcat(row_comb) => 
        self.unify_row_comb(row_comb)
            .map_err(|kind| TypeError { kind, node_id })?,
    }
  }
  Ok(())
}
```
We've added a new case for our new constraint that immediately calls `unify_row_comb`:
```rs
fn unify_row_comb(&mut self, row_comb: RowCombination) -> Result<(), TypeErrorKind> {
  let left = self.normalize_row(row_comb.left);
  let right = self.normalize_row(row_comb.right);
  let goal = self.normalize_row(row_comb.goal);
  match (left, right, goal) {
    // ...
  }
}
```
A plethora of parallels present themselves promptly:

* `unify_row_comb` has the same structure as `unify_ty_ty`
* `normalize_row` is `normalize_ty` but for rows
* (Don't look at me, couple didn't fit the alliteration)

It only makes sense we proceed, as `unify_ty_ty` does, with a pattern match.
The cases of our match can be categorized by their row variable count:

- 0 variables -- we combine our left and right rows and unify the combination with our goal row
- 1 variable -- solve our 1 variable based on our two known rows
- 2+ variables -- we don't know enough to solve this equation

With that, let's look at our 0 variable case(s):
```rs
(Row::Closed(left), Row::Closed(right), goal) => {
  let calc_goal = ClosedRow::merge(left, right);
  self.unify_row_row(Row::Closed(calc_goal), goal)
}
```
This first case actually covers two cases:

* When `goal` is closed, we have 0 variables.
* When `goal` is open, we have 1 variable.

Isn't counting fun?
Thanks to our great abstractions, we handle both cases the same way.
Combine our `left` and `right` into a new row and unify that row against our `goal`.
`ClosedRow::merge` is a helper to merge closed rows.
It appends the two rows' fields and sorts them while maintaining the original mapping from field to type.
We're going to be unifying a lot of rows; let's introduce a helper `unify_row_row` to do it for us:
```rs
fn unify_row_row(&mut self, left: Row, right: Row) -> Result<(), TypeErrorKind> {
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
A row can be open or closed. 
We have two rows, so there are 4 possibilities.
But two of our possibilities are handled the same way, so we only have to consider 3 cases:
```rs
(Row::Open(left), Row::Open(right)) => 
  self
    .row_unification_table
    .unify_var_var(left, right)
    .map_err(TypeError::RowsNotEqual),

```
When two row variables meet, we unify them in our Union-Find.
Easy, just like we do for type variables.
Next is our BOGO case, a row variable and a closed row:
```rs
(Row::Open(var), Row::Closed(row)) 
| (Row::Closed(row), Row::Open(var)) => {
  self.row_unification_table
      .unify_var_value(var, Some(row.clone()))
      .map_err(TypeError::RowsNotEqual)?;
  self.dispatch_any_solved(var, row)
}
```
When a row variable meets a closed row, we solve our variable to that row.
To solve, we unify our variable with the closed row in our Union-Find.
(This also looks a lot like our type case.)
Once we've done that we call another helper `dispatch_any_solved`.
We'll talk about the details of this function later.
Its job is to check if solving a row variable solves any of our partial row equations.
The final case is two closed rows:

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
If two closed rows meet, we can decompose them (same as when two function types meet).
First, we check they have equal fields.
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
Regardless of where our single variable resides, we handle it the same way.
We have enough to solve our variable from our goal row and our sub row.
We create a new row from the difference of our goal row and our sub row.
Determining the difference between our goal row and sub row is done by `diff_and_unify`.
The created row is unified with our variable using `unify_row_row`.


Note an important detail that lives after the 'and' in `diff_and_unify`.
While calculating the difference between `goal` and `sub`, we also have to unify `goal` and `sub` themselves.
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
We know the least about cases with 2+ variables.
We can't solve them the way we could our other two cases.
Instead, we need to add them to our partial combination set.
At least until we learn enough to solve them.

Alas, we can't just insert them in our set and move on.
If we did, we'd miss another possible unification like we almost did with `diff_and_unify`.
First, we check our existing combinations, and see if our new row combination can unify with any of them.
Just because we can't solve our row combination, doesn't mean we can't learn anything new from it.

Before adding a combination we look for existing combinations that can unify with our combination.
To do that, we have to know when combinations are unifiable.
Two row combinations are unifiable when two of their components are unifiable.
A quick example to exemplify:
```
b + c = (x : Int, y : e)
b + d = (x : Int, y : Int)
```
We know these two combinations can unify because two of their components can unify.
`b` unifies with itself because it's a row variable.
`(x : Int, y : e)` unifies with `(x : Int, y : Int)` because they have the same fields, and the types at each field unify.
This check is performed by helpers `is_unifiable` and `is_comm_unifiable`.
Row combinations commute so we have to check both orders of their components.

If we find a unifiable combination, we can skip adding a new row combination to our set entirely.
It suffices to unify our combination against the found combination.
Only when we can't find a unifiable combination do we need to insert our combination into our set.
This guarantees we learn the most about our row variables from each combination and keeps our partial combination set minimal.

We're now successfully solving row combinations.
There's just one last detail we brushed over earlier that we'll cover now.
When a row variable unifies with a closed row we call `dispatch_any_solved`.
What does `dispatch_any_solved` entail:

```rs
fn dispatch_any_solved(&mut self, var: RowVar, row: ClosedRow) -> Result<(), TypeErrorKind> {
  let mut changed_combs = vec![];
  self.partial_row_combs = std::mem::take(&mut self.partial_row_combs)
    .into_iter()
    .filter_map(/* look for our variable */)
    .collect();

  for row_comb in changed_combs {
    self.unify_row_comb(row_comb)?;
  }
  Ok(())
}
```

`dispatch_any_solved` iterates over all our partial equations looking for any that contain our variable.
If an equation contains our variable, we remove it from the partial set, replace the variable by its solution, and add it to `changed_combs`.
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

After we've found all our changed equations we unify them with `unify_row_comb`, and we've finished unification.
That involved a lot more machinery than our `unify_ty_ty`.
Rows no longer being syntactically equal turned out to have far-reaching consequences.
For those curious, theory has an explanation for this added complexity.
We've left the world of [syntactic unification](https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm) and entered the realm of [E-unification](https://en.wikipedia.org/wiki/Unification_(computer_science)#E-unification).
Granted, we're only dipping our toes into the sea of E-unification.

[E-unification](https://en.wikipedia.org/wiki/Unification_(computer_science)#E-unification) is equational unification.
Instead of comparing terms purely syntactically, terms are compared with a set of equalities.
For us the set only contains one equality: commutativity (`x + y = y + x`).
But in general, the set can contain any number of equalities.
Regardless of what set you select, they all share one commonality.
You lose the near linear runtime of syntactic unification once you admit a set of equalities.

We can see that's the case for us as well (although I've tried my best to hide it).
When we unify a row combination, we iterate over our partial row combinations.
Even worse we might trigger a cascade of iterations through continued calls to `dispatch_any_solved`.
In practice, we expect our partial combinations sets to be small for any given item.
But it's important to recognize the tradeoff we're making.

# Shoring up our rowing
Can you see the light at the end of the tunnel?
Only one thing stands between us and the beckoning shore of row types: unsolved row combinations.
We're so close, I can taste the sand.

Much like type variables, we'll sometimes have unsolved row combinations.
So alike in fact that we handle them the same way.
Unsolved type variables got added to our final type as part of our [type scheme](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Polytypes).
We'll do the same with unsolved row combinations.
With just one caveat, we only add an unsolved combination to our type scheme if it references a row variable used in our type.
Project and Inject both generate row combinations with unused row variables, these shouldn't show up in our final type even though they're unsolved.

Quite handy that we can just foist our unsolved problems off onto someone else.
With that final stroke we've reached the coveted shore.
Our language supports data types, and quite flexible ones, thanks to row types.
We skimmed over some code for time, but if you want all the gory details you can always find them in the [full implementation](https://github.com/thunderseethe/making-a-language/tree/main/types/rows).
Next up we have two options: typecheck [top level functions](/posts/check-top-level-items) or [lower rows](/posts/lowering-rows-intro).

