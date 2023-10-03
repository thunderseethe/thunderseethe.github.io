+++
title = "Part 3: Row Types"
date = "2023-08-12T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Type Inference"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Type Inference", "Bidirectional Typechecking", "Unification", "Constraint Solving", "E-Unification", "Row Types", "Structural Typing"]
description = "Row, row, row your types"
+++

# Introduction
Define and motivate why we want row types. 
Provide background for our row implementation. (Possibly lay out steps to implement?)

  * Row types allow combining data types structurally. 
    * Represent Algebraic Data Types, products and sums
    * As opposed to more traditional solutions that are nominal.
    * Abstracting Extensible Data Types (TODO: link)
        * Doesn't use subtyping (which is often how row typing occurs)
        * Maps to type class semantics under the hood which fits in well with the rest of our goals for language
    * Row maps labels to types
        * Example: `( x: Int, y: String )`
            * This is a row with two fields x and y where x maps to type Int and y maps to type String.
        * On its own this row is not a type
        * We have to interpret it as either a Product or a Sum to form a type
        * The row can be a product as a pair of an Int and a String: `{ x: Int, y: String }`
        * Or the row can be a sum with two cases an x case holding an Int and a y case holding a String: `< x: Int, y: String >`
        * The same row is used for both data types just interpreted two different ways
    * Functions can be polymorphic over rows
        * Functions can have row variables that are constrained to contain necessary fields and types
        * Then the function will work on any row that meets those constraints
        * Same idea as type polymorphic functions
    * Rows are somewhat underexplored outside of research.
        * Dynamic languages
        * OCaml but they are not often used and by no means the main way to represent data
    * I'd like to put them front and center to see how their tradeoffs play out.
    * This is a bit of an experiment.

  * Steps:
    1. Add new AST Nodes for row constructors
    2. Infer new row types for the constructs (and new row constraint)
    3. Unify new row constraints to solve row equations

# Refresher
Cover what we've done up till now:

  * AST has 4 nodes: Int, Var, Func, App. It is the lambda calculus plus int literals.
  * Types are either Int, Function, or Variable.
  * Type checking is split into two phases constraint generation and constraint solving.
    * Constraint generation is done with a bidirectional type system
    * Constraint solving is done with unification using union-find
  * Hindley-Milner type system with no frills

# AST (WIP Title)
Six new nodes are required to support rows. 
That might seem like a lot, it increases our AST case 250%! 
However, remember these nodes are going to support all our data needs forever. 
When you look at it that way six nodes is a steal.
The nodes are:

 * Project and Concat
    * These construct and destruct product types
    * Concat combines two product types into a bigger product type
    * Project maps a big product into a smaller product made from a subset of its fields
 * Inject and Branch
    * These construct and destruct sum types
    * Inject maps a small sum into a bigger sum containing the smaller sum's cases
    * Branch combines two destructor terms for sum types into a big destructor term for the combination of the two sum types
        * This is our version of a match statement it just looks different to support the flexibility of rows

 * Lot of parallels between our product and sum row nodes:

 | <br>    | Construct | Destruct |
 | ------- | --------- | -------- |
 | Product | Concat    | Project  |
 | Sum     | Inject    | Branch   |

Product and Sum are compliments of each other.
Both have one operator to map between bigger rows and smaller rows, and one operator to combine rows.
They just serve opposite purposes for products and sums.
That's beautiful symmetry, but I have some concerns.
Our new nodes don't include a way to make a new row, only ways to build bigger and smaller rows out of existing rows.
Haskell might be okay with treating an infinite tree of `Concat` nodes as data, but we're stricter than that.
Fortunately we still have 2 more nodes (that solve that exact problem):

 * Label and Unlabel
    * These constructors move normal terms into and out of the row world
    * Label lifts a term into a row by giving it a label turning it into a singleton row
        * A Label term can be used with any of our previous row nodes, and it will cast to either a product or sum as needed
    * Unlabel lowers a term out of a row by removing the label from a singleton row
        * It is invalid to unlabel on a row with more than one field

With these we now have a way to turn any term into a singleton row. 
Once it's a row we can use our other row nodes to transform it as needed.

TODO: Should we note that we aren't limited to singleton row construction? We're free to support more complicated row instantiation?

# Row Types

Same as our original type inference, we can use our new AST nodes to inform what new types we need to cover.
The new nodes we have that construct values all require a type:
    * Concat - combines two product values into a new product value
    * Inject - injects a sum value into a bigger sum value
    * Label - labels a value turning it into a new row value

We'll create a type for each of these nodes:
    * Product - The type of products produced by `Concat`
    * Sum - The type of sums produced by `Inject`
    * Label - The type of singleton row produced by `Label`

Notice that none of our new types are a row type.
In fact, now that you've made it this far, a row type is a misnomer in our system.
Rows aren't types themselves, they are only types once you wrap them as either a product type or a sum type.

TODO: Consider noting label could be considered a row type, but we will not to prevent confusion

This is reflected in the additions to our `Type`:
```rs
pub enum Type {
    // ... our previous cases
    /// A product type
    Prod(Row),
    /// A sum type
    Sum(Row),
    /// Type of singleton rows
    Label(Label, Box<Self>),
}
```
TODO: Transition
```rs
pub enum Row {
    Open(RowVar),
    Closed(ClosedRow),
}
```
That's not super helpful.
A row is either open and a row variable, or closed and whatever a `ClosedRow` is.
Let's crack open `ClosedRow`:

```rs
pub struct ClosedRow {
    pub fields: Vec<Label>,
    pub values: Vec<Type>,
}
```

That's starting to look familiar. 
A closed row is a map from fields to types, stored as sorted vectors.
We might expect a closed row would use a HashMap or BTreeMap. 
The reason we don't do that will become clear in unification. 
During unification, it's helpful to access just a rows fields for equality checks.

If a closed row is just a row, what's an open row?
An open row is how we encode row polymorphism.
It stands for a row we don't know yet and will be determined by our caller.
Our row variables don't do much on their own.
Fortunately, our new row constraint will let us do more with rows

```rs
pub enum Constraint {
  // ... our previous cases
  RowConcat(RowEquation),
}
pub struct RowEquation {
  pub left: Row,
  pub right: Row,
  pub goal: Row,
}
```

A row equation is a constraint that says `left + right = goal`.
Adding two rows means combining them to create a new row `goal` that contains the fields of `left` and `right` and maps them to their corresponding values.
It is not always valid thing to combine rows, if `left` and `right` have the same field we can't combine them.
Rows must be disjoint to be combined.

That's it though! We only need one new constraint to support rows.
That might seem too good to be true, we needed 6 new AST nodes and 3 new types to support rows.
Let's look at how we use our row constraint to type our new AST nodes.
We'll use some shorthand for space: `{}` for Prod, `<>` for Sum, and `left + right = goal` for a row equation of row variables:

| <br>    | Equation | Type |
| ------- | ----- | ---- |
| Concat  | `l + r = g` | `{l} -> {r} -> {g}` |
| Project | `_ + r = g` | `{g} -> {r}` |
| Inject  | `_ + r = g` | `<r> -> <g>` |
| Branch  | `l + r = g` | `(<l> -> a) -> (<r> -> a) -> (<g> -> a)` |

Wow, it really did it.
We typed all those nodes with just one row constraint.
That's pretty neat and gives us some insight on how we're gonna type check rows.
We generate a bunch of these row equations and try to solve them during unification.

# Row Constraint Generation
Introduce the new constraint to support row types. Cover modifications to infer and check. Purport the benefits of check for rows

We have everything we need to start on constraint generation: row AST nodes, row Types, and our row constraint.
Before we get to the heart of generation, we have to do some boiler plate in our `TypeInference` struct:

```rs
struct TypeInference {
  // ... previous fields
  row_unification_table: InPlaceUnificationTable<RowVar>,
  partial_row_eqns: BTreeSet<RowEquation>,
}
// We'll add some helper methods while we're here
impl TypeInference {
  // ... earlier stuff
  /// Create a unique row variable
  fn fresh_row_var(&mut self) -> RowVar {
      self.row_unification_table.new_key(None)
  }

  /// Create a row equation with fresh row variables
  fn fresh_row_equation(&mut self) -> RowEquation {
      RowEquation {
          left: Row::Open(self.fresh_row_var()),
          right: Row::Open(self.fresh_row_var()),
          goal: Row::Open(self.fresh_row_var()),
      }
  }
  // ...later stuff
}
```

We have a new kind of variable for rows, so we have a new unification table.
Our row variables will be solved to `ClosedRow`s.
Alongside this we have a set of partial row equations, we'll need that later during unification.
These are row equations we don't know enough about to solve, and will solve as we learn more row information during unification.

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
            )),
        Type::label(
            label, 
            value_ty
        ),
    )
}
```

Our label case is straightforward.
We infer a type for our value, and wrap whatever we infer as a new `Label` type using our provided `label`.
It only makes sense to look at `Unlabel` next:
```rs
Ast::Unlabel(value, label) => {
    let value_var = self.fresh_ty_var();
    let expected_ty = Type::label(label, Type::Var(value_var));
    (self.check(env, *value, expected_ty), Type::Var(value_var))
}
```
Our unlabel case is more interesting. 
We get to make use of our check mode to exploit the extra info `Unlabel` provides.
We know the value of an `Unlabel` has to be a Label type.
So we can construct a Label with a fresh type variable and check our `value` against it.

### Products
TODO: Transition into product nodes

```rs
// Concat combines two smaller rows into a larger row.
Ast::Concat(left, right) => {
    // Create a row equation with fresh row variables
    let row_eqn = self.fresh_row_equation();

    // Check that left is a product type with our fresh left row variable
    let left_out = self.check(env.clone(), *left, Type::Prod(row_eqn.left.clone()));
    // Check that right is a product type with our fresh right row variable
    let right_out = self.check(env, *right, Type::Prod(row_eqn.right.clone()));

    // Our output type is the goal of our fresh equation
    let out_ty = Type::Prod(row_eqn.goal.clone());
    let mut constraints = left_out.constraints;
    constraints.extend(right_out.constraints);
    // Add a new constraint to solve our row equation
    constraints.push(Constraint::RowConcat(row_eqn));

    (
        InferOut {
            constraints,
            typed_ast: Ast::concat(left_out.typed_ast, right_out.typed_ast),
        },
        out_ty,
    )
}
```
`Concat` is the first node where we make use of our new row equations.
We know our `left` and `right` nodes have to be product types.
We exploit this knowledge to check them against Product types made from our fresh row equation instead of inferring their types.
The inferred type for our `Concat` node is then the goal row decided by our equation wrapped as a Product type.
TODO: Transition
```rs
Ast::Project(dir, goal) => {
    let row_eqn = self.fresh_row_equation();
    // Based on the direction of our projection,
    // our output row is either left or right
    let sub_row = match dir {
        Direction::Left => row_eqn.left.clone(),
        Direction::Right => row_eqn.right.clone(),
    };
    // Project transforms a row into a subset of its fields, so we check our goal ast
    // node against our goal row (not our sub_row)
    let mut out = self.check(env, *goal, Type::Prod(row_eqn.goal.clone()));
    // Add our row equation constraint to solve our projection
    out.constraints.push(Constraint::RowConcat(row_eqn));
    (
        out.with_typed_ast(|ast| Ast::project(dir, ast)),
        // Our sub row is the output type of the projection
        Type::Prod(sub_row),
    )
}
```
* Project uses goal as check value not left or right
* Left or right is the output of based on direction
    * Talk about direction? Defer to explainer?
* TODO: Transition

### Sums
```rs
Ast::Branch(left, right) => {
    let row_eqn = self.fresh_row_equation();
    let ret_ty = self.fresh_ty_var();

    // Branch expects it's two inputs to be handling functions
    // with type: <sum> -> a
    // So we check that our left and right AST both have function types that
    // agree on return type
    let left_out = self.check(
        env.clone(),
        *left,
        Type::fun(
            Type::Sum(row_eqn.left.clone()), 
            Type::Var(ret_ty)
        ),
    );
    let right_out = self.check(
        env,
        *right,
        Type::fun(
            Type::Sum(row_eqn.right.clone()), 
            Type::Var(ret_ty)
        ),
    );

    // If they do the overall type of our Branch node is a function from our goal row
    // sum type to our return type
    let out_ty = Type::fun(
        Type::Sum(row_eqn.goal.clone()), 
        Type::Var(ret_ty)
    );
    // Collect all our constraints for our final output
    let mut constraints = left_out.constraints;
    constraints.extend(right_out.constraints);
    constraints.push(Constraint::RowConcat(row_eqn));

    (
        InferOut {
            constraints,
            typed_ast: Ast::branch(left_out.typed_ast, right_out.typed_ast),
        },
        out_ty,
    )
}
```
* Almost exactly the same as our `Concat` case
* Our types are of the form `<sum> -> a` instead of just `{prod}`
```rs
Ast::Inject(dir, value) => {
    let row_eqn = self.fresh_row_equation();
    // Like project, inject works in terms of sub rows and goal rows.
    // But inject is _injecting_ a smaller row into a bigger row.
    let sub_row = match dir {
        Direction::Left => row_eqn.left.clone(),
        Direction::Right => row_eqn.right.clone(),
    };

    let out_ty = Type::Sum(row_eqn.goal.clone());
    // Because of this our sub row is the type of our value
    let mut out = self.check(env, *value, Type::Sum(sub_row));
    out.constraints.push(Constraint::RowConcat(row_eqn));
    (
        out.with_typed_ast(|ast| Ast::inject(dir, ast)),
        // Our goal row is the type of our output
        out_ty,
    )
}
```
* Almost exactly the same as our `Project` case
* Except we're injecting a smaller row into a bigger row, so goal is used in our output type instead of input type.

* And that's all our infer cases
* Each row case generates a row equation and checks its inputs against the row equation and creates its inferred output type from the row equation
  * We make much more use of check for these nodes then previously.

## Check
* Now we need to modify `check` to handler our new Row nodes, starting again with `Label`:

### Labels
```rs
(Ast::Label(ast_lbl, value), Type::Label(ty_lbl, ty))
    if ast_lbl == ty_lbl => {
  self.check(env, *value, *ty)
}
```
Much like our `Int` and `Fun` cases, a `Label` node checks against a `Label` type.
Unlike those cases, a `Label` node only checks against a `Label` type if their labels are equal.
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
`Label` types are effectively singleton rows.
They act as a kind of transition between the type world and the row world.
Because they sit in this middle point they can act as either a Product or a Sum.
It's always unambiguous which one a `Label` is acting as, but until we see it in context we don't know which one it is.
Whenever a `Label` encounters the context that determines its type we cast it to that type and continue.

### Products
TODO: transition
```rs
(Ast::Concat(left, right), Type::Prod(goal_row)) => {
    let left_row = Row::Open(self.fresh_row_var());
    let right_row = Row::Open(self.fresh_row_var());

    // Check our left and right nodes are Prod types
    let left_out = self.check(env.clone(), *left, Type::Prod(left_row.clone()));
    let right_out = self.check(env, *right, Type::Prod(right_row.clone()));

    // Merge our subconstraints
    let mut constraints = left_out.constraints;
    constraints.extend(right_out.constraints);
    // Construct our row equation and add it to our constraints
    constraints.push(Constraint::RowConcat(RowEquation {
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
* Since we're checking, our expected `Prod` type acts as our goal row.
* We check that our `left` and `right` nodes are both `Prod` types.
* If they both are `Prod`s, we construct a row equation from our three rows and add it to our constraints
* TODO: transition to check `project`
```rs
(Ast::Project(dir, goal), Type::Prod(sub_row)) => {
  let goal_row = Row::Open(self.fresh_row_var());

  let (left, right) = match dir {
    Direction::Left => (sub_row, Row::Open(self.fresh_row_var())),
    Direction::Right => (Row::Open(self.fresh_row_var()), sub_row),
  };

  let mut out = self.check(env, *goal, Type::Prod(goal_row.clone()));
  out.constraints.push(Constraint::RowConcat(RowEquation {
    left,
    right,
    goal: goal_row,
  }));

  out.with_typed_ast(|ast| Ast::project(dir, ast))
}
```
* TODO: Explain check project

### Sums
* TODO: Transition to branch
```rs
(Ast::Branch(left_ast, right_ast), Type::Fun(arg_ty, ret_ty)) => {
  let mut constraints = vec![];
  let goal = match arg_ty.deref() {
    Type::Sum(goal) => goal.clone(),
    _ => {
      let goal = self.fresh_row_var();
      constraints
        .push(Constraint::TypeEqual(*arg_ty, Type::Sum(Row::Open(goal))));
      Row::Open(goal
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
  constraints.push(Constraint::RowConcat(RowEquation { left, right, goal }));

  InferOut {
    constraints,
    typed_ast: Ast::branch(left_out.typed_ast, right_out.typed_ast),
  }
}
```
* TODO: Explain branch
* TODO: Transition to inject
```rs
(Ast::Inject(dir, value), Type::Sum(goal)) => {
  let sub_row = self.fresh_row_var();
  let mut out = self.check(env, *value, Type::Sum(Row::Open(sub_row)));
  let row_eqn = match dir {
    Direction::Left => RowEquation {
      left: Row::Open(sub_row),
      right: Row::Open(self.fresh_row_var()),
      goal,
    },
    Direction::Right => RowEquation {
      left: Row::Open(self.fresh_row_var()),
      right: Row::Open(sub_row),
      goal,
    },
  };
  out.constraints.push(Constraint::RowConcat(row_eqn));
  out.with_typed_ast(|ast| Ast::inject(dir, ast))
}
```
* TODO: Explain inject
* That concludes our `infer` and `check` modifications. 
* Overall not too bad, damn near repetitive by the end.
* We can check rows in our sleep.

# Row Constraint Generation Example
Show how row equations are generated by our new AST nodes

# Row Unification
Define row unification as equation solving. Modify unification to do row solving.

* Our modifications to unification are just what we need to unify our new `RowConcat` constraint.
* While we only have one new constraint this turns out to require some extensive changes to our unification algorithm.
* The fundamental reason for this is that our constraints are not all syntactic now.
* Before we added rows we could compare terms for equality and if they were equal unify them, and if they weren't we could be confident no unification existed.
* This is no longer the case, consider two row constraints:
```
(x: Int) + (f: Bool, y: Int) = (f: Bool, x: Int, y: Int)
(f: Bool, y: Int) + (x: Int) = (f: Bool, x: Int, y: Int)
```
* These look different syntactically, but are actually the same!
* Because our row equations are commutative we should recognize these are the same equation
* That's easy in this simple example but things get a little trickier once we start introducing variables in equations
```
a0 + b = c
b + a1 = c
```
* If we squint, we actually have enough information to learn `a0 = a1` from this equation.
* We love learning! But how are we going to know when it's valid to equate these type variables and when two equations can't be unified?

* One final straw, our equations will generally always be solved to a concrete equation we can dispatch.
* But not always, like type variables, some equations might be left unsolved at the end of unification.

We begin in our top level `unification` function again:
```rs
pub(crate) fn unification(
    &mut self,
    constraints: Vec<Constraint>,
) -> Result<(), TypeError> {
    for constr in constraints {
        match constr {
            // ...
            Constraint::RowConcat(row_eqn) => self.unify_row_eqn(row_eqn)?,
        }
    }
    Ok(())
}
```
We've added a new case for our new constraint, that immediately calls `unify_row_eqn`:
```rs
fn unify_row_eqn(&mut self, row_eqn: RowEquation) -> Result<(), TypeError> {
    let left = self.normalize_row(row_eqn.left);
    let right = self.normalize_row(row_eqn.right);
    let goal = self.normalize_row(row_eqn.goal);
    match (left, right, goal) {
    }
}
```

`normalize_row` is similar to `normalize_ty`, except it walks a row instead of a type.
We're familiar with the idea from our `unify_ty_ty` method.
How we unify our equations depends on how many row variables they have:

- 0 variables - we combine our left and right rows and unify the combination with our goal row
- 1 variable - solve our 1 variable based on our two known rows, then unify our 3 known rows
- 2+ variables - we don't know enough to solve this equation, unify it against our partial equation set

Our first two cases are straightforward we have enough info to produce a solved row equation that we can unify.
Two row variables is trickier.
We don't have enough information to solve the row equation, but we might learn information that solves it later in unification.
So we save it in our partial set of equations for a later time when we've learned more about our rows.

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
TODO: Transition
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
TODO: Transition

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
Finally, if two closed rows meet we can decompose them (similar to if two function types unify).
We first have to check they have the same fields.
If not, there's no way we can unify them, and we can stop early with an error.
If their fields are equal, we can zip their types together and iterate over them unifying each pair.
Now that we know how to unify rows, let's get back to unifying row equations:

TODO: One variable cases
```rs
(Row::Open(var), Row::Closed(sub), Row::Closed(goal))
| (Row::Closed(sub), Row::Open(var), Row::Closed(goal)) => {
  let diff_row = self.diff_and_unify(goal, sub)?;
  self.unify_row_row(Row::Open(var), Row::Closed(diff_row))
}
```
If either left or right is our one variable we can handle it the same way.
Create a new row from the difference of our goal row and our sub closed row.
Unify our variable with the created row using our helper.

An important detail lives after the and in `diff_and_unify`. 
While calculating the difference between `goal` and `sub`, we also have to unify their types.
It's never happened to me, but I hear if you forget to do this your type checker silently gives you the wrong results, and you scratch your head at why for days.
Fortunately we read this article, so we're saved from that embarrassingly time-consuming mistake.
TODO: Transition


```rs
(left, right, goal) => {
    let new_eqn = RowEquation { left, right, goal };
    // Check if we've already seen an equation that we can unify against
    let poss_uni = self.partial_row_eqns.iter().find_map(|eqn| {
        if eqn.is_unifiable(&new_eqn) {
            Some(eqn.clone())
        //Row equations commute so we have to check for that possible unification
        } else if eqn.is_comm_unifiable(&new_eqn) {
            // We commute our equation so we unify the correct rows later
            Some(RowEquation {
                left: eqn.right.clone(),
                right: eqn.left.clone(),
                goal: eqn.goal.clone(),
            })
        } else {
            None
        }
    });

    match poss_uni {
        // Unify if we have a match
        Some(match_eqn) => {
            self.unify_row_row(new_eqn.left, match_eqn.left)?;
            self.unify_row_row(new_eqn.right, match_eqn.right)?;
            self.unify_row_row(new_eqn.goal, match_eqn.goal)?;
        }
        None => {
            // Otherwise, our equation is new
            // Add it to our list of partial equations
            self.partial_row_eqns.insert(new_eqn);
        }
    }
    Ok(())
}
```
This case is equations with 2+ variables, so we know the least about them.
We can't solve and unify them the way we could the other two cases.
Instead, we need to add them to our partial equation set until we learn enough to solve them.

Alas, we can't just insert them in our set and move on.
If we did, we'd miss another possible unification like we almost did with `diff_and_unify`.
First we have to check our existing equations and see if our new row equation can unify with any of them.
Just because we can't solve our row equation, doesn't mean we can't learn anything new from it.

Before adding the equation we look for unifiable equations already in our set.
How do we know when two equations are unifiable?
Two row equations are unifiable when two of its components are unifiable.
Since our components are rows, we know they're unifiable either when two open row variables are equal or two closed rows have equal fields.
This check is performed by helpers `is_unifiable` and `is_comm_unifiable`.
Row equations commute so we have to check if a commuted equation could unify as well.

If such an equation exists we don't have to add our row equation to our set at all.
Instead, we unify each component of our equation against the unifiable equation already in our set.
Only once we can't find a unifiable equation do we add our equation to our set.
This ensures we learn the most about our row variables from each equation, and keeps our partial equation set minimal.

We're now successfully solving row equations.
We have one last detail we brushed over earlier that we'll talk about now that we're older and wiser.
When a row variable unifies with a closed row we have a call to `dispatch_any_solved`.
Let's take a look at what `dispatch_any_solved` entails:

```rs
fn dispatch_any_solved(&mut self, var: RowVar, row: ClosedRow) -> Result<(), TypeError> {
    let mut changed_eqns = vec![];
    self.partial_row_eqns = std::mem::take(&mut self.partial_row_eqns)
        .into_iter()
        .filter_map(/* see below */)
        .collect();

    for row_eqn in changed_eqns {
        self.unify_row_eqn(row_eqn)?;
    }
    Ok(())
}
```

`dispatch_any_solved` is called whenever we solve a row variable to its closed row.
It iterates over all our partial equations looking for any that contain our variable.
If an equation contains our variable we remove it from the partial set, replace the variable by its solution and add it to `changed_eqns`
We can see how that's done in the callback passed to `filter_map`:

```rs
|eqn| match eqn {
    RowEquation { left, right, goal } if left == Row::Open(var) => {
        changed_eqns.push(RowEquation {
            left: Row::Closed(row.clone()),
            right,
            goal,
        });
        None
    }
    RowEquation { left, right, goal } if right == Row::Open(var) => {
        changed_eqns.push(RowEquation {
            left,
            right: Row::Closed(row.clone()),
            goal,
        });
        None
    }
    RowEquation { left, right, goal } if goal == Row::Open(var) => {
        changed_eqns.push(RowEquation {
            left,
            right,
            goal: Row::Closed(row.clone()),
        });
        None
    }
    eqn => Some(eqn),
}
```

After we've found all our changed equations we unify them with `unify_row_eqn`, and we're done.


# Row unification example
Walkthrough an example unification of constraints that shows how row equation solving works.


# Conclusion

TODO
