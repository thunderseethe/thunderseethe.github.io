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
Our new nodes don't include a way to actually make a new row, only ways to build bigger and smaller rows out of existing rows.
Fortunately we still have 2 more nodes to cover that solve that exact problem:

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
    // ...
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
    // ...
}
```

We have a new kind of variable for rows, so we have a new unification table.
Our row variables will be solved to `ClosedRow`s.
Alongside this we have a set of partial row equations.
These are row equations we don't know enough about to solve, and will solve as we learn more row information during unification.
Okay enough bureaucracy let's look at our `infer` cases for rows, starting with our `Label` case:

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
    * Talk about direction? Defer to explainer
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
* Similar to our `Project` case
* Except we're injecting a smaller row into a bigger row, so goal is used in our output type instead of input type.

* And that's all our infer cases
* Each row case generates a row equation and checks its inputs against the row equation and creates its inferred output type from the row equation
  * We make much more use of check for these nodes then previously.
* Now we need to modify `check` to handler our new Row nodes, starting again with `Label`:
```rs
(Ast::Label(ast_lbl, value), Type::Label(ty_lbl, ty))
    if ast_lbl == ty_lbl => {
  self.check(env, *value, *ty)
}
```
Much like our `Int` and `Fun` cases, a `Label` node checks against a `Label` type.
Unlike those cases, a `Label` node only checks against `Label` case if their labels are equal.
Somewhat surprisingly, `Concat` and `Project` also check against a `Label` type:
```rs
(ast @ Ast::Concat(_, _), Type::Label(lbl, ty))
| (ast @ Ast::Project(_, _), Type::Label(lbl, ty)) => {
    // Cast a singleton row into a product
    self.check(env, ast, Type::Prod(Row::single(lbl, *ty)))
}
```
`Branch` and `Inject` can check against `Label` in the same way:
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
It's always unambiguous which one a `Label` is acting as, but until we see it context we don't know which one it is.
Whenever a `Label` encounters context that determines it's type we cast it to that type and continue.
TODO: transition
```rs
(Ast::Concat(left, right), Type::Prod(goal_row)) => {
    let left_row = Row::Open(self.fresh_row_var());
    let right_row = Row::Open(self.fresh_row_var());

    let left_out = self.check(env.clone(), *left, Type::Prod(left_row.clone()));
    let right_out = self.check(env, *right, Type::Prod(right_row.clone()));

    let mut constraints = left_out.constraints;
    constraints.extend(right_out.constraints);
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


# Row Constraint Generation Example
Show how row equations are generated by our new AST nodes

# Row Unification
Define row unification as equation solving. Modify unification to do row solving.

# Row unification example
Walkthrough an example unification of constraints that shows how row equation solving works.

