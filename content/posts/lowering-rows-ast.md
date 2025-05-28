+++
title = "The Heart of Lowered Rows"
date = "2025-02-26T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Lowering"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Lowering", "Elaboration", "IR", "Evidence Passing", "Witness", "Row Types", "Abstracting Extensible Datatypes"]
description = "Lowering row asts by generating evidence terms"
+++

{{< accessory title="Making a Language" >}}
This post is part of the [making a language series](/series/making-a-language).
A series that teaches you how to implement a programming language using Rust.

This post continues lowering rows from our [previous post](/posts/lowering-rows-ty).
{{</ accessory >}}

[Last time](/posts/lowering-rows-ty) we upgraded `lower_ty_scheme` to support rows and saw how we'd use it's evidence to inform `lower_ast`.
We're getting to the heart of lowering rows this time: generating and applying our evidence terms.

Recall we left off our with a partially updated `lower`:

```rs
fn lower(out: TypesOutput) -> (IR, Type) {
  let lowered_scheme = lower_ty_scheme(out.scheme);
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
  
  let mut lower_ast = LowerAst {
    supply,
    types: lowered_scheme.lower_types,
    ev_to_var,
    solved: vec![],
    row_to_ev: out.row_to_ev,
    branch_to_ret_ty: out.branch_to_ret_ty,
  };
  let ir = lower_ast.lower_ast(out.typed_ast);

  todo!()
}
```

Right on the cusp of lowering into `lower_ast`.
But first, we have some new fields in `LowerAst`. 
One we know (`ev_to_var`) and one we do not (`solved`).
We'll find out how `solved` is used as we update `lower_ast`.

{{< accessory title="Quick Refresher" >}}

We're going to be lowering our Row AST nodes, so let's remind ourselves what those nodes are.
We confront them in pairs, first is our `Label` and `Unlabel` nodes:

```rs
enum Ast<V> {
  // ... our base nodes
  // Label a node turning it into a singleton row
  Label(NodeId, Label, Box<Self>),
  // Unwrap a singleton row into it's underlying value
  Unlabel(NodeId, Box<Self>, Label),
  // ...
}
```

These nodes are responsible for transporting a normal AST into and out of a singleton row.
`Label` wraps an `Ast` turning it into a row using the accompanying `Label`.

A `Label`, the type not the node, is just an alias for a `String`:

```rs
type Label = String;
```

`Unlabel` compliments `Label`.
It takes a singleton row and unwraps it giving us back the value at the field specified.

After `Label` and `Unlabel`, we look at our nodes for product row types:

```rs
enum Ast<V> {
  // ...
  // Concat two products
  Concat(NodeId, Box<Self>, Box<Self>),
  // Project a product into a sub product
  Project(NodeId, Direction, Box<Self>),
  // ...
}
```

`Concat` takes two other product rows and merges them into a bigger product.
This node is how we build records.
Combined with `Label`, we can represent a record such as `{ x: 3, y: 42 }` with `Ast`:

```rs
Concat(
  ...,
  Label(..., "x", Int(3)),
  Label(..., "y", Int(42))
)
```

Conversely, `Project` destructs a record.
It takes in a larger record and returns a subset of that record.
Returning to `{ x: 3, y: 42 }`, `Project` could return any of `{ x: 3 }`, `{ y: 42 }`, `{}`, or even `{ x: 3, y: 42 }`.
We rely on type inference to determine the output of our `Project`.

Last, but not least, we have two nodes for sum row types.
Again one to introduce them and one to destruct them:

```rs
enum Ast<V> {
  // ...
  // Inject a value into a sum type
  Inject(NodeId, Direction, Box<Self>),
  // Branch on a sum type to two handler functions
  Branch(NodeId, Box<Self>, Box<Self>),
  // ...
}
```

`Inject` takes a row and turns it into a bigger sum type.
Like `Project`, it relies on type inference to determine the larger sum type.
An `Ast`:

```rs
Inject(Label("A", Int(2)))
```

could be given any number of Sum types:

  * `enum { A(Int) }`
  * `enum { A(Int), B(Int) }`
  * `enum { A(Int), B(Int), C(Int) }`
  * ...

`Inject`'s compliment `Branch` takes a Sum type apart.
`Branch` takes two functions that handle smaller sum types, and combines them into a function that takes a bigger sum type.
If we have a value of sum type `enum { X(Int), Y(Int) }`, we can branch on it with term:

```rs
Branch(
  ...,
  Fun(..., x, Unlabel(..., Var(x), "X")),
  Fun(..., y, Unlabel(..., Var(y), "Y")))
```

Our branch takes two functions.
They take `enum { X(Int) }` and `enum { Y(Int) }` as input respectively.
Both of them return an `Int`.
Branch combines them into a function from `enum { X(Int), Y(Int) }` to `Int`.

We keep metadata about our row nodes in two new outputs from type checking: `row_to_ev` and `branch_to_ret_ty`:

```rs
struct TypesOutput {
  row_to_ev: HashMap<NodeId, Evidence>,
  branch_to_ret_ty: HashMap<NodeId, Type>,
}
```

`row_to_ev` has an entry for each row node in our `Ast` and provides the row combination solved for that node during type checking.
`branch_to_ret_ty` is similar, but only has an entry for `Branch` nodes and provides the return type of each branch.

{{< /accessory >}}

## Onto lower_ast

`lower_ast` has the same usual shape:

```rs
impl LowerAst {
  fn lower_ast(
    &mut self, 
    ast: Ast<TypedVar>
  ) -> IR {
    match ast {
      // All our cases for the base AST...
    }
  }
}
```

We'll start by adding cases for `Label` and `Unlabel`:

```rs
Ast::Label(_, _, body) => self.lower_ast(*body),
Ast::Unlabel(_, body, _) => self.lower_ast(*body),
```

These are trivial.
Erase the labels and lower their bodies.
`Concat` is where the action starts:

```rs
Ast::Concat(id, left, right) => {
  let param = self
    .row_to_ev
    .get(&id)
    .cloned()
    .map(|ev| self.lookup_ev(ev))
    .expect("ICE: Concat AST node lacks an expected evidence");

  let concat = IR::field(IR::Var(param), 0);
  let left = self.lower_ast(*left);
  let right = self.lower_ast(*right);
  IR::app(IR::app(concat, left), right)
}
```

Recall that our `meta` field is an instance of `Option<ast::Evidence>`.
After type checking this field is always `Some`, so we can assume it's present.
We call `lookup_ev` on our `meta`, which returns a `Var`.
We'll come back to `lookup_ev` to lookup how it's implemented in a moment.

The `Var` from `lookup_ev` is a lowered evidence term, so it has the big product type we saw in `lower_ev_ty`.
We know the first field of that product type is the `concat` operation.
We extract `concat` by wrapping our variable in a field access of the `0` index.
To complete our lowering, we apply our lowered `left` and `right` to our evidence's `concat` term.

We've essentially erased the `Concat` node.
What was an explicit node of our `Ast` becomes a normal function call in our `IR`.
In doing so, we've both reduced the language we have to compile and made it easier to execute our rows.
I have no idea how to execute a `Concat` node, but computers have been executing functions since the Lisp machine.

### Looking up lookup_ev

Time to peek into `lookup_ev` to understand how we get a `Var` for our evidence:

```rs
770| fn lookup_ev(&mut self, ev: Evidence) -> Var {
...|   // Lookup the variable for our evidence.
835| }
```

Nothing surprising so far.
Hang on, did we always have those line numbers?
Why is it so many lines?
It's only a lookup function, isn't it?

`lookup_ev` begins by querying `ev_to_var` for `ev` to see if it already has a variable:

```rs
fn lookup_ev(&mut self, ev: Evidence) -> Var {
  self
    .ev_to_var
    .entry(ev)
    // ... more entry work
}
```

For our unsolved evidence, this is always the case thanks to the great work we did in `lower_ty_scheme`.
Solved evidence, however, does not appear in the type scheme, so it won't have a variable the first time we encounter it.
Even worse, because the evidence is solved, we have an obligation to generate an actual `IR` term for it, not just a variable.

We do this lazily in `lookup_ev`.
When we generate a term for a solved evidence, we bind it to a variable.
We save that variable in `ev_to_var`, so we can reuse it moving forward.
Rust's `Entry` API makes this easy:

```rs
self
  .ev_to_var
  .entry(ev)
  .or_insert_with_key(|ev| {
    // We can generate our evidence here 
    // and it gets cached for free.
  })
```

Inside `or_insert_with_key` we start by assuming our `ev` is solved. 
If it's not, it should've appeared in our scheme and already have a variable:

```rs
let Evidence::RowEquation {
  left: ast::Row::Closed(left),
  right: ast::Row::Closed(right),
  goal: ast::Row::Closed(goal),
} = ev
else {
  panic!("ICE: Unsolved evidence appeared in AST that wasn't in type scheme");
};
```

We'll also generate a fresh variable for our to be evidence term:

```rs
let param = self.supply.supply();
```

After that we have some metadata to calculate about our rows.
Lowered rows work with indices in place of labels.
We know that our `left` and `right` row combine to form `goal`. 
But we don't yet know which index of `left` (or `right`) maps to what index in `goal`.

We might, naively, assume that `goal` is all the `left` indices and then all the `right` indices: `[left[0], left[1], ..., right[0], right[1], ...]`.
This isn't the case because our rows are sorted lexicographically on their fields.
We aren't keeping the fields around, but before they go we use them to map our indices between `left`, `right`, and `goal`.
Our mapping from fields to indices is calculated by iterating over goal's `fields`:

```rs
let goal_indices = goal
  .fields
  .iter()
  .map(|field| {
    left
      .fields
      .binary_search(field)
      .map(RowIndex::Left)
      .or_else(|_| {
        right
          .fields
          .binary_search(field)
          .map(RowIndex::Right)
      })
      .expect("ICE: Invalid solved row combination.")
  })
  .collect::<Vec<_>>();
```

For each field in `goal`, we're going to find the corresponding field in either `left` or `right`.
We use a wrapper enum `RowIndex` to remember what we found:

```rs
enum RowIndex {
  Left(usize),
  Right(usize),
}
```

Because `fields` are sorted, we can use `.binary_search` to find the index of each field.
At the end, `goal_indices` is a `Vec<RowIndex>`, but we can really think of it more like a `Map<usize, RowIndex>`.
It maps each index of `goal` to its corresponding index in our sub Rows.
After that we lower the values of each of our rows:

```rs
let left_values = self.types.lower_closed_row_ty(left.clone());
let right_values = self.types.lower_closed_row_ty(right.clone());
let goal_values = self.types.lower_closed_row_ty(goal.clone());
```

All of these combine to form a new helper struct `LowerSolvedEv`:

```rs
let lower_solved_ev = LowerSolvedEv {
  supply: &mut self.supply,
  left: left_values,
  right: right_values,
  goal: goal_values,
  goal_indices,
};
```

It offers one important method `lower_ev_term`:

```rs 
let term = lower_solved_ev.lower_ev_term();
```

### One important method: lower_ev_term

`lower_ev_term` is the compliment to `lower_ev_ty`.
It generates an `IR` term for the evidence we used to construct `LowerSolvedEv`:

```rs
fn lower_ev_term(mut self) -> IR {
  IR::tuple([
    self.concat(),
    self.branch(),
    IR::tuple([self.prj_left(), self.inj_left()]),
    IR::tuple([self.prj_right(), self.inj_right()]),
  ])
}
```

Hopefully this looks familiar.
It almost exactly parallels the result of `lower_ev_ty` but in place of `Type`s we're creating `IR`s.

### Starting with concat

Let's look at how each operation gets generated starting with `concat`:

```rs
fn concat(&mut self) -> IR {
  let vars = self.make_vars([self.left_prod(), self.right_prod()]);
  todo!()
}
```

From `lower_ev_ty`, we know `concat` is a function taking `left` and `right` tuples and returning a `goal` tuple.
Our first step in making that function is to make some variables for our function parameters.
A slew of helper functions assist us in this humble task.
We'll take them one by one starting with `left_prod`.

`left_prod` creates a `Type::Prod` type from the `Type`s of `left`:

```rs
fn left_prod(&self) -> Type {
  Type::prod(Row::Closed(self.left.clone()))
}
```

It exists to save us keystrokes.
As you might guess, `right_prod` does the same but for `right`:

```rs
fn right_prod(&self) -> Type {
  Type::prod(Row::Closed(self.right.clone()))
}
```

I'll let you in on a little secret.
Before we're done with `lower_ev_term`, we're going to see `goal_prod`, `left_sum`, `right_sum`, and `goal_sum` as well.
Left as an exercise for the reader to figure out what they create.

We made quick work of those two.
The final helper on our list is `make_vars`.
Now, normally we try to keep code simple around here.
I'm a simple man.
If it wasn't for this blog's meteoric success, you'd find me in the GitHub issues making furniture [out of wood](https://github.com/docker/cli/issues/267#issuecomment-695149477).

Which is all to say, I don't show you the following code lightly:

```rs
fn make_vars<const N: usize>(&mut self, tys: [Type; N]) -> [Var; N] {
  tys.map(|ty| {
    let id = self.supply.supply();
    Var::new(id, ty)
  })
}
```

Not only does `make_vars` make vars, it also makes use of a more advanced rust feature: Const Generics.
We'll only touch on [Const Generics](https://practice.course.rs/generics-traits/const-generics.html) lightly here.
Read about them in more detail at that link if you're curious. 

All they're doing here is letting us take in an array of an arbitrary size `[Type; N]` and return an array of the same size `[Var; N]`.
We could write this implementation just as easily in terms of `Vec`.
We don't because you can pattern match an array, and can't pattern match a `Vec`.

On the implementation side, there isn't much to look at.
For each type we pass in, we generate a variable and return it.
With that we return to `concat` and generate our function:

```rs
IR::funs(vars.clone(), {
  todo!()
})
```

`IR::funs` is like `IR::fun` but it supports multiple variables.
We pass it our made `vars` to create a function of two parameters.
The body of that function will be a tuple created by concatenating our inputs.
Before we can construct the tuple, we need to figure out the element to put at each index:

```rs
let [left, right] = vars;
let mut elems = self.goal_indices.iter().map(|row_index| match row_index {
  RowIndex::Left(i) => unwrap_prj(*i, self.left.len(), left.clone()),
  RowIndex::Right(i) => unwrap_prj(*i, self.right.len(), right.clone()),
});
```

Our tuple represents the goal row, so we can figure out its elements by iterating over `goal_indices`.
At each index, we either access an element of our `left` variable or `right` variable depending on `row_index`.
`unwrap_prj` is another helper to create an `IR::Field` while handling an edge case around singleton rows:

```rs
fn unwrap_prj(
  index: usize, 
  len: usize, 
  prod: Var
) -> IR {
  unwrap_single(len, prod, |ir| IR::field(ir, index))
}
```

and `unwrap_single` is:

```rs
fn unwrap_single(
  len: usize, 
  var: Var, 
  else_fn: impl FnOnce(IR) -> IR
) -> IR {
  if len == 1 {
    IR::Var(var)
  } else {
    else_fn(IR::Var(var))
  }
}
```

Because we erase labels at this stage, a singleton row becomes indistinguishable from its underlying value.
An `Ast::Label("x", Ast::Int(3))` is lowered into `IR::Int(3)`.
This presents a problem for our projection.
If we try to access the field of our lowered singleton row it'll explode.

Fortunately, [the paper](https://homepage.cs.uiowa.edu/~jgmorrs/pubs/morris-popl2019-rows.pdf) has already foreseen this issue.
Whenever we access a row, we first check if it's a singleton.
If it is, we return the value directly instead of accessing it.
`unwrap_single` handles this logic for us, and `unwrap_prj` is a handy wrapper to always call `unwrap_single` with an `IR::field`.

So `elems` is an iterator over our field accesses (or values for singleton rows).
We're going to use that iterator to construct our final goal tuple:

```rs
if self.goal_indices.len() == 1 {
  elems.next().unwrap()
} else {
  IR::tuple(elems)
}
```
Except, we also have to check for singleton rows when we construct a tuple, not just when we index it.
If our goal is a singleton row, we won't bother constructing a tuple.
We return the single value directly.
Otherwise, we feed `elems` to `IR::tuple` to make a proper tuple.

At the end of all this, the IR generated for `concat` will look something like:

```rs
// We aren't super worried about the details.
// We just want a sense of the structure
let left = Var(...);
let right = Var(...);
IR::Fun(left, IR::Fun(right, IR::Tuple(vec![
  IR::Field(IR::Var(left), 0),
  IR::Field(IR::Var(right), 0),
  IR::Field(IR::Var(right), 1),
  IR::Field(IR::Var(left), 1)
])
```

We've made a custom spread operator tailored precisely to our two input rows `left` and `right`.

### Undertaking `branch` generation

Next on our list is `branch` which shares a high level shape with `concat` but has a more involved implementation.

```rs
fn branch(&mut self) -> IR {
  let left_sum = self.left_sum().shifted();
  let right_sum = self.right_sum().shifted();
  let goal_sum = self.goal_sum().shifted();
  let ret_ty = Type::Var(TypeVar(0));

  todo!()
}
```

First stop on `branch` is to prep the types we'll need.
We create `Sum` types for all of our rows.
Recall from `lower_ev_ty`, that `branch` introduces a type variable for our return type.
Because we introduce a `TyFun`, we have to `shift` our types that are nested within it.
We introduce the variables we'll need:

```rs
let vars = self.make_vars([
  Type::fun(left_sum.clone(), ret_ty.clone()),
  Type::fun(right_sum.clone(), ret_ty.clone()),
  goal_sum,
]);
```

Like `concat`, we can think of `branch` as taking two things and returning a third.
Unlike `concat`, those things are functions.
Because of this and our support for currying, there's no difference between returning a function and taking an extra parameter.
They're equivalent `IR` terms:

```rs
// Returning a function.
IR::Fun(left, IR::Fun(right, IR::Fun(goal, <body>)))
// Taking an extra parameter.
IR::Fun(left, IR::Fun(right, IR::Fun(goal, <body>)))
```

We'll think of `branch` as taking two parameters and returning a value, but our term will look like it takes three values and returns `ret_ty`.
With our `vars` at the ready, we can start laying down the structure of our term:

```rs
IR::ty_fun(
  Kind::Type,
  IR::funs(vars.clone(), {
    todo!()
  }),
)
```

We introduce the `ty_fun` to bind our return type.
We control how `branch` gets lowered, so we know we'll only ever apply a type to `branch`.
This makes it safe to assume our return type is `Kind::Type`.
In fact, lowering a `TypeScheme` provides the only opportunity to produce a variable of `Kind::Row`.

Ultimately, we're producing a function from `goal_sum` to `ret_ty`.
We don't have a lot of levers to pull to get a `ret_ty`.
We can either call `left`, a function from `left_sum` to `ret_ty`, or call `right`, a function from `right_sum` to `ret_ty`.
One impediment to calling either function is that our value of type `goal_sum` is not of type `left_sum` or `right_sum`

We resolve this using our new `Case` construct.
`branch` will case our `goal_sum` value.
In each `Branch` of this `Case` we'll wrap our `Branch`'s value up as either a `left_sum` or `right_sum` and call the relevant function.
Similar to `elems` from `concat`, each `Branch` will use `goal_indices` to determine if it should call `left` or `right`:

```rs
let [left_var, right_var, goal_var] = vars;
let goal_len = self.goal.len();
let mut branches = self
  .goal_indices
  .clone()
  .into_iter()
  .map(|row_index| {
    todo!()
  });
```
Because we're constructing a `Branch` (and not a `Field`), we need more metadata out of our `RowIndex`:

```rs
let (i, ty, len, var, sum) = match row_index {
  RowIndex::Left(i) => (
    i,
    self.left[i].clone().shifted(),
    self.left.len(),
    left_var.clone(),
    left_sum.clone(),
  ),
  RowIndex::Right(i) => (
    i,
    self.right[i].clone().shifted(),
    self.right.len(),
    right_var.clone(),
    right_sum.clone(),
  ),
};
```

We determine five things from `RowIndex`:
  * The index into `left` or `right`
  * The type at that index
  * The length of `left` or `right` (for unwrapping purposes)
  * The variable holding the function we need to call
  * The sum type of `left` or `right`

After collecting everything, we can build our `Branch`:

```rs
let [case_var] = self.make_vars([ty]);
IR::branch(case_var.clone(), {
  IR::app(
    IR::Var(var),
    unwrap_single(
      len, 
      case_var, 
      |ir| IR::tag(sum, i, ir)),
  )
})
```

The value from our `goal_sum` is bound to `case_var` in the branch.
Using `unwrap_single`, we wrap `case_var` as a new tagged value (handling our singleton edge case) using `sum`, `i`, and `len` from our `RowIndex` metadata.
The freshly minted `Tag` is then applied to the function that we selected in `var`.
Our `branches` iterator is used to construct the `Case` on our `goal_var`:

```rs
let mut branches = self
  .goal_indices
  .clone()
  .into_iter()
  .map(|row_index| {
    // ... excluded for space
  });
if goal_len == 1 {
  IR::app(branches.next().unwrap().as_fun(), IR::Var(goal_var))
} else {
  IR::case(ret_ty, IR::Var(goal_var), branches)
}
```

We have to consider the case when `goal` is a singleton here as well.
If it is a singleton, we can't construct a case.
Our singleton goal isn't a tagged value, it's just a value.
Instead, we turn our first, and only, branch into a function and pass `goal_var` to it directly.

We can be confident this will work, because if our goal is a singleton our sole branch must take an unwrapped value.
The only way for our goal to be a singleton is either:

  * `left` is a singleton and `right` is empty
  * `right` is a singleton and `left` is empty 

In both those cases our branch is working with a singleton `Sum` type.
When `goal` isn't a singleton, we construct a case normally using `goal_var` and `branches`.
At the end of all this we'll have generated a term like:

```rs
let left = Var(...);
let right = Var(...);
let goal = Var(...);
// Don't worry about the types here.
let case = Var(...);
IR::TyFun(Kind::Type,
  IR::Fun(left, IR::Fun(right, IR::Fun(goal,
    IR::Case(ret_ty, goal, vec![
      // Pretend branch is a tuple struct, so it's easier to write.
      Branch(case, 
        IR::App(IR::Var(left), IR::Tag(left_sum, 0, IR::Var(case)))),
      Branch(case, 
        IR::App(IR::Var(right), IR::Tag(right_sum, 0, IR::Var(case)))),
      Branch(case, 
        IR::App(IR::Var(right), IR::Tag(right_sum, 1, IR::Var(case)))),
      Branch(case, 
        IR::App(IR::Var(left), IR::Tag(left_sum, 1, IR::Var(case)))),
    ])))))
```

### Parading around `prj_left` and `prj_right`

Phew, that was a lot.
Please tell me our `prj` and `inj` terms will be simpler.
We test the waters with `prj_left`:

```rs
696| fn prj_left(&mut self) -> IR {
...|   todo!()
710| }
```

Huh, the line numbers are back.
They look kinder this time though.
You know maybe we can do this.
Let's see what's inside:

```rs { hl_lines=[2] }
let [goal] = self.make_vars([self.goal_prod()]);
let left_indices = self.left_indices();
IR::fun(goal.clone(), {
  if self.left.len() == 1 {
    unwrap_prj(left_indices[0], self.goal.len(), goal)
  } else {
    IR::tuple(
      left_indices
        .into_iter()
        .map(|i| unwrap_prj(i, self.goal.len(), goal.clone())),
    )
  }
})
```

Finally, there's nothing new -- wait have we seen `left_indices` before?  
... Dang.  

`prj_left` is a function that turns a `goal` tuple into a `left` tuple.
To accomplish this we have to know what indices of our `goal` tuple come from our `left` row, and where they end up in `goal`.

`goal_indices` maps each index of our goal row to its constituent's index.
`left_indices` does the inverse.
`left_indices` maps every index in our `left` row to its index in `goal`.
We can represent this without `RowIndex` because all our indices are in `goal`.

```rs
fn left_indices(&self) -> Vec<usize> {
  let mut left = self
    .goal_indices
    .iter()
    .enumerate()
    .filter_map(|(goal_index, row_index)| match row_index {
      RowIndex::Left(left_indx) => Some((*left_indx, goal_index)),
      _ => None,
    })
    .collect::<Vec<_>>();
  left.sort_by_key(|(key, _)| *key);
  left.into_iter().map(|(_, goal_index)| goal_index).collect()
}
```
The code walks over `goal_indices`, grabs every `RowIndex::Left` index, and then returns a vector of the goal indices sorted by the left indices.
If our `goal_indices` is: `vec![RowIndex::Left(0), RowIndex::Right(0), RowIndex::Left(2), RowIndex::Left(1)]`, our `left_indices` would be: `vec![0, 3, 2]`.
That's our only new helper though, with that out of the way we can talk about the heart of `prj_left`:

```rs
IR::fun(goal.clone(), {
  if self.left.len() == 1 {
    unwrap_prj(left_indices[0], self.goal.len(), goal)
  } else {
    IR::tuple(
      left_indices
        .into_iter()
        .map(|i| unwrap_prj(i, self.goal.len(), goal.clone())),
    )
  }
})
```

`prj_left`'s implementation shares a lot with `concat`.
It's a function of a single parameter mapping `goal` to `left`.
If `left` is a singleton, we don't build a tuple and return our first index into `goal`.
Otherwise, we construct a tuple from field accesses of our `goal` parameter.

It is with great relief that I tell you, `prj_right` has no more suprises:

```rs
fn prj_right(&mut self) -> IR {
  let [goal] = self.make_vars([self.goal_prod()]);
  let right_indices = self.right_indices();
  IR::fun(goal.clone(), {
    if self.right.len() == 1 {
      unwrap_prj(right_indices[0], self.goal.len(), goal)
    } else {
      IR::tuple(
        right_indices
          .into_iter()
          .map(|i| unwrap_prj(i, self.goal.len(), goal.clone())),
      )
    }
  })
}
```

We may not know precisely what `right_indices` is, but it's the compliment of `left_indices`.
We have a pretty good idea.
`right_indices` produces a `Vec` that maps indices of our `right` row to their index in `goal`.
Aside from that, `prj_right` looks just like `prj_left` but...on the right.

### Intuiting inj_left and inj_right

Rounding us out are `inj_left` and `inj_right`.
These functions take a `left` or `right` sum type and _inject_ it into the `goal` sum type.
We're working with sum types again but unlike branch, these are just sum types, not functions.
Even so, we'll still need `Case`s starting with `inj_left`:

```rs
fn inj_left(&mut self) -> IR {
  let [left_var] = self.make_vars([self.left_sum()]);
  IR::fun(left_var.clone(), {
    let branches = self
      .left_enumerated_values()
      .map(|(i, ty)| {
        let [branch_var] = self.make_vars([ty]);
        IR::branch(branch_var.clone(), {
          unwrap_single(self.goal.len(), branch_var, |ir| {
            IR::tag(self.goal_sum(), i, ir)
          })
        })
      })
      .collect::<Vec<_>>();
    if self.left.len() == 1 {
      IR::app(branches[0].as_fun(), IR::Var(left_var))
    } else {
      IR::case(self.goal_sum(), IR::Var(left_var), branches)
    }
  })
}
```

`inj_left` is a function taking in `left_sum` value.
We case match on that value and each branch of our case unwraps the tagged value and rewraps it as a `goal` tagged value.
Generating these branches requires not only the `left_indices` but also the type of the value at each index.
This is the job of `left_enumerated_values()`:

```rs
fn left_enumerated_values(
  &self
) -> impl Iterator<Item = (usize, Type)> {
  self.left_indices()
      .into_iter()
      .zip(self.left.clone())
}
```

The scariest part of this function is its return type.
All it does is zip `left_indices` together with our types `self.left`.
Correctly constructing the variable for our branch relies on that type:

```rs
.map(|(i, ty)| {
  let [branch_var] = self.make_vars([ty]);
  IR::branch(branch_var.clone(), {
    unwrap_single(self.goal.len(), branch_var, |ir| {
      IR::tag(self.goal_sum(), i, ir)
    })
  })
})
```

Because our output is another `Sum` type, all our branch body needs to do is construct a tagged value using the index `i`.
Capping us off, like `branch`, we handle a possible singleton row and construct our `Case`:

```rs
if self.left.len() == 1 {
  IR::app(branches[0].as_fun(), IR::Var(left_var))
} else {
  IR::case(self.goal_sum(), IR::Var(left_var), branches)
}
```

`inj_right` is the spitting image of `inj_left`:

```rs
fn inj_right(&mut self) -> IR {
  let [right_var] = self.make_vars([self.right_sum()]);
  IR::fun(right_var.clone(), {
    let branches = self
      .right_enumerated_values()
      .map(|(i, ty)| {
        let [branch_var] = self.make_vars([ty]);
        IR::branch(branch_var.clone(), {
          unwrap_single(self.goal.len(), branch_var, |ir| {
            IR::tag(self.goal_sum(), i, ir)
          })
        })
      })
      .collect::<Vec<_>>();
    if self.right.len() == 1 {
      IR::app(branches[0].as_fun(), IR::Var(right_var))
    } else {
      IR::case(self.goal_sum(), IR::Var(right_var), branches)
    }
  })
}
```

Again we'll rely on our powers of imagination to tell us what `right_enumerated_values` does.
With that we've completed all our evidence operations.
We now know how to lower evidence into a -- large -- `IR` term.

### Finishing up `lookup_ev`

After that long excursion, we return to `lookup_ev` triumphantly:

```rs

fn lookup_ev(&mut self, ev: Evidence) -> Var {
  self
    .ev_to_var
    .entry(ev)
    .or_insert_with_key(|ev| {
      // ...previous work
      let term = lower_solved_ev.lower_ev_term();

      todo!()
    })
}
```

Even better there's not much left to see:

```rs
let ty = self.types.lower_ev_ty(ev.clone());
let var = Var::new(param, ty);
self.solved.push((var.clone(), term));
var
```

The type of our evidence, paired with our `VarId` `param`, constructs a fresh variable `var`.
We save `var` with our generated `term` in our `solved` vector and return `var` as the entry for `ev_to_var`.
What a relief we won't have to do this all over again next time we see this evidence.

## Back in `lower_ast`

Popping the stack one more time, we look upon `lower_ast` with a new appreciation for all the work behind `lookup_ev`.
We'll pick back up with our next case `Branch`:

```rs
Ast::Branch(id, left, right) => {
  todo!()
}
```

Our branch has both evidence and a return type.
We start by grabbing its evidence.
Looking it up to get our term:

```rs
let param = self
  .row_to_ev
  .get(&id)
  .cloned()
  .map(|ev| self.lookup_ev(ev))
  .expect("ICE: Branch AST node lacks an expected evidence");
```

Next we get our return type and apply it to our evidence.

```rs
let ret_ty = self
  .branch_to_ret_ty
  .get(&id)
  .map(|ty| self.types.lower_ty(ty.clone()))
  .expect("ICE: Branch AST node lacks expected type");
let branch = IR::ty_app(IR::field(IR::Var(param), 1), TyApp::Ty(ret_ty));
```
Once we've type applied our `branch`, we lower `left` and `right` and pass them as arguments:

```rs
let left = self.lower_ast(*left);
let right = self.lower_ast(*right);
IR::app(IR::app(branch, left), right)
```

That's all for our `Branch` case.
Next up is `Project`:

```rs
Ast::Project(id, direction, body) => {
  let param = self
    .row_to_ev
    .get(&id)
    .cloned()
    .map(|ev| self.lookup_ev(ev))
    .expect("ICE: Project AST node lacks an expected evidence");

  let term = self.lower_ast(*body);
  let direction_field = match direction {
    ast::Direction::Left => 2,
    ast::Direction::Right => 3,
  };
  let prj_direction = 
    IR::field(
      IR::field(
        IR::Var(param), 
        direction_field), 
      0);
  IR::app(prj_direction, term)
}
```

Same idea as `concat` and `branch`.
The one hiccup is `Project` has a direction `Left` or `Right`.
Based on direction we need to get either the second or third element of our evidence term.
From there we always access index `0` to retrieve `prj`.

`Inject` is the same, but we retrieve index `1` instead of `0` at the end:

```rs
Ast::Inject(id, direction, body) => {
  let param = self
    .row_to_ev
    .get(&id)
    .cloned()
    .map(|ev| self.lookup_ev(ev))
    .expect("ICE: Inject AST node lacks an expected evidence");

  let term = self.lower_ast(*body);
  let direction_field = match direction {
    ast::Direction::Left => 2,
    ast::Direction::Right => 3,
  };
  let inj_direction = 
    IR::field(
      IR::field(
        IR::Var(param), 
        direction_field),
      1);
  IR::app(inj_direction, term)
}
```

## A hero's return

That's all our new cases in `lower_ast`.
We have one more stop in `lower` to polish off our additions, and then we're done.

```rs
fn lower(
  out: TypesOutput
) -> (IR, Type) {
  // ...Everything up to lower_ast
  let ir = lower_ast.lower_ast(ast);
  let solved_ir = lower_ast
    .solved
    .into_iter()
    .fold(ir, |ir, (var, solved)| IR::local(var, solved, ir));
}
```

Each evidence we solved during `lower_ast` gets bound as a local variable.
Each unsolved evidence gets turned into a function parameter:

```rs
let param_ir = params
  .into_iter()
  .rfold(solved_ir, |ir, var| IR::fun(var, ir));
```

Last, but not least, we wrap our term in its type functions:

```rs
let bound_ir = lowered_scheme
  .kinds
  .into_iter()
  .fold(param_ir, |ir, kind| IR::ty_fun(kind, ir));
```

We have to be more careful about this now that we have two `Kind`s.
`kinds` from `lowered_scheme` ensures we create our `ty_fun`s in the right order.

```rs
(bound_ir, lowered_scheme.scheme)
```

With that we bring our trilogy to a close.
Our rows are finally low.
Lowering base had me worried that `Ast` was going to copyright strike `IR`.
Lowering rows has assured me this is transformative content.
At this stage we really start to see how our `IR` diverges from `Ast` and is one abstraction level lower.
Next on our list is handling items, hopefully it goes better than our [previous brush](/posts/check-top-level-items/).
