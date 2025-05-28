+++
title = "Lowering Row Types, Evidently"
date = "2025-02-11T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Lowering"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Lowering", "Elaboration", "IR", "Evidence Passing", "Witness", "Row Types", "Abstracting Extensible Datatypes"]
description = "Explaining how we'll lower row types into our IR"
+++

{{< accessory title="Making a Language" >}}
This post is part of the [making a language series](/series/making-a-language).
A series that teaches you how to implement a programming language using Rust.

This post undertakes lowering rows building on our [previous pass](/posts/row-types) that inferred row types.
It extends the [lowering](/posts/lowering-base-ir) we did for the base language.
{{</ accessory >}}

[Last time](/posts/lowering-base-ir), we lowered [types/base](https://github.com/thunderseethe/making-a-language/tree/main/types/base) into our IR.
That was exciting, but I professed some anxieties that all we had really done is swap an `Ast::` prefix for an `IR::` prefix.
Today, we're going to assuage those concerns by lowering [types/rows](https://github.com/thunderseethe/making-a-language/tree/main/types/rows).
A more seismic lowering.
Recall that [row types](/posts/row-types) provide data types for our language.
Rows are great in the surface language and the type checker but start to lose their luster once we need to represent them in memory.

A naive implementation of rows represents them as HashMaps or some kind of associated array at runtime.
This has the advantage of being very easy to compile, and the disadvantage of now your rows are hashmaps at runtime.
Were this a series on runtimes that might satisfy us, but this is a series on compiling.
There must be a way we can compile our way to a tighter memory representation.

Fortunately, the boundless wellspring of knowledge [Abstracting Extensible Datatypes](https://homepage.cs.uiowa.edu/~jgmorrs/pubs/morris-popl2019-rows.pdf) has a lot to say about lowering rows and nothing to say about runtime rows.
They lower rows into a System F based calculus -- hey that's just like us! -- but with some additions for data.
Not to say the prim `IR::Int` isn't data, but we're going to need some more help to handle rows.

On their recommendation, we add two new constructs to our `IR`: tuples and tagged values.
Coming from Rust, we're familiar with tuples.
If you've somehow made it six parts deep into this series without knowing Rust, my congratulations and condolences, you're a real one.
Have a [link](https://doc.rust-lang.org/rust-by-example/primitives/tuples.html) as a prize.

We've also seen tagged values in Rust but under a different name: `enum`.
A tagged value is a value stored alongside an integer that tags the value with an enum variant.
We can think of it like a tuple: `(tag, value)`.
Rust represents `enum`s as tagged values under the hood (barring [layout optimizations](https://doc.rust-lang.org/std/option/index.html#representation)).

Lowering rows is going to map our `Prod` and `Sum` rows of `Ast` onto the tuples and tagged values of `IR`.
The astute among us may have noticed tuples and tagged values don't look anything like rows.
Rows have fields and can be concatenated and projected, branched or injected. 
Tuples can't do any of that; they just kind of sit there...in order.

## Row Lowering Techniques

[The paper](https://homepage.cs.uiowa.edu/~jgmorrs/pubs/morris-popl2019-rows.pdf) is well aware that rows look nothing like tuples.
It employs two techniques to resolve the discrepancy during lowering: Evidence Passing and Label Erasure.
These two techniques allow us to convert rows into the simpler tuples/tagged values while maintaining the row operations we love.
At the end of lowering, we'll perform the same row operations (concat, branch, etc) but on our tuples and tagged values.

Our first technique, evidence passing, has a long tradition in the world of compiling.
Any language with a trait-like feature (Rust, Haskell, Swift, etc.) represents traits as evidence that are inevitably passed around.
If you've heard of [dictionary passing](https://okmij.org/ftp/Computation/typeclass.html#dict) from Haskell, this is evidence passing for type classes.
Swift even uses evidence passing for [memory management](https://faultlore.com/blah/swift-abi/#resilient-type-layout).
Each swift type has evidence for moving, cloning, and destructing itself that gets passed around.

With that pedigree, we're assured to be standing on strong foundations.
Evidence passing has been battled tested in real compilers.
We're simply yoinking and twisting it to work for rows.

The crux of evidence passing centers around unsolved constraints in the type checker.
As we saw while type checking rows, unsolved row constraints get turned into evidence during generalization and put in the type scheme.
Anything that calls our item is now responsible for solving that evidence and _passing_ it to us.
In the type checker, this is done implicitly with instantiation.
In lowering, it becomes more literal.

We create an actual `IR` term that represents each piece of evidence.
If our type checker solves a row combination, lowering is going to generate an `IR` term for that solved evidence.
Unsolved evidence in our type scheme is going to become an extra function parameter of the `IR` term `lower` produces.
Where the type checker instantiates and solves a constraint, lowering is going to generate an `IR` term and pass it as a function argument.

This mirrors our introduction of type functions in spirit.
When we lowered `base`, a big part of our shift was introducing term nodes to represent what was previously purely type information.
Evidence passing is a natural progression along this path.
Not only are we representing type information in our terms, our generated evidence terms affect our final `IR` term's behavior.

Our second technique, label erasure, is more modest but equally important.
Labels are critical to accurately type checking rows, but we no longer need them in lowering.
We'll remove the labels from all our rows and our `Ast`.
While we're erasing the labels themselves, we keep an important property they provide.
The order of values in our rows.
Labels order our rows, and we'll preserve that in lowering but using indices instead of labels.

After erasing labels, a row is just a vector of types.
This maps naturally onto our target data representation:

  * As a tuple, our vector represents the type of the field at each index.
  * As a tagged value, our vector represents the type of value at each tag.

Machines are great at numbers and awful at words, ignore machine words please, so this is great lowering.

## Bird's Eye View of our Work

Our newfound familiarity with our two tools, evidence passing and label erasure, allows us to paint the high level picture of row lowering.
During type checking, our Row AST nodes all save an evidence in themselves: 

  * `Concat`
  * `Branch` 
  * `Project`
  * `Inject`

That evidence is a row combination telling us how to combine left and right rows to form a goal row.
From that evidence, we're going to generate an `IR` term that implements the behavior of each `AST` node for the row combination.

When we encounter one of these AST nodes while lowering, we'll grab its generated operation out of the corresponding evidence term.
A call to the generated operation will replace our row node.
By the end of this process, we'll have erased all our Row AST nodes and be left with `IR` terms that work in terms of the generated operations.

That sounds great but is pretty abstract.
What does the generated term for evidence look like?
Each evidence needs to provide an implementation for our 4 row nodes.
We're already adding tuples to our `IR`, so we'll use a 4 field tuple to hold each operation.

Recall `Project` and `Inject` have a direction: `Left` or `Right`.
We'll account for this by having two versions of each in our generated term.
We'll nest our directed operations in their own tuples rather than flatten them out.
The overall layout for our evidence term will be:

```elm
( <concat impl>
, <branch impl>
, (<project left impl>, <inject left impl>)
, (<project right impl>, <inject right impl>)
)
```

Each `impl` within our term is an `IR` function that performs its row operation.
We're getting closer, but what do our implementation functions look like?
Let's walk through an example to clarify. 
We'll generate the term for evidence:

```rs
Evidence::RowEquation {
  left: Row::Closed(ClosedRow {
    fields: vec!["a", "d"],
    values: vec![Type::Int; 2],
  }), 
  right: Row::Closed(ClosedRow {
    fields: vec!["b", "c"],
    values: vec![Type::Int; 2],
  }), 
  goal: Row::Closed(ClosedRow {
    fields: vec!["a", "b", "c", "d"],
    values: vec![Type::Int; 4],
  }) 
}
```

The types aren't super important here, so we'll use `Type::Int` for everything.
What we want to focus on is how the fields guide the indices used in the generated term.

I'm not going to write out the `IR` instance itself.
It would be too long to be legible.
Instead, we'll use Rust syntax to convey the idea.
Our first generated operation is `concat`:

```rs
fn concat(
  left: (Int, Int), 
  right: (Int, Int)
) -> (Int, Int, Int, Int) {
  (left[0], right[0], right[1], left[1])
}
```
`concat` concatenates two product types, so we're working with tuples.
We take in a tuples representing our `left` and `right` rows and return a tuple representing `goal`.
The order of the fields in our goal tuple is important.

We might expect our output to be: `(left[0], left[1], right[0], right[1])`.
This would ignore the order of our fields.
We order our left and right index operations based on the fields in our evidence even though we erase them in the actual implementation.
Since left is `["a", "d"]`, it appears at the start and end of the tuple because that's where `a` and `d` appear in goal's `fields`.

Rust does not support the anonymous Sum types of our rows types.
We'll introduce some `enum`s for our example instead but remember they'll actually be anonymous Sum types:

```rs
enum Left {
  A(Int),
  D(Int)
}

enum Right {
  B(Int),
  C(Int),
}

enum Goal {
  A(Int),
  B(Int),
  C(Int),
  D(Int),
}
```

With those enums, we define our `branch` operation:

```rs
fn branch<T>(
  left: fn(Left) -> T, 
  right: fn(Right) -> T, 
) -> impl Fn(Goal) -> T {
  move |goal| match goal {
    Goal::A(x) => left(Left::A(x)),
    Goal::B(x) => right(Right::B(x)),
    Goal::C(x) => right(Right::C(x)),
    Goal::D(x) => left(Left::D(x)),
  }
}
```

`branch` is more involved than `concat` because rather than working on values directly it works on functions.
`branch` takes two functions `fn(Left) -> T` and `fn(Right) -> T` and combines them into a third function `fn(Goal) -> T`.
Returning our function requires a closure so in Rust we spell it `impl Fn(Goal) -> T`.
In our language this will be a normal `IR::Fun` node. 
We don't differentiate between closures and functions.

Because we don't know what type our `left` and `right` function return, we use a type variable to make sure all three functions return the same type, whatever it is.
From there, the implementation:

  * `match`es on our `Goal` enum.
  * Unwraps the enum in each branch and rewraps it as either `Left` or `Right`.
  * Dispatches to `left` or `right` with the rewrapped enum.

Just like `concat` our branches are ordered based on the order of fields found in our evidence.

Next, our project functions take a `goal` tuple and constructs either the `left` or `right` tuple.
`prj_left` is responsible for the `left` tuple:

```rs
fn prj_left(
  goal: (Int, Int, Int, Int)
) -> (Int, Int) {
  (goal[0], goal[3])
}
```

Again the fields guide us. 
Because left is `["a", "d"]`, we use the indexes of `a` and `d` in goal to build our tuple here.
We see something similar in `prj_right` which is responsible for the `right` tuple:

```rs
fn prj_right(
  goal: (Int, Int, Int, Int)
) -> (Int, Int) {
  (goal[1], goal[2])
}
```

Our inject functions are similar to `project` but work in the opposite direction.
They take a `Left` or `Right` enum and construct a `Goal` enum from it.
Unlike `branch`, they work directly on values.
`inj_left` takes our `Left` enum:

```rs
fn inj_left(left: Left) -> Goal {
  match left {
    Left::A(x) => Goal::A(x),
    Left::D(x) => Goal::D(x),
  }
}
```
Unsurprisingly, `inj_right` takes our `Right` enum:

```rs
fn inj_right(
  right: Right
) -> Goal {
  match right {
    Right::B(x) => Goal::B(x),
    Right::C(x) => Goal::C(x),
  }
}
```

Finally, our evidence term is all of those functions collected in a big tuple:

```rs
( concat
, branch
, (prj_left, inj_left)
, (prj_right, inj_right)
)
```

Again, we've used Rust syntax here, but the actual implementation will be generating terms of `IR` that fulfill this behavior.
They will be... _sizable_.
They have to be ready to handle any row operation we throw at them after all.
In practice, however, we'll only ever need one of these operations at a time.
When we're lowering a `Concat` node, we can grab our implementation out of the `0`th index of our term and throw away the rest.

## Row IR Nodes

We understand enough now to get into our implementation.
We'll start with the new nodes we need in our data types before moving onto modifying `lower`.
For `IR`, we're adding support for tuples and tagged values:

```rs
enum IR {
  // Our previous cases...
  // Create a product type.
  Tuple(Vec<Self>),
  // Select a field out of a product.
  Field(Box<Self>, usize),
  // Create a sum type.
  Tag(Type, usize, Box<Self>),
  // Case match on a sum.
  Case(Type, Box<Self>, Vec<Branch>),
}
```

We have nodes for constructing our new values: `Tuple` and `Tag`.
But we also need nodes for taking them apart: `Field` and `Case`.
In our example Rust code, we use `[0]` and `match`; these are `Field` and `Case` respectively.
Our `Case` does not have the same fancy pattern matching of Rust's `match`.
It just dispatches to a `Branch` based on the `tag` in its scrutinee.

A `Branch` is:

```rs
struct Branch {
  param: Var,
  body: IR,
}
```

This looks the same as a `Fun` node.
We use a different node, rather than `Case` having a list of `Fun`s, to make the optimizers job easier.
As a functional language, we're very interested in optimizing `Fun` nodes.
We do not want to accidentally apply any of those optimizations to `Branch`.

Another note, our `Tag` and `Case` both have a `Type` member.
For `Tag`, this `Type` remembers the full Sum type of our `Tag`.
Given a tagged value `IR::Tag(..., 0, IR::Int(343))` it could be a member of any number of Sum types.
But for any particular tagged value, the type checker figured out a specific Sum type.
We save that type in `IR::Tag`, so we can reconstruct types.

`Case` stores a `Type` to remember its return type.
It is not always obvious what the return type of `Case` will be.
For example, what is the return type of:

```rs
match an_enum {
}
```

With no branches to guide us it could be any type.
One might question our premise, but it will come up when we generate evidence that contains an empty row.
While this is a problem in general, the type checker will have figured out the return type for any particular `Case` (or we have a type error).
So an easy resolution is to save the return type from type checking in our `Case`.

## Row Type Nodes

After our additions to `IR`, we have additions to `Type`:

```rs
enum Type {
  // Our base types...
  Prod(Row),
  Sum(Row),
}
```

Products are the type of `IR::Tuple`.
Sums are the type of `IR::Tag`.
After the four nodes we added to `IR`, two feels like a bargain.
That is until we crack open `Row`:

```rs
enum Row {
  Open(TypeVar),
  Closed(Vec<Type>),
}
```

Huh, it feels like we just added four new types with extra steps.
Either way, since we're erasing labels a `Closed` row becomes a `Vec<Type>`.
Instead of the separate `RowVar` used during type checking, open rows are represented using `TypeVar` in our `IR`.
We differentiate a type `TypeVar` from a row `TypeVar` using our new `Kind`:

```rs
enum Kind {
  Type,
  Row,
}
```

Now we can mess up our kinds if we're not careful. Hooray!
As a consequence of `Row` and its new `Kind`, we have to make a small addendum to `TyApp` in `IR`:

```rs
enum IR {
  // ...
  TyApp(Box<Self>, TyApp),
  // ...
}
```

Instead of taking a `Type`, it now takes a `TyApp`:

```rs
enum TyApp {
  Ty(Type),
  Row(Row),
}
```

`TyApp` exists solely for passing either a `Type` or a `Row` to `TyApp`.
The ability to apply a `Row` to a type function opens up a new possibility: substituting a `Row` for a `TypeVar`.
We'll modify our `subst` function to support `Row` [behind the camera](https://github.com/thunderseethe/making-a-language/tree/main/lowering/rows).
While we're taking care of bookkeeping, we need to update `type_of` to handle our new additions.
Now that we're familiar with `type_of`, these updates are not particularly interesting. 
You can find them in the [full code](https://github.com/thunderseethe/making-a-language/tree/main/lowering/rows).

We've covered a lot of ground.
We know what we have to do, and we've prepared our `IR` and `Type` for the rows ahead.
But we still have a lot to do.
[Next time](/posts/lowering-rows-ty), we'll use our additions to lower evidence in our type scheme.
