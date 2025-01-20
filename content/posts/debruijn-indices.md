+++
title = "Picking Equatable Names"
date = "2025-01-15T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages"]
series = []
keywords = ["DeBruijn Index", "Alpha Equivalence", "Naming", "Bound Variables", "Substitution"]
description = "An explanation of DeBruijn Indices"
+++

Naming things is hard.
It's one of two hard problems in computer science, alongside cache invalidation and off by one errors.
Humans struggle to come up with good names.
Perhaps more surprisingly, computers are also stumped by this task.

A reasonable precursor to ask, why are computers naming things at all?
Naming things is vital to programming languages and compilers.
When we represent programs in memory, as an AST for example, we need names for the bound variables of functions.
Some of these names will be given to us by the user when they write the syntax of the function:
```rs
fn a_lil_fun(a_name: String, a_great_name: usize) {
 // do something cool with our names.
}
```
Unfortunately, it's rare the user gives us as many names as the compiler needs.
As our function is compiled towards machine code, the compiler will introduce new local variables or even whole new parameters.
At this point, the need for names comes knocking.

Hopefully, you believe me that compilers are interested in naming.
Now let's understand why naming is hard.
Our headaches emanate from two properties we want in our AST: Equality and Substitution.

Let's work with a concrete AST to make our properties more clear:
```rs
struct Ast {
  Var(String),
  Fun(String, Box<Self>),
  App(Box<Self>, Box<Self>),
}
```
Our compiler wants to be able to tell when two instances of `Ast` are equal.
If we can, we'd like the compiler to use `#[derive(Eq)]` for `Ast`.
This is not only to save on typing time, but also because it uses a fast tree traversal to determine equality.

Equating ASTs isn't strictly required for compilation.
You'll find, however, that all nontrivial compiler that allow for equating ASTs.
This is because it's too useful for interning and other practical concerns that come with compilation.

The other property we care about is Substitution.
Substitution is the process of inlining function parameters into the function body.
It's important for optimization and lowering of our AST into IRs later in the compiler.
For example, given the AST:

```rs
Ast::app(
  Ast::app(
    Ast::fun("fst", Ast::fun("snd", Ast::Var("fst"))),
    Ast::Var("a"),
  ),
  Ast::Var("b"))
```

Okay actually that's pretty hard to read, especially on my phone screen.
We don't actually need rust syntax here, so let's use a shorter syntax that makes it easier to see what we care about:

```
x // variables
\x. x // functions
M N // application
```
Where `M` and `N` in application stand for arbitrary lambda calculus terms.
We'll allow a little bit of syntax sugar for functions of multiple parameter.
We write `\x y . x` as shorthand for the term `\x. \y. x`.
Our substitution example is now:

```
(\fst snd. fst) a b
```

We'd like to substitute `a` for `fst` and `b` for `snd` to get the AST:

```
a
```

If we're contrived enough, substitution can save us a lot of work.

We understand what's at stake now: all of compilation...probably forever.
How do names hinder our ability to equate and substitute ASTs?
Consider two similar AST terms:

```
\x. x
\y. y
```
I have to say similar, and not equal, because if we chuck these into our `==` implementation today it returns `false`.
We can use simple pattern matching to tell that if we replace `x` in `foo` by `y`:

```
\y. y
\y. y
```
Considering terms equal regardless of names like this is called [alpha equivalence](TODO)
We can see this, but think of the poor computer.
It's not able to look at the two terms and assess that's the case.
At a minimum, it lacks eyes.

`foo` and `bar` look different despite behaving the same way.
As functions, we don't actually care what their parameters are called.
If they behave the same when we apply them to arguments, we consider them equal.
Names, however, get in the way of our humble computer seeing this.

Names also present a barrier to substitution.
It's easy to substitute a term and change it's meaning incidentally.
Consider the term:

```
(\y. (\x y. x y) y)
```
Naively substituting gives us the term:

```
(\y. (\y. y y))
```
This is a different term.
Our `y` that was previously only referenced once is now referenced twice.
We've changed the meaning of our term.

The root of the problem is our representation makes it hard to see how variables are used.
The names are great for legibility but hinder equity.
We're interested in the structure of the tree.
Where and how are variables used.
Can we pick names that highlight this structure rather than obscure it?

To the great relief of this article, we can.
We can think of our functions as forming an array of variables:
`\[x, y, z]. x z (y z)`
Our function binds 3 variables, and we can think of it as an array of 3 bound variables.
This works even when our functions aren't right next to each other:
`\[x, y]. (\[z]. x z) (\[w]. y w)`
We keep track of the running array of names and any time we encounter a function append its variables to the array.
In `x z` our array of variables is `[x, y, z]` but in `y w` it's `[x, y, w]`.

Once we think of functions as forming an array like this its just one more small step.
Replace each use of a variable by its index into the array.
Instead of `\[x]. x` we write `\[x]. 0`.
Instead of `\[x, y]. (\[z]. x z) (\[w]. y w))` we write `\[x, y]. (\[z]. 0 2) (\[w]. 1 2)`
We'll include the array of names to help you and me out, but we could just as easily omit it and write `\.0` or `\.\. (\. 0 2) (\. 1 2)`

This solves our equation dilemma.
`(\[x]. 0) == \([y]. 0)`
We can safely ignore what names are in the array and the computer can immediately see these are the same term.

As we saw with our `\[x, y]. (\[z]. x z) (\[w]. y w)` term, we might have multiple arrays of names.
Whenever an application causes us to branch we have the opportunity to introduce multiple arrays.
This leads to the ability for the same index to refer to different names.
A reasonable person might worry this lands us in the same predicament.

Part of our problem with names stemmed from different variables using the same name.
Fortunately, the same problems can't arise with our use of indices.
It's always clear whether `2` is `z` or `w` based on where our variable apperas in our term.
Capturing the structure of the term with indices like this is called DeBruijn Levels.

DeBruijn Levels walk the term top to bottom building our array of names and use that to assign an index to each variable.
As we saw, this makes for easy equality.
Let's look at substitution and see if it's also deftly dispatched by DeBruijn Levels.
Our conveniently selected example:
`(\[x, y]. 0 (\[z]. 1 2)) (\[w]. 0)`

We can substitute `(\[w]. 0)` for `x`, and why wouldn't we want to?

`\[y]. (\[w]. 0) (\[z]. 1 2)`

It seemed like a good idea at the time, but we've really screwed things up now, haven't we?
Literally every index in this term is now wrong.
`2` gives us an out-of-bounds exception now.
That's not the kinds of bounds we're supposed to be talking about today.

We can recover this though.
All we have to do is adjust our indices based on their new variable arrays:

`\[y]. (\[w]. 1) (\[z]. 0 1)`

That gets us back on course, but we had to adjust every index in our term.
I was planning on substituting a lot.
Seems like quite the hassle to go through every time.

de Bruijn certainly thought so, and had a good idea how to reduce the work of substituting.
DeBruijn Levels have an even harder to read compliment: DeBruijn Indices.
DeBruijn Levels come from constructing our array of names top to bottom and indexing into it.
DeBruijn Indices do the reverse.
They build a stack of names bottom to top.

We can think of this as reversing our array and then assigning indices to our variables.
Take our term `\[x, y]. (\[z]. 0 2) (\[w]. 1 2)` with it's two name arrays: `[x, y, z]` and `[x, y, w]`.
To convert to DeBruijn Indices we reverse these arrays: `[z, y, x]` and `[w, y, x]`.
Then adjust our indices to use the new order: `\[x, y]. (\[z]. 2 0) (\[w]. 1 0)`
Let's do a couple more examples to really get a handle on it:

* `\[x]. 0` → `\[x]. 0` (reversing a single element array doesn't change much)
* `\[x, y]. 0` → `\[x, y]. 1`
* `\[x, y, z]. 0 2 (1 2)` → `\[x, y, z]. 2 0 (1 0)`

I've done enough leetcode to know how to reverse a list.
Let's revisit our substitution example to see how DeBruijn Indices fare better:

`(\[x, y]. 1 (\[z]. 1 0)) (\[w]. 0)`

Again we substitute `\[w]. 0` for `x`:

`\[y]. (\[w]. 0) (\[z]. 1 0)`

It...it just worked.
We have no indices to adjust.
I mean I wrote the example, I knew it'd be easier, but not totally devoid of work.

With a little bit of thought this makes some sense though.
DeBruijn Levels start indexing from the "top" of the tree, so when we substitute it reveberts across the entire tree because our top has changed.
DeBruijn Indices start indexing from the "bottom" of the tree, when substitution changes the top of the tree it pops the top element off the stack but leaves the other indices unperturbed.

Another way to view this difference is that DeBruijn Indices make it easy to work with bound variables.
Conversely, DeBruijn Levels make it easy to work with free variables.
They both have their applications based on these strengths.
But for our purposes, dealing mostly with bound variables, DeBruijn Indices shine brighter.

Except for the legibility issues.
DeBruijn Indices solve both our equality and substitution issues, but I can't make heads or tails of looking at them.
In practice a lot can be done to solve this.
Our examples have included lists of names for reference: `\[x, y, z]. 2 0 (1 0)`
A real implementation of DeBruijn Indices can use a similar tactic.

Save names in our binders but only for printing purposes.
Internally we use the DeBruijn Indices that make it easy to equate and substitute.
Once we want to print a human-readable version, we use the list of stored names to replace all our indices by their names.
This approach recovers legibility for humans achieving the best of both worlds.
