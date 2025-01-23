+++
title = "Picking Equatable Names"
date = "2025-01-23T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages"]
series = []
keywords = ["DeBruijn Index", "Alpha Equivalence", "Naming", "Bound Variables", "Substitution"]
description = "An explanation of DeBruijn Indices"
+++

Naming things is hard.
It's one of two hard problems in computer science, alongside cache invalidation and off by one errors.
Humans struggle to come up with good names.
Personally, I can never remember if it's `AbstractProducerFactory` or `AbstractFactoryProducer`.

Perhaps more surprisingly, computers are also stumped by this task.
Compilers often need to come up with names and find themselves at a loss just like you and me.
Sure, some of the names will be given to the compiler by the user writing code:
```rs
fn a_lil_fun(a_name: String, a_great_name: usize) {
 // do something cool with our names.
}
```
Unfortunately, it's rare the user gives us as many names as the compiler needs.
As our function is compiled towards machine code, the compiler will introduce new local variables or even whole new function parameters.
Each of which deserves a name, so the compiler knows how to find it later.
Inevitably, the need for names comes knocking.

## We want Names

Okay, so compilers want names, but why is it hard for them to come up with them?
We have to look to how compilers represent programs in memory to understand their naming dilemma.
Let's set up a simple AST with named variables:
```rs
struct Ast {
  Var(String), // variables
  Fun(String, Box<Self>), // function
  App(Box<Self>, Box<Self>), // application
}
```
This is a really simple AST to make it easy to see the problem.
The same issues arise in a real AST with more nodes and binding constructs.

Our headaches emanate from two properties we want in our AST: Equality and Substitution.
Our compiler wants to be able to tell when two instances of `Ast` are equal.
Even better if the compiler can use `#[derive(Eq)]` for `Ast`.
This is not only to save on typing time, but also to use a fast tree traversal to determine equality.

Equating ASTs isn't strictly required for compilation.
You'll find, however, that all nontrivial compiler equate ASTs.
This is because it's too useful for interning and optimization passes, especially for functional languages where closure conversion is common.

## Inlining Names

The other property we care about is Substitution.
Substitution is the process of inlining function parameters into the function body.
Inlining is the artery of a compiler, carrying code cells to their various callsites opening up new optimization opportunities.
If naming makes inlining hard, our compiler will get a blood clot and produce bad code, or even worse die.
Here's an example of inlining gone right:

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

```js
x // variables
\x. M // functions
M N // application
```
Where `M` and `N` in stand for arbitrary ASTs, think of them like `Box<Self>`.
We'll employ a bit of syntax sugar for functions of multiple parameter.
We write `\x y . x` as shorthand for the AST `\x. \y. x`.
Our substitution example is now much easier to see:

```js
(\fst snd. fst) a b
```

We'd like to inline `a` for `fst` and `b` for `snd` to get the AST:

```js
a
```

I'd much rather generate machine code for that AST than the former.

## More Names, More Problems

We understand what's at stake now: all of compilation..._probably forever_.
But what's the actual problem?
How do names hinder our ability to equate and substitute ASTs?
Consider two similar AST terms:

```js
\x. x
\y. y
```
I have to say similar, and not equal, because if we chuck these into our `==` implementation today it returns `false`.
This is our first problem.
You and I can use simple pattern matching to tell that if we replace `x` in `foo` by `y`:

```js
\y. y
\y. y
```
Our terms are equal, and who really cares if we name our variable `x` or `y`.
A variable by any other name would smell as...  

Either way, I trust you to know how variables smell.
Considering terms equal regardless of names like this is called [alpha equivalence](TODO)
We can see this, but think of the poor computer.
It's not able to look at the two terms and assess that's the case.
At a minimum, it lacks eyes with which to pattern match.

Crucially, `foo` and `bar` look different despite behaving the same way.
As functions, we don't actually care what their parameters are called.
If they behave the same when we apply them to arguments, we consider them equal.
Names, however, get in the way of our humble computer seeing this.

## Inlining Issues

On the inlining side, names also present a barrier.
Simply picking a bad name causes inlining to change the meaning of an AST.
Let's revisit our previous substitution AST with one innocent name change:

```js
(\fst snd. fst) snd b
```

Naively substituting `fst` gives us the AST:

```js
(\snd. snd) b
```

Those `snd`s should be different variables, but we've lost that with our inlining. 
Affairs grow more dire as we inline `b`:

```js
b
```

That's an entirely different AST from our original result: `a`.
Names should just help us identify which variables we're talking about.
But here they've changed the meaning of our AST.

With our superior pattern matching faculties we can quickly ascertain the problem.
We changed `a` to `snd` which was a name that was already in use.
The compiler, however, struggles to see the same issue.
Especially on the much larger ASTs that represent real programs.

It's possible to track what names are already in use, and generate unique names for every variable.
But in practice this can be quite expensive.
Even if you succeed, introducing new names hinders equality.
If all ASTs use unique names for every variable, ASTs are by definition never equal, one step forward two steps back.

The root of the problem is our AST representation makes it hard to see how variables are used.
The names are great for legibility but hinder equity.
Rather than getting caught up on the names, we'd rather focus on the structure of the AST.
How many times is a variable used, and where do those variable occurrences show up?
If we can pick names that highlight this structure, our names will work for us instead of against us.

## Picking Better Names

To the great relief of this article, we can.
The key insight is to think of our functions as forming an array of names for its bound variables.
Given the AST:
```js
\x y z. x z (y z)
```
We can imagine our `Fun` node maintains an array of names:
```js
\[x, y, z]. x z (y z)
```
Our function, really syntax sugar for 3 functions, binds 3 variables, and we can think of that as an array of 3 names.
This works even when our functions are nested:
`\[x, y]. (\[z]. x z) (\[w]. y w)`
We keep track of the running array of names and any time we encounter a function append its variables to the array.
In `x z` our array of variables is `[x, y, z]` and in `y w` it's `[x, y, w]`.
Conceptually, we're building up this array of names as we traverse our AST top to bottom.

Arrays allow for an important feature previously missing: indexing.
Instead of variables using their names, we'll represent each variable by its index into the array.
For example, the AST:
```js
\[x]. x
```
becomes:
```js
\[x]. 0
  ▲   │
  └───┘
```

A slightly more complicated AST:
```js
\[x, y]. y x 
```
becomes:
```js
     ┌───┐  
     ▼   │  
\[x, y]. 1 0
  ▲        │
  └────────┘
```

Finally, our most complicated AST:
```js
\[x, y]. (\[z]. x z) (\[w]. y w))
```
becomes: 
```js
            ┌─────┐     ┌─────┐ 
            ▼     │     ▼     │ 
\[x, y]. (\[z]. 0 2) (\[w]. 1 2)
  ▲  ▲          │           │   
  └──┼──────────┘           │   
     └──────────────────────┘
```

When we look at `0 2`, we remember our name array is `[x, y, z]` and know that `0` is `x` and `2` is `z`.
We'll include the array of names to help you and me out, but we could omit it and write `\.0` or `\.\. (\. 0 2) (\. 1 2)` without losing any structure.

## Achieving Equality for All

Systematically selecting indices is our salvation for equality:
```js
\[x]. 0 == \[y]. 0
```
Recall, our names are just their for debugging.
When the compiler compares our ASTs for equality all it sees is: `\.0 == \.0`.
Which it can tell is true without eyes or pattern matching.
This also works for the converse.
Indices make it easy to see when two ASTs are not equal:

```js
\[x, y]. 0 != \[x, y]. 1
```

As we saw with our `\[x, y]. (\[z]. x z) (\[w]. y w)` AST, we might have multiple arrays of names.
Whenever an application causes our AST to branch, the opportunity to introduce multiple arrays arises.
This leads to the ability for the same index to refer to different names.
A reasonable person might worry this lands us in the same predicament as before.

Part of our problem with names stemmed from different variables using the same name.
Fortunately, the same problems can't arise with our use of indices.
It's always clear from the structure of our AST whether `2` refers to `z` or `w`.
Capturing the structure of the AST with indices like this is called: DeBruijn Levels.


## Trouble in Paradise

DeBruijn Levels walk the AST top to bottom building our array of names and use that to assign an index to each variable.
As we saw, this makes for easy equality.
If it also makes for easy substitution, we'll have found the perfect representation.
A conveniently selected example will test our hypothesis:

```js
      ┌────────────┐              
      ▼            │              
(\[x, y]. 0 (\[z]. 1 2)) (\[w]. 0)
   ▲      │    ▲     │      ▲   │ 
   └──────┘    └─────┘      └───┘
```

We can substitute `(\[w]. 0)` for `x`:

```js
               ???◄──────┐ 
                         │ 
\[y]. (\[w]. 0) (\[z]. 1 2)
  ▲          │     ▲   │   
  └──────────┘     └───┘
```

It seemed like a good idea at the time, but we've really screwed things up now, haven't we?
Literally every index in this AST is now wrong.
The name array at `1 2` is `[y, z]`.
`2` gives us an out-of-bounds exception now.
That's not the kinds of bounds we're supposed to be talking about today.

We can recover this though.
All we have to do is adjust our indices based on their new variable arrays:

```js
  ┌────────────────────┐   
  ▼                    │   
\[y]. (\[w]. 1) (\[z]. 0 1)
         ▲   │     ▲     │ 
         └───┘     └─────┘
```

That gets us back on course, but we had to update every index in our AST.
I was planning on inlining a lot.
Modifying every index every time seems like quite the hassle.

## Turning around our Troubles

de Bruijn certainly thought so, and had an idea how to fix it.
DeBruijn Levels have an even harder to read compliment: DeBruijn Indices.
Levels come from constructing our array of names top to bottom and indexing into it.
DeBruijn Indices do the reverse.
They build a stack of names bottom to top and index into it.

We can think of this as reversing our array before assigning indices to our variables.
Take our DeBruijn Level AST: 

```js
            ┌─────┐     ┌─────┐ 
            ▼     │     ▼     │ 
\[x, y]. (\[z]. 0 2) (\[w]. 1 2)
  ▲  ▲          │           │   
  └──┼──────────┘           │   
     └──────────────────────┘
```

Recall, it's two name arrays are `[x, y, z]` and `[x, y, w]`.
To convert to DeBruijn Indices we first reverse these arrays: `[z, y, x]` and `[w, y, x]`.
Then adjust our indices to use the new order:

```js
            ┌─────┐     ┌─────┐ 
            ▼     │     ▼     │ 
\[x, y]. (\[z]. 2 0) (\[w]. 1 0)
  ▲  ▲          │           │   
  └──┼──────────┘           │   
     └──────────────────────┘
```

Let's do a couple more examples to really get a handle on it. The AST:
```js
\[x]. 0
  ▲   │
  └───┘
```
becomes:
```js
\[x]. 0
  ▲   │
  └───┘
```
It doesn't take a lot to reverse a single element array.
A more interesting AST:
```js
\[x, y]. 0
  ▲      │
  └──────┘
```
becomes:
```js
\[x, y]. 1
  ▲      │
  └──────┘
```

One more AST to really flex our indexing:
```js
  ┌─────────┐        
  │  ┌──────┼────┐   
  ▼  ▼      │    │   
\[x, y, z]. 0 2 (1 2)
        ▲     │    │ 
        └─────┴────┘
```
becomes:
```js
  ┌─────────┐        
  │  ┌──────┼────┐   
  ▼  ▼      │    │   
\[x, y, z]. 2 0 (1 0)
        ▲     │    │ 
        └─────┴────┘
```

I've done enough leetcode to know how to reverse an array.
If I could just figure out how to invert a binary tree, I could get hired.
Equipped with our new DeBruijn Indices, our previous inlining example is now trivial:

```js
      ┌────────────┐              
      ▼            │              
(\[x, y]. 1 (\[z]. 1 0)) (\[w]. 0)
   ▲      │    ▲     │      ▲   │ 
   └──────┘    └─────┘      └───┘
```

Again we substitute `\[w]. 0` for `x`:

```js
  ┌────────────────────┐   
  ▼                    │   
\[y]. (\[w]. 0) (\[z]. 1 0)
         ▲   │     ▲     │ 
         └───┘     └─────┘
```

Every index is correct.
We have no indices to adjust.
I mean I wrote the example, I knew it'd be easier, but not totally devoid of work.
DeBruijn was really onto something.

## Pick the Right Name for the Job

With a little bit of thought this makes sense.
DeBruijn Levels start indexing from the "top" of the tree, so when we substitute it reverberts across the entire tree because the top of the tree has changed.
DeBruijn Indices start indexing from the "bottom" of the tree, when substitution changes the top of the tree it pops the top element off the stack but leaves the other indices unperturbed.
Another way to look at is what's happening to the name arrays.
It's really easy to pop an element off the end of an array, but much more work to remove the first element of said array.

DeBruijn Indices make it easy to work with bound variables.
Conversely, DeBruijn Levels make it easy to work with free variables.
They both have their applications based on these strengths.
But for our purposes, dealing mostly with bound variables, DeBruijn Indices shine brighter.

Except for the legibility issues.
DeBruijn Indices solve both our equality and substitution issues, but I can't make heads or tails of looking at them.
In practice a lot can be done to ease this.
Our examples have included lists of names for reference: `\[x, y, z]. 2 0 (1 0)`
A real implementation of DeBruijn Indices can use a similar tactic.

Save names in our functions, but only for printing purposes.
Internally we use the DeBruijn Indices that make it easy to equate and substitute.
Once we want to print a human-readable version, we use the list of stored names to replace all our indices by their names.
This approach recovers legibility for humans achieving the best of both worlds.

This approach isn't always applicable.
Sometimes it suffices to just use a dumb integer counter and attach a unique int to every name your compiler generates.
No ones ever going to see `foo__3405__`, so just increment the counter and move on with your day.
But if you find yourself scratching your head at how to check two ASTs are equal, or, like me, wasting weeks wondering why inling is emitting the wrong code, now you have the tools to tackle these tribulations.
