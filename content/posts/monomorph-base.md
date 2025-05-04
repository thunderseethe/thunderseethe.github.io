+++
title = "Monomorph[0].Base: Modest exposition of Monomorphization"
date = "2025-04-30T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Monomorph"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Polymorphism", "IR", "Generics", "Type Erasure", "Monomorphization"]
description = "Casting out generics once and for all!"
+++

* There are no machine instructions for polymorphism.
  * Ergo polymorphism stands between us and executing our code.
  * Two options:
    * Type erasure
    * Monomorphization

* Type erasure
  * One function per parametric function
  * Treats generic values as black boxes
  * Uniform representation
  * As seen in Java, Haskell

* Monomorphization
  * Multiple functions per parameter function.
  * Instantiate a function per type combination.
  * Update callsites to reference instantiated function.
  * Specific memory layouts are available.
  * As seen in Rust, C++.

* Tradeoffs
  * Monomorphization
    * Pros
      * New optimization opportunities based on specific types
      * More efficient memory layout and calling convention
    * Cons
      * Requires whole program compilation
      * Code size from instantiating copies of each function
  * Type erasure
    * Pros
      * Separate compilation
      * Reduced code size
    * Cons
      * Inefficient memory layout
      * Reduces optimization opportunities

* Approaches are not mutually exclusive
  * Can monomorphize for some specific types and leave others type erased.
  * Reference material on this.

* For all that exposition we don't have items in our base language.
* We don't even have interesting types
* All our type variables are solved to Int.
  * Explain why a type variable can't be solved to a function.

* The job of monomorphizing in base is trivial:

```rs
pub fn monomorph(ir: IR, types: Vec<Type>) -> IR {
  types.into_iter().fold(ir, unwrap_forall)
}
```

* Given a list of types to instantiate our `IR` at, we unwrap a forall per type and return the resulting `IR`.
* `unwrap_forall` isn't much to look at either:


```rs
fn unwrap_forall(ir: IR, ty: Type) -> IR {
  let IR::TyFun(_, body) = ir else {
    panic!("ICE: Applied a type to a non type function IR in monomorph");
  };
  simplify_base::subst_ty(*body, ty)
}
```

* We aren't even using our own code!
* We already had to substitute types while simplifying.
* We reuse that logic as-is here.

* Monomorphizing our base is downright trivial.
* We'll more work once we introduce `items`.
* Until then, this pass is free parking.

TODO: Making a language blurb

As we press deeper into the compiler, we must strip away high level features that stand between us and execution.
Many of our language's features have no direct instructions in the hardware, and we must either remove them or translate them into something that does.
The feature on today's chopping block is polymorphism.

Polymorphism has been a close colleague till now, our whole IR design serves to better accommodate polymorphism.
Still, you won't find instructions to type apply a type function on any hardware I've seen.
There are two main approaches to removing polymorphism from your language:

  * Type Erasure
  * Monomorphization

The post title gives away which one we'll be selecting, but we'll discuss each, so we understand the tradeoff.

## Type Erasure

Type erasure, as the name implies, erases polymorphism by simply removing type functions and type applications wherever they occur.
Hard to imagine a simpler solution.
We can't represent polymorphism in hardware, so we simply won't.
We're left with a conundrum, however.

How do we represent the remaining type variables after erasing their type functions?
Our code is already type checked, so we don't have to worry about calling a nonexistent method or the like.
But we still have pragmatic considerations, how do we represent a generic type `T` in memory?
If `T` stands for a type such as `usize`, its value will be in a register.
If, however, our `T` stands for type `Box<(usize, usize)>`, it will be a pointer into the heap.

In erasing our types we've lost the ability to distinguish their different memory layouts.
To support passing and returning any type, we're forced to pick the lowest common denominator memory layout for our type `T`.
A generic type `T` will be represented by a pointer to an undisclosed heap value.
In Rust terms, we can think of it as a `Box<dyn Any>`
This is called uniform representation.

Representing all generics as a pointer makes passing and returning them trivial, we pass and return the pointer and don't worry about what it's pointing at.
Our job isn't quite done, though.
Consider again the case when our `T` is instantiated at type `usize`.
A `usize` isn't a pointer and treating it as one has a history of undefined behavior.
Before we can pass `usize` to our function, we have to turn it into a pointer by boxing it into `Box<usize>`.

Type erasure is employed by a disparate set of languages.
You'll find it used to great effect in Java, Haskell, Swift, and many others.
TODO: Transition here

## Monomorphization

Monomorphization removes polymorphism by creating a new copy of our function for each instantiation of their types.
Anytime we encounter a type application we: 

* Grab all the types being appplied
* Create a new copy of our function body
* Substitute each type variable with our applied type in the new body

At the end of all that we have a fresh function, devoid of polymorphism, and we swap our application to use the new function.
None of the memory layout issues of type erasure are present here.
If our type `T` was `usize`, it remains `usize`, and we pass and return it as is.
Even better, now that we know specific types in our function body, we can further optimize based on that information.

Monomorphization is used by our mother language Rust.
C++ templates behave much like monomorphization, they create a copy of the template per instantiation.
Templates, however, are only validated once instantiated, whereas during monomorphization our polymorphic functions are all known to be valid.
TODO: Transition

## Tradeoffs

I've painted monomorphization in a pleasing light, but that's not the whole picture.
The optimizations provided by knowing specific types are a huge boon, but they don't come without a cost.
First and foremost, we have to create a copy of our function per type instantiation.
These copies quickly add up and inflate binary size and slow compile times.

Monomorphization also forces our whole program to be present during compilation.
Because we create a copy of a function anywhere it's applied, our function definition must be available anywhere it's applied.
This effectively precludes any opportunities for separate compilation.
Separate compilation is critical to compiler performance, allowing compilation to be split up into multiple parts and run in parallel, or processed incrementally and cached.

The pros and cons of type erasure reflect the tradeoffs of monomorphization.
Type erasure maintains only requires one function body for any number of instantiations.
Because all instantiations use the same body, we don't require the function definition be present during compilation.
It suffices to know the function's signature, enabling separate compilation.

Of course, we lose the opportunity to optimize knowing our specific types.
In some cases we may even hinder performance by boxing previously unboxed values such as `usize`.
It may sound like monomorphization is the clear victor here.
Who wouldn't trade compilation time for better runtime performance?
The ruckus about Rust compile times should be enough to tell you that, in fact, many people.

It also not a given that monomorphization will perform better.
Boxing up values hurts performance for small types, but can often be a performance win for big types.
Many copies of, essentially, the same function puts more pressure on the instruction cache as well.
In practice many compilers will use some mixture of both type erasure and monomorphization where appropiate.

GHC defaults to type erause, but will monomorphize aggressively for code that is all within the same module.
Small functions will be embedded in GHC's object files, so that they can be monomorphized across module boundaries.
TODO: Transition

## Implementation

After acquainting ourselves with the tradeoffs, we're going to use monomorphization for our language.
We lose out on separate compilation, but for our simple toy language writing code large enough to want sepearate compilation is a pipe dream anyhow.
We're also not that worried about compile times due to our small scale.
These are problems for successful languages that people use.

Despite all that exposition, our monomorphization implementation for base is rather underwhelming.
Monomorphization seeks out type applications and replaces them.
But our language only introduces type functions for top level items when we generalize in type inference.
Correspondingly, type applications only appear applied to top level items.

Our base language, however, lacks top level items.
You'll find no mention of type applications among our base IR.
Fret not, we still have some work to do, but it feels frivolous compared to true monomorphization.

We may not have type applications, but we do have type functions at the top of our IR.
Removing type functions from our IR is a matter of guessing what type to substitute for the function.
Lack of top level items makes this easy, they are all `Int`.

This sounds too good to be true.
Surely not all of our type variables can be `Int`?
Our `Type` only has two non-polymorphic cases to consider `Int` and `Fun`.
The only way a variable becomes solved to a `Fun` type is when we pass a function to another function.

Without top level items, however, passing a function like this is always decomposed.
We introduce fresh type variables to represent the argument and return type of our function, rather than the function itself.
Consider the code:

```rs
let id = |x| x;
id(|y| y + 1);
```

Our `id`'s argument is a function type, but the type of our overall term is: 

```rs
Type::fun(Type::Var(0), Type::Var(0))
```

We always have enough information to decomposes a function type into it's argument and return types when reasoning locally.
Given we can always decompose function types, only one type remains for any variable we encounter: `Int`.

TODO: Quick Refresher

With that there is not much to see in our implementation:

```rs
fn trivial_monomorph(ir: IR) -> IR {
  let mut types = vec![];
  let mut fun = &ir;
  // Assume all types are Int.
  // This can't be wrong for base because we don't yet support any interesting types.
  // Any function getting passed around will use a function type not a
  while let IR::TyFun(_, body) = fun {
    types.push(Type::Int);
    fun = body;
  }
  instantiate(ir, types)
}
```

We take an IR as input, find all the type functions at the root of the IR, and instantiate them all to `Int`.
`instantiate` isn't particularly breathtaking either:

```rs
fn instantiate(
  ir: IR, 
  types: Vec<Type>
) -> IR {
  types.into_iter().fold(ir, |ir, ty| {
    let IR::TyFun(_, body) = ir else {
      panic!("ICE: Applied a type to a non type function IR in monomorph");
    };
    simplify_base::subst_ty(*body, ty)
  })
}
```

It's not even using new code!
The meat of the implementation resides in `subst_ty`, a method we already had lying around from simplification.
That's really all we have for monomorphization right now.
This will be much more substantial once we introduce items.
Until then, monomorphization provides a respite along our journey to executing code.
