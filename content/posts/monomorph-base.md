+++
title = "Casting Out Polymorphism with Monomorphization"
date = "2025-05-11T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Monomorph"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Polymorphism", "IR", "Generics", "Type Erasure", "Monomorphization"]
description = "Instantiating concrete functions from generic roots"
+++

{{< accessory title="Making a Language" >}}
This post is part of the [making a language series](/series/making-a-language).
A series that teaches you how to implement a programming language using Rust.

Today's post is preceded by the [base simplify pass](/posts/simplify-base/).
That pass, however, is not a prerequisite to understanding monomorphization.
Monomorphization relies only on the `IR` introduced in [lowering](/posts/lowering-base-ir/).
We'll review `IR` before the action starts.
{{</ accessory >}}

As we press deeper into the compiler, we strip away high level features that stand between us and machine code.
Many of our language's features have no direct instructions in the hardware, and we must either remove them or translate them into something that does.
The feature on today's chopping block is polymorphism.

Polymorphism has been a close colleague till now, our whole IR design serves to better accommodate polymorphism.
Still, you won't find instructions to type apply a type function on any computer I've seen.
There are two main approaches to removing polymorphism from a language:

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

In erasing our types, we've lost the ability to distinguish their different memory layouts.
To support passing and returning any type, we're forced to pick the lowest common denominator memory layout for our type `T`.
A generic type `T` will be represented by a pointer to an undisclosed heap value.
In Rust terms, we can think of it as a `Box<dyn Any>`.
This is called uniform representation.

Representing generics as a pointer renders passing and returning them trivial.
We pass and return the pointer and don't worry about what it's pointing at.
Our job isn't quite done, though.
Consider again the case when our type `T` is `usize`.
A `usize` isn't a pointer and treating it as one has a history of undefined behavior.
Before we can pass `usize` to our generic function, we have to turn it into a pointer by boxing it into `Box<usize>`.
We'll have to do this for all types that'd otherwise be stack allocated and passed in registers.
They must be heap allocated behind a pointer before they can participate in type erased generics.

Type erasure is employed by a disparate set of languages.
You'll find it used to great effect in Java, Haskell, Swift, and many others.
Often times languages with garbage collection will recruit type erasure because a uniform representation makes garbage collection easier.

## Monomorphization

Monomorphization removes polymorphism by creating a new copy of our function for each instantiation of its types.
Anytime we encounter a type application we: 

* Grab all the types being appplied
* Create a new copy of our function body
* Substitute each type variable with our applied type in the new body

At the end of all that we have a fresh function, devoid of polymorphism, and we swap our application to use the new function.
None of the memory layout issues of type erasure are present here.
If our type `T` was `usize`, it remains `usize`, and we pass and return it as is.
Even better, now that we know specific types in our function body, we can further optimize based on that information.

[Wikipedia](https://en.wikipedia.org/wiki/Monomorphization) provides a concise snippet that highlights the idea:

```rs
fn id<T>(x: T) -> T {
  x
}

fn main() {
  let int = id::<i32>(10);
  let string = id::<&str>("some text");
  println!("{int}, {string}");
}
```

Each application of `id` spawns a new function specialized to the applied type (type application is spelled `::<>` in Rust):

```rs
fn id_i32(x: i32) -> i32 {
  x
}

fn id_str(x: &str) -> &str {
  x
}

fn main() {
  let int = id_i32(10);
  let string = id_str("some text");
  println!("{int}, {string}");
}
```

Monomorphization is used by our mother language Rust.
Other languages make piecemeal use of monomorphization, but Rust is one of the few that always monomorphizes.
C++ doesn't use monomorphization, but its template system shares a genealogy with monomorphization and is always used.
Each template function is copied once per instantiation and function definitions must be present everywhere they're applied.

## Tradeoffs

I've painted monomorphization in a pleasing light, but that's not the whole picture.
The optimizations provided by knowing specific types are a huge boon, but they don't come for free.
First and foremost, we have to create a copy of our function per type instantiation.
These copies quickly add up, inflating binary size and increasing compile times.

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
In practice many compilers will use some mixture of both type erasure and monomorphization where appropriate.

GHC defaults to type erasure, but will monomorphize aggressively for code that is all within the same module.
Small functions will be embedded in GHC's object files, so that they can be monomorphized across module boundaries.

## Implementation

After acquainting ourselves with the tradeoffs, we're going to use monomorphization for our language.
We lose out on separate compilation, but for our simple toy language writing code large enough to want separate compilation is a pipe dream.
Due to our small scale, we're also not worried about compile times.
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

Without top level items, however, passing a function like this is always decomposed into two type variables.
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

{{< accessory title="Quick Refresher" >}}

Our middleend passes all work on a common `IR`:

```rs
enum IR {
  Var(Var),
  Int(i32),
  Fun(Var, Box<Self>),
  App(Box<Self>, Box<Self>),
  TyFun(Kind, Box<Self>),
  TyApp(Box<Self>, Type),
  Local(Var, Box<Self>, Box<Self>),
}
```

Our IR is explicitly typed and represents generics using type functions and type applications.
It also introduces `Locals` which are not strictly required, but we'll see are very useful in simplification.
Alongside our IR, we have a `Type` (that is lowered from our AST's type):

```rs
enum Type {
  Int,
  Var(TypeVar),
  Fun(Box<Self>, Box<Self>),
  TyFun(Kind, Box<Self>),
}
```

Our `Type` has a `Kind`, unparalleled in our AST.
The same way values have types, types have kinds.
Our base `Kind` is vacuous:

```rs
enum Kind {
  Type
}
```

A `Type` is of kind `Type`.
I believe it to be true, but I question if we really need to postulate such a circular statement.
That's everything we need to know about our IR, back to monomorphization.

{{< /accessory >}}

With that there is not much to see in our implementation:

```rs
fn trivial_monomorph(
  ir: IR
) -> IR {
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
Don't believe me? Check the [repo](https://github.com/thunderseethe/making-a-language/tree/main/monomorph/base).
This will be much more substantial once we introduce items.
Until then, monomorphization provides a respite along our compilation journey.
If you're champing at the bit, our next pass, [closure conversion](/posts/closure-convert-base/), provides plenty to chew on.
