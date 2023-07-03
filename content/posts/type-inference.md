+++
title = "Part 1: Designing a Language without a Parser"
date = "2023-06-17T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Type Inference"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Type Inference", "Bidirectional Typechecking", "Unification", "Union-Find", "Constraint Solving"]
description = "Designing a language, types first"
+++

## Intro
I'd like to design a language, more specifically implement a compiler for a programming language I've made up. This is not the first time I've wanted to do this. In fact, I've had the [itch](https://github.com/thunderseethe/waht) [quite a](https://github.com/thunderseethe/panera) [few](https://github.com/thunderseethe/brainfuck_interpreter) [times](https://github.com/thunderseethe/false_interpreter) [before](https://github.com/thunderseethe/tiger). I can't tell you why I keep returning to this venture when I've failed at it so many times. What I can tell you is why I always fail. Every time I begin a sparkly new project with fresh eyes and quickly whip together a lexer.

However, once I start constructing a parser, progress slows to a crawl. I endlessly struggle with bikeshedding my syntax or fret to figure out my operator precedence. Without fail by the time I've managed to produce an Abstract Syntax Tree (AST) I've lost all steam. The language is added to my ever-growing pile of incomplete side projects doomed to forever be an empty repository of listless aspirations. The more attempts I've made the clearer this pattern has become to me.

So let's take a step back, why do I start with the lexer and parser at all? Compiler construction can be thought of as a couple of big steps where the output of each step is fed as input into the next step:

![Infographic for Compiler Pipeline](/img/compiler_pipeline.svg)

Because the compiler executes these steps in order when it runs, it's natural to think about creating them in order. The problem is compounded by the educational material on creating languages. Teaching material focuses a **lot** on the many methods by which you can parse text. It's to the point that the wonderful [Crafting Interpreters](https://craftinginterpreters.com/) felt the need to include [a note](https://craftinginterpreters.com/compiling-expressions.html#design-note) explaining why they are brushing over parsing. This over emphasis on parsing could lead one (certainly led me) to believe that parsing is the best place to start on a new language. However, there's no rule that states things have to be created in this order. In fact, doing things this way is impractical if you're designing a language from scratch. Trying to create syntax for your language without first knowing its semantics is a trap. It's like trying to design a vehicle's interior without knowing how many wheels it will have or how its engine will power them. You might design the interior to comfortably seat 4, but if you discover down the line you're designing a motorcycle you're in trouble!

So how do I overcome my parsing problem? I'll simply not write a lexer or parser. I can't get stuck writing a parser if I don't start writing one. I can come back later and add a parser once I've designed more of the internals of my language. My hope is having the other steps (Type Checking, Code Generation, etc.) will provide a guiding hand for what my parser should look like when I do eventually write one. The goal of writing a parser is to produce an AST. But we don't have to parse our AST out of text during prototyping, we can design an AST and then construct it ad hoc where we need it. Then the question is, if we're not starting with lexing/parsing, then what step do we start with? The answer is type inference (surprise), but it's relevant to answer why we're starting with type inference. 

To understand why we're starting with our types, look no further than the explanation in [Practical Foundations for Programming Languages](http://www.cs.cmu.edu/~rwh/pfpl.html):
> Types are the central organizing principle of the theory of programming languages. Language features are the manifestations of type structure. The syntax of a language is governed by the constructs that define its types, and its semantics is determined by the interactions among those constructs.

Types inform every other part of our language design, so it's important we nail them down first. Conversely, once we've designed our types the syntax and semantics of our language should easily fall out of our type system.

## Background
This article will not be a great introduction to programming language design in general. There are already a lot of great resources for that:

  * [Crafting Interpreters](https://craftinginterpreters.com/)
  * [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/)

We're focused on implementing type inference (For brevity, we won't even cover definitions of types, polymorphism, or ASTs). None of the ideas I'm covering here are new. My aspirations with this post are to coalesce good ideas I've found in disparate sources.  I've cobbled together an idea of how to do type inference from reading these sources, and looking at the source code of rustc and GHC. I couldn't find a good resource that tied all the concepts together, so I decided to write one. 

We'll build our language in Rust. One might wonder why we'd pick a low level language like Rust, and why not a higher level language that is less verbose such as Haskell or OCaml. These languages are often featured in literature on compilers, and for good reason. They make expressing the tree traversals involved in compilation very natural. However, I don't know OCaml. I'm more familiar with Haskell, but our type inference will make heavy use of mutation which makes the immutable purity of Haskell a poor fit. Rust has enough features to be functional for our tree traversals while still allowing for mutation. So we'll be using Rust, if you're unfamiliar [cheats.rs](https://cheats.rs) has a dense rundown to get you up to speed.

## Abstract Syntax Tree (AST)
The whole point of parsing is to convert a text file into an AST. Since we're skipping over parsing, we'll start with an AST already constructed. If you'd like a refresher on ASTs check out [this blog post](https://ruslanspivak.com/lsbasi-part7/). Our language is new, so we'll start small and add more fancy AST nodes incrementally. Initially, our AST will have just 4 nodes: variables, integer literals, function literals, and function application:
```rs
enum Ast<V> {
  /// A local variable
  Var(V),
  /// An integer literal
  Int(isize),
  /// A function literal 
  /// (lambda, closure, etc.)
  Fun(V, Box<Self>),
  /// Function application
  App(Box<Self>, Box<Self>), 
}
```
It doesn't get much simpler than this. This isn't really enough for a usable language, although it is Turing complete. For our purposes, it's plenty to get a minimal type inference system set up. We parameterize our AST by a type parameter `V`. `V` will be the variable of our AST used in `Var` and `Fun`. Initially our AST variable type parameter will be:
```rs
struct Var(usize);
```
For simplicity, we'll represent variables as a `usize`. We can imagine these might be interned strings in a more sophisticated compiler. Once we type check our AST, each variable will be annotated with its type:
```rs
struct TypedVar(Var, Type);
```
Rust requires a layer of indirection for recursive types. Boxing values will make our code samples pretty noisy, let's define some helper methods to hide that noise:
```rs
impl<V> Ast<V> {
  fn fun(
    arg: V, 
    body: Self
  ) -> Self {
    Self::Fun(
      arg, 
      Box::new(body))
  }

  fn app(
    fun: Self, 
    arg: Self
  ) -> Self {
    Self::App(
      Box::new(fun), 
      Box::new(arg))
  }
}
```

## Type
Before we can begin inferring types, we have to know what types can be inferred. Our `Type` type (ha) is determined by our `Ast`. We need a type to cover each possible value our `Ast` could produce. Since our `Ast` only has 4 cases, we can check each one and determine what values they can produce:

 * `Var` - it produces any value `AST` produces (but is not a value itself)
 * `Int` - Integer literals are values and will need a type, the type of integers
 * `Fun` - Function literals are also values and will need a type, the type of functions
 * `App` - Function application will return a value when evaluated, but the application case itself is not a value.

So we have two values we can produce from our `Ast`: `Int`, and `Fun`. We need two types for these values. We also need a type for type variables; these won't be for any values but allow us to support polymorphism. Knowing all this we can lay out `Type`:
```rs
struct TypeVar(u32);
enum Type {
  // A type variable
  Var(TypeVar),
  // Type of integers
  Int,
  // Type of functions
  Fun(Box<Self>, Box<Self>),
}
```
Similar to `Var`, our `TypeVar` is just a number under the hood. Our `Type` is a recursive tree, same as `AST`, so we have to box our nodes. We introduce similar helper methods to alleviate the boxing:
```rs
impl Type {
  fn fun(
    arg: Self, 
    ret: Self
  ) -> Self {
    Self::Fun(
      Box::new(arg), 
      Box::new(ret))
  }
}
```

## Type Inference Algorithm
At a high level, the goal of type inference is to use contextual information from our AST to infer the type of each AST node. In trivial cases this is very easy when we see an `Int` AST node we know it always has type `Int`. It's not always so straightforward, though. If we want to know the type of an `App` node, we have to look at the return type of the function of that `App` node. However, we may not know the type of that function yet. The function might be a variable that refers to an input parameter to our overall AST term. In that case we can't know the type of our function until we know the type of our whole term, which seems circular. The way to break up this circular reasoning is by waiting to infer our type until we have enough information to do so. Instead of inferring a type immediately, when we encounter a type we don't know, we'll track some constraints about that type. Then once we've walked our entire AST we should have enough constraints to infer all our types.

That sounds a little hand wavy; how can we be sure we'll always generate enough constraints to figure out our types? The answer is **math**! I won't go into details here, but we'll lean on some very nifty proofs a lot of folks worked on to guide us to a type system that is always inferrable. For more information, look into the [Hindley-Milner (HM) type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system).

So we're going to track information about types. We do this on our type variables since they represent unknown types. When we encounter a node, we'll give it a fresh type variable and emit a constraint on that variable. Any other nodes that rely on that node will reuse its type variable, producing their own constraints on that variable. This is the basis for implementations of the HM type system such as [Algorithm J](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_J), or its cousin Algorithm W. There's a key difference between those implementations and ours though. Algorithm J attempts to solve constraints as soon as it encounters them and uses a clever data structure to avoid the circular reasoning we saw earlier. Our implementation will instead generate all constraints in one pass and then solve them all in a separate pass. We can visualize this as 3 separate passes: constraint generation, constraint solving, and a final type substitution.

![Type Inference Algorithm](/img/type_inference_algorithm.svg)

This split into constraint generation followed by constraint solving offers some nice properties over the "solve as you go" solution. Because we generate a set of constraints from the AST and then solve them, constraint solving only has to know about constraints and nothing about the AST. This means constraint solving requires no modifications when we change our AST. Right now while we have 4 AST nodes the benefit is small, but most languages have 100+ AST nodes. Being able to handle all these cases in constraint generation and not have them bleed over into constraint solving is a huge win for managing complexity. An explicit list of constraints can also make error reporting easier. We can associate a span with each constraint. Then use these spans help to print smarter error messages on a type error. LambdaAle has a great talk by Simon Peyton Jones [Type Inference as Constraint Solving](https://www.youtube.com/watch?v=-TJGhGa04F8&t=2731s) that goes over the benefits in more detail.

We'll put in pin our type inference aspirations here. Our AST, Type, and type inference algorithm provide a strong foundation for our implementation. In our [next post](/posts/bidirectional-constraint-generation) we'll implement constraint generation using a bidirectional type system.
