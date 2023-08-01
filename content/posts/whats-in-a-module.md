+++
title = "What's in a Module?"
date = "2023-06-21T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Modules"]
keywords = ["Programming Languages", "Compiler", "Modules", "Namespaces", "Encapsulation"]
description = "A discussion of the overloaded use of modules in languages"
+++

## That Which We Call a Module

>
> A module by any other namespace would encapsulate just as sweet
>
>                                        - Shakespeare...mostly

[Modular programming](https://en.wikipedia.org/wiki/Modular_programming) has become a cornerstone of modern software development. 
Software projects are only getting larger, and as they do the need to divide up monolithic codebases becomes indispensable. 
It is no surprise then, that some form of modules show up in all modern programming languages in use today:

  * Java, Haskell, Rust, Python all have modules. (Java wisely calls them packages, alas that's not enough to keep them out of this article)
  * JavaScript notably lacked modules, and that was so painful they now have [multiple](https://requirejs.org/) [competing](https://github.com/amdjs/amdjs-api/blob/master/AMD.md) [modules](https://en.wikipedia.org/wiki/CommonJS).
  * C lacks modules, but C++ has them as of C++20.
  * SML and OCaml have **Higher-Order** modules.

What is surprising however, is that all these modules implementations are different. 
Not just surface level distinctions either. 
Each language refers to a different idea when they call something a module:

| Language   | Allows nesting | Higher-order | First-class | Representation |
| ---------- | -------------- | ------------ | ----------- | -------------- |
| Haskell    | Yes            | No           | No          | Compile-time   |
| Rust       | Yes            | No           | No          | Compile-time   |
| Java       | No             | No           | No          | Compile-time   |
| Javascript | No             | No           | Yes         | Run-time       |
| Python     | No             | No           | Yes         | Run-time       |
| SML/OCaml  | Yes            | Yes          | No          | Compile-time   |

This stands in stark contrast to other language features. All of these languages also support structured control flow (while, for, if/else) and first-class functions (lambda, closures, etc.). If we know how those work in one language we have a pretty good idea how they work in every other language, barring some syntax differences. What's different about modules that splintered their implementations so heavily?

## Modules, a Little More Formally

Module is an overloaded term. If we blur the details of the modules we've looked at, they're all loosely a bundle of definitions.
A module bundles a list of definitions up as a thing we can reference from somewhere else.
Nevertheless, that doesn't explain the vastly different set of features we've seen in each module system.
We'll have to look elsewhere to find a more holistic definition of modules.

Fortunately, there's a wealth of research into modules dating back decades.
Academic research is known for being accessible and easily translated to a practical implementation, so let's see what they say about modules:

  * [Modules Matter Most](https://existentialtype.wordpress.com/2011/04/16/modules-matter-most/) - Blog post from Bob Harper, one of the creators of SML
  * [Backpack](https://plv.mpi-sws.org/backpack/) - Retrofitting more powerful modules onto Haskell's existing module system
  * [F-ing Modules](https://people.mpi-sws.org/~rossberg/papers/Rossberg,%20Russo,%20Dreyer%20-%20F-ing%20Modules.pdf) - A paper that seeks to unify and simplify the various module implementations
  * [Advanced Types and Programming Languages; chapter 8](https://www.cis.upenn.edu/~bcpierce/attapl/) - Great introduction into module implementation and the theory of modules

You don't need to read all of these (and I desperately need you to finish this blog post). 
But if you're interested in learning more about modules they are a great starting point.

We know modules are bags of definitions. The insight we glean from our research is what makes a module good or bad at bundling definitions. To be an effective bag of definitions, modules are responsible for two things:

  * **Namespacing** - Control the visibility of its definitions
  * **Packaging** - Creating self-contained components of code that can be combined arbitrarily

We'll discuss what these mean in more detail, but first I need to introduce some terminology we'll need.

## Strong vs Weak Modules

When we're talking about module implementations it can be helpful to categorize them based on how they handle dependencies. Modules depending on other modules is core functionality of any module system. It informs a lot of decisions in the rest of the system implementation. [Backpack](https://plv.mpi-sws.org/backpack/) introduces a helpful distinction between module systems based on how they handle dependencies:

  * **Weak** - A module must have all dependencies available before it can start compilation
  * **Strong** - A module can be compiled without its dependencies, and its dependencies can be provided later

Based on the definition alone, Strong Modules sound better. 
Why would we want to wait on our dependencies if we don't have to? The answer is complexity. 
Weak Modules are, historically, much simpler than Strong Modules both to use and implement. 
The tradeoff for this simplicity is that they provide far fewer guarantees. 
A Strong Module avoids requiring its dependencies by programming against an interface specifying its dependencies. 
This means once a Strong Module compiles we're guaranteed it will work as long as provide dependencies that conform to that interface.

Requiring an interface for every dependency sounds very brittle and verbose though. 
Anytime a dependency needs to change we have to update its interface as well or our modules break
However, this added friction provides a lot of benefits for building modules. 
Being able to build a module without its dependencies means we can build it in parallel with its dependencies. 
We can even build the module, and then later link it against dependencies we downloaded from a cache. 
We could replace once of our dependencies with a faster or more secure implementation, as long as its interface doesn't change. 
This is either impossible or much trickier to accomplish with Weak Modules.

Despite the advantages, of the module systems we've seen so far only SML/OCaml use Strong modules. 
Every other module system we've seen uses Weak Modules. 
Yet, all the research on modules deals with Strong Modules. 
Weak Modules are nowhere to be seen. Why is there such a strong discrepancy between modules in theory and modules in practice? 
Especially when Strong Modules have so many advantages? Personally, I suspect this is due to a lag in mainstream languages.
Much like every language eventually adopted lambdas and lazy iterators from functional programming, eventually they'll adopt Strong Modules as well.

Not to say that Strong Modules are purely upside. We've already touched on their downsides a little bit.
We'll talk more in depth about their downsides after we talk about namespacing and packaging.

## Namespacing
Namespacing is all about determining what code can be seen by other code. 
It shares a lot of similarity with OO visibility where a class method can be declared public or private. 
Modules manage the visibility of their definitions with an export list. Anything in the export list is visible outside the module.
Definitions not on the list cannot be accessed outside the module. 
We can see how this maps to public and private. 
Exported symbols are public, everything else is private. However, modules can support more sophisticated forms of encapsulation.

Systems that allow nested modules can allow re-exporting. 
We can define a submodule with its own export list, and re-export a subset of that list from the parent module.
This hides that the submodule exists at all. Other modules simply import the definition from the parent module.

On top of encapsulation, namespacing also allows for name duplication. 
It's fine for two functions to be named `foo` as long as they're in different modules. 
We can disambiguate them by their module name, `Module1.foo` and `Module2.foo`. 
This is helpful in large codebases that multiple teams are working on simultaneously.

Encapsulation and name management through namespacing are critical to modules. 
Every module system we've talked about supports them. 
If your modules don't provide namespacing, they aren't truly modules. 

In fact, all the systems we've seen so far are more namespace systems than module systems, except SML/OCaml.
They all support namespacing sufficiently, but none of them support packaging. 
There are only two tenets to being an effective module, and all our systems are missing one!
They're basically providing half of a module.


## Packaging

None of our modules support packaging. So what is packaging then, is it important? (Spoilers: Yeah, we made it one of our **two** tenets)
Packaging is concerned with how code gets compiled. When we feed our code to a compiler it has some choice in how to process it. 
Worst case the compiler just processes all the code in order, but generally that's unacceptably slow by today's standards. 
The compiler breaks the code up into chunks that can be processed in parallel. 
These chunks go by a different name in most compilers, but we'll call them packages for our purpose. 

Parallelization is a large benefit of packages but not the only one. 
Because packages are a self-contained "chunk" of code, they act as separation boundaries in a codebase.
This lets us manage separate packages independently.
We can swap out one package for another as long as they line up on the module boundary we can be confident our code will continue to work as expected.
Similar to how a dynamic library can be updated without recompiling, as long as it exports the same interface.
This is particularly helpful for dependency management.

From this list of benefits, packaging looks a lot like Strong Modules. 
While packaging does not necessitate Strong Modules, Strong Modules do accomplish packaging.
Given this, it makes sense that all of our languages, except SML/OCaml, lack packaging. 
They all use Weak Modules.

Packaging is clearly vital, but languages don’t use the module system to do it. 
They all invent ad hoc methods to handle packaging.
So the system has Weak Modules for namespacing and makes something else up for packages, calls it a compilation unit, JAR, or something else programmers hate. 
The notable exception to this being SML/OCaml with its Strong Modules.
Let’s talk about why Strong Modules aren’t more popular.

## Downsides of Strong Modules (as seen in SML/OCaml)

I've spoken very highly of Strong Modules. They almost sound too good to be true. 
They allow for highly parallel builds and naturally breaking our code up into packages. 
So why then, have they not seen wider adoption outside SML and OCaml? Is the industry simply blind to the light of Strong Modules?
Of course not. Strong Modules incur a high complexity cost in exchange for their benefits.

We talked about how Strong Modules mandate an interface between modules. 
Now we'll talk about the specifics of what that looks like for SML/OCaml. SML/OCaml has 3 language constructs that work with modules:

  * **Structure** - This is a module, a list of definitions.
  * **Signature** - An interface for a module, or a module type.
  * **Functor** - Builds a module out of other modules (like a function for modules)

Functors are how SML/OCaml implement Strong Modules. Functors allow implementing a module in terms of signatures. 
We're free to compile the functor without know what modules it will be applied to. 
And when we later apply the Functor to a module, we check that module conforms to the signature, and we know it will work. 
This incurs verbosity the same way C headers do. 
We now have to maintain our definition and its signature in two different places and update them both anytime one changes. 
Arguably its even more complex because a module can conform to multiple signatures, not just one header like in C.

It gets worse from there. SML/OCaml also have abstract types, sharing constraints, and generative module instantiation. 
All of these features are powerful sure, but we pay a steep complexity cost for that power.
They are what allow us to be confident when we apply a functor it constructs a valid module.
But Weak Module systems produce valid modules without any of that complexity.

## We Want Stronger Modules
It becomes obvious to me, languages want Strong Modules. 
They already have Weak Modules, and they all invent their own solution for packaging.
But the complexity of Strong Modules scares them off from adding them to their language.
Fortunately the complexity of Strong Modules isn't an insurmountable obstacle.

There's always been a long lag between a feature being prevalent in academia, and becoming prevalent in mainstream languages.
This was true of first class functions and even structured control flow once upon a time.
I think we're in the middle of a similar lag for modules. Academia is currently pushing to simplify Strong Modules.
While mainstream languages reinvent packaging to try and achieve its benefits without the complexity of Strong Modules.
It's only a matter of time before Strong Modules become simple enough they become an attractive solution for the packaging needs of the large codebases in mainstream languages.

[Backpack](https://plv.mpi-sws.org/backpack/) and [F-ing Modules](https://people.mpi-sws.org/~rossberg/papers/Rossberg,%20Russo,%20Dreyer%20-%20F-ing%20Modules.pdf) are two prime examples of this we already talked about. 
[Mixin' Up the Module System](https://people.mpi-sws.org/~rossberg/papers/Rossberg,%20Dreyer%20-%20Mixin'%20Up%20the%20ML%20Module%20System.pdf) is a third approach that I think shows a lot of promise. 
It seeks to simplify modules by combining structures and signatures into one construct, and using a linking mechanism to avoid a lot of the boilerplate around functors. Strong modules are only getting easier to use, and I think they will eventually become the standard the same way first class functions and structured control flow have.
