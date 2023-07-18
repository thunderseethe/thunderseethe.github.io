+++
title = "What's in a Module?"
date = "2023-06-21T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Modules"]
keywords = ["Programming Languages", "Compiler", "Modules", "Namespaces", "Encapsulation"]
description = "A discussion of the overloaded use of modules in languages"
+++

> A module by any other namespace would encapsulate just as sweet
>                  - Shakespeare..._mostly_

[Modular programming](https://en.wikipedia.org/wiki/Modular_programming) has become a cornerstone of modern software development. Software projects are only getting larger, and as they do the need to divide up monolithic codebases becomes indispensable. It is no surprise then, that some form of modules show up in all modern programming languages in use today:

  * Java calls their Modules Packages
  * Haskell has Modules
  * Rust has Modules (_and_ Crates)
  * Python has Modules
  * JavaScript notably lacked Modules, and that was so painful they now have [multiple](https://requirejs.org/) [competing](https://github.com/amdjs/amdjs-api/blob/master/AMD.md) [Modules](https://en.wikipedia.org/wiki/CommonJS)
  * C lacks Modules, but C++ has them as of C++20.
  * SML and OCaml have **Higher-Order** Modules

What is surprising however, is that all these modules implementations are different. Not just surface level distinctions either. Each language refers to a different idea when they call something a module:

  * Haskell/Rust modules allow nesting of modules inside other modules.
  * Java packages are strictly tied to files and are just namespaces. They cannot be nested, although it looks like they can.
  * JavaScript/Python modules are just runtime objects that can be imported with special syntax.
  * SML/OCaml allow you to create a new module that takes other modules as parameters.

This stands in stark contrast to other language features. All of these languages also support structured control flow (while, for, if/else) and first-class functions (lambda, closures, etc.). If we know how those work in one language we have a pretty good idea how they work in every other language, barring some syntax differences. What's different about modules that splintered their implementations so heavily.

Not to say these module implementations have no common ground. If we blur the details, we can see all of these modules are a list of definitions. A module bundles a list of definitions up as a thing we can reference from somewhere else. These module systems all share the same basic understanding of a module as an organizational tool. Why then are they so different in what features modules support, and what role modules play in compilation? Clearly these systems all started with the same goal in mind, but the devil is in the details.

## Modules, a little more formally

Module is an overloaded term. A module is loosely a bundle of definitions, for some that may be a satisfactory conclusion. But my end goal is to design my own module system for [my language](/series/making-a-language). To do that, we're going to need a more specific understanding of modules. There's a wealth of research into modules dating back decades. Academic research is known for being accessible and easily translated to a practical implementation, so let's see what they say about modules:

  * [Modules Matter Most](https://existentialtype.wordpress.com/2011/04/16/modules-matter-most/) - Blog post from Bob Harper, one of the creators of SML
  * [Backpack](https://plv.mpi-sws.org/backpack/) - Retrofitting more powerful modules onto Haskell's existing module system
  * [F-ing Modules](https://people.mpi-sws.org/~rossberg/papers/Rossberg,%20Russo,%20Dreyer%20-%20F-ing%20Modules.pdf) - A paper that seeks to unify and simplify the various module implementations
  * [Advanced Types and Programming Languages; chapter 8](https://www.cis.upenn.edu/~bcpierce/attapl/) - Great introduction into module implementation and the theory of modules

You don't need to read all of these (and I desperately need you to finish this blog post). But if you're interested in learning more about modules they are a great starting point.

We know modules are bags of definitions. The insight we glean from our research is what makes a module good or bad at bundling definitions. To be an effective bag of definitions, modules are responsible for two things:

  * Namespacing - control the visibility of its definitions
  * Packaging - creating self-contained components of code that can be combined arbitrarily

We'll discuss what these mean in more detail, but first I need to introduce some terminology we'll need.

### Strong vs Weak Modules

When we're talking about module implementations it can be helpful to categorize them based on how they handle dependencies. Modules depending on other modules is core functionality of any module system. It informs a lot of decisions in the rest of the system implementation. [Backpack](https://plv.mpi-sws.org/backpack/) introduces a helpful distinction between module systems based on how they handle dependencies:

  * Weak - a module must have all dependencies available before it can start compilation
  * Strong - a module can be compiled without its dependencies, and its dependencies can be provided later

Based on the definition alone, Strong modules sound better. Why would I want to wait on my dependencies if I don't have to? The answer is complexity. Weak modules are, historically, much simpler than Strong modules both to use and implement. The tradeoff for this simplicity is that they provide far fewer guarantees. A Strong module avoids requiring its dependencies by programming against an interface specifying its dependencies. This means once a Strong module compiles we're guaranteed it will work as long as provide dependencies that conform to that interface.

Requiring an interface for every dependency sounds very brittle and verbose though. Anytime a dependency needs to change I have to update its interface as well or my modules break. However this added friction provides a lot of benefits for building modules. Being able to build a module without its dependencies means we can build it in parallel with its dependencies. We can even build the module, and then later link it against dependencies we downloaded from a cache. We could replace once of our dependencies with a faster or more secure implementation, as long as its interface doesn't change. This is either impossible or much trickier to accomplish with Weak modules.

Despite the advantages, of the module systems we've seen so far only SML/OCaml use Strong modules. Every other module system we've seen uses Weak modules. Yet, all the research on modules deals with Strong modules. Weak modules are nowhere to be seen. Why is there such a strong discrepancy between modules in theory and modules in practice? Especially when Strong modules have so many advantages? I suspect this is due to a lag in mainstream languages. Much like every language eventually adopted lambdas and lazy iterators from functional programming, eventually they'll adopt Strong modules as well.

Not to say that Strong modules are purely upside. We've already touched on their downsides a little bit. We'll talk more in depth about their downsides after we talk about namespacing and packaging.

### Namespacing
Namespacing is all about determining what code can be seen by other code. It shares a lot of similarity with OO visibility where a class method can be declared public or private. Modules manage the visibility of their definitions with an export list. Anything in the export list is visible outside the module. Definitions not on the list, cannot be accessed outside the module. We can see how this maps to public and private. Exported symbols are public, everything else is private. However, modules can support more sophisticated forms of encapsulation.

Systems that allow nested modules can allow re-exporting. I can define a submodule with its own export list, and re-export a subset of that list from the parent module. This hides that the submodule exists at all. Other modules simply import the definition from the parent module.

On top of encapsulation, namespacing also allows for name duplication. It's fine for two functions to be named `foo` as long as they're in different modules. I can disambiguate them by their module name, `Module1.foo` and `Module2.foo`. This is helpful in large codebases that multiple teams are working on simultaneously.

Encapsulation and name management through namespacing are critical to modules. Every module system we've talked about supports them. If your modules don't provide namespacing, they aren't truly modules. I would argue all the systems we've seen so far are more namespace systems than module systems, except SML/OCaml. Because they all support namespacing but lack packaging features that are central to modules.

### Packaging
In contrast to namespacing, most module systems we've talked about do not support packaging. Packaging is concerned with how code gets compiled. When we feed our code to a compiler it has some choice in how to process it. Worst case the compiler just processes all the code in order, but generally that's unacceptably slow by today's standarads. The compiler breaks the code up into chunks that can be processed in parallel. These chunks go by a different name in most compilers, but we'll call them packages for our purpose. 

Parallelization is a large benefit of packages but not the only one. Because packages are a self-contained "chunk" of code, they can be serialized into a file and cached or shipped off somewhere else. Similar to the idea of object files that get linked together to create an executable (although packages need not literally be object files, frequently they are a serialized AST). This is particularly helpful for dependency management.

From this list of benefits, packaging looks a lot like Strong modules. While packaging does not necessitate Strong modules, Strong modules do accomplish packaging. Given this, it makes sense that all of our languages, but SML/OCaml, lack packaging. They all use Weak modules.

What's interesting is they all invent ad hoc methods to handle packaging. So the system has Weak modules for namespacing, and then makes something up for packages. Calls it a compilation unit, or a JAR. Packaging is clearly vital, but languages don't use the module system to do it. The notable exception to this being SML/OCaml with its Strong modules. So let's talk about why Strong modules aren't more popular.

### Downsides of Strong Modules (as seen in SML/OCaml)

I've spoken very highly of Strong modules. They almost sound too good to be true. They allow for highly parallel builds and naturally breaking our code up into packages. So why then, have they not seen wider adoption outside SML and OCaml? Is the industry simply blind to the light of Strong modules? Of course not. Strong modules incur a high complexity cost in exchange for their benefits.

We talked about how Strong modules mandate an interface between modules. Now we'll talk about the specifics of what that looks like for SML/OCaml. SML/OCaml has 3 language constructs that work with modules:

  * Structure - This is a module, a list of definitions.
  * Signature - An interface for a module, or a module type.
  * Functor - Builds a module out of other modules (like a function for modules)

Functors are how SML/OCaml implement Strong modules. Functors allow implementing a module in terms of signatures. We're free to compile the functor without know what modules it will be applied to. And when we later apply the Functor to a module, we check that module conforms to the signature, and we know it will work. This incurs verbosity the same way C headers do. I now have to maintain my definition and its signature in two different places and update them both anytime one changes. Arguably its even more complex because a module can conform to multiple signatures, not just one header like in C.

It gets worse from there. SML/OCaml also have abstract types, sharing constraints, and generative module instantiation. All of these features are powerful. They are what allow us to be confident when we apply a functor it constructs a valid module. But they all add a ton of complexity that aren't present in Weak module systems.

## Where does that leave Modules
So is that it then? Weak modules are prevalent in mainstream languages because they are easy to implement and easy to use. While Strong modules are relegated to the ivory tower of academy, where they'll never see the large codebases in which they thrive. I don't think so, quite the opposite in fact. I think there's a wealth of low-hanging fruit in research on modules. That can be applied to produce more powerful module systems than what we see today, without adding all the complexity of the module system of SML/OCaml. 

I do think the eventual consensus will be on using Strong modules of some fashion. Codebases are only growing larger, and programming in the large is where Strong modules really shine.[Backpack](https://plv.mpi-sws.org/backpack/) and [F-ing Modules](https://people.mpi-sws.org/~rossberg/papers/Rossberg,%20Russo,%20Dreyer%20-%20F-ing%20Modules.pdf) are two prime examples of that. A promising third approach I like a lot is [Mixin' Up the Module System](TODO). It seeks to simplify modules by combining structures and signatures into one construct, and using a linking mechanism to avoid a lot of the boilerplate around functors. I suspect ideas from these systems will become more prevalent in languages to come as they seek out Strong module systems to solve their needs.
