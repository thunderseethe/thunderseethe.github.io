+++
title = "What's in a Module?"
date = "2023-06-21T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Modules"]
keywords = ["Programming Languages", "Compiler", "Modules", "Namespaces", "Encapsulation"]
description = "A discussion of the overloaded use of modules in languages"
+++

Purpose: 
    1. Every language has a module system at this point, but they are all different.
    2. Modules handle namespacing and packaging, but most languages only use modules for namespacing.
    3. SML/OCaml modules support namespacing and packaging, why hasn't their module system seen widespread adoption?
    4. Mixin Modules offer a large usability improvement over SML/OCaml modules, while maintaining the same powerful module features SML/OCaml modules provide.

## Intro (WIP Name)
Purpose: Introduce modules and talk about their prevalence in programming languages. Motivate module is an overloaded term that doesn't have a clear implementation in programming languages.

* Shakespeare quote 😤
* Modules are ubiquitous
* Researching modules to design my own module system for my languages
  * Shameless self plug
* Each language has a slightly different implementation of modules (JavaScript, Haskell/Rust, SML/OCaml)
  * Enumerate meaningful differences between modules to exemplify problem
* High level understanding of modules
  * Break large code bases up into independent pieces
  * Program is a composition of independent modules bundled together

## Point of Modules
Purpose: The main concern of modules are: namespacing and packaging. Link to resources that this article is going to distill.

* Have to look elsewhere for a more precise definition of modules
* List of resources on modules (Modules matter most, ATAPL, backpack, f-ing modules)
* Modules are responsible for two main things
  * Namespacing
  * Packaging
* Strong vs Weak distinction introduced by backpack paper
    * Strong modules can be compiled in isolation
    * Weak modules have to have all dependencies available before they can be compiled

## Strong vs Weak Modules
Purpose: Most languages support weak modules, but not Strong modules. Strong modules sound strictly superior.

* Weak modules must have all of their dependencies available before they can be compiled.
  * Easier to implement
    * Probably why it is more common
  * Forces sequential work during compilation
* Strong modules can be compiled in isolation and linked against their dependencies later
  * Allows for building modules in parallel
  * Allows for prebuilding modules and downloading them
  * Allows for stronger verification of dependencies, they have to meet the expected signature
  * This is only getting more important as codebases gets bigger

## Namespacing
Purpose: Namespacing controls visibility of code. Allows for encapsulation by determining the public vs private API of a piece of code. Highly visible to the user of the language.

* Namespacing is about controlling visibility of code
* Modules export a list of symbols, anything not exported cannot be accessed outside the module
* Allows encapsulation by only exporting the public API of a module
* Implementation details are hidden within the module (private API)
* Compare to public/private for OO classes
* Every language that has modules supports namespacing (unlike packaging)
* Feature becomes invaluable in large code bases
  * JavaScript needed the feature so badly once code got big they created multiple user land module systems before they were added to the language

## Packaging
Purpose: Packaging is important for [separation of concerns](TODO). Code can be created as separate components and then combined arbitrarily later as long as their interfaces line up.

* Modules package up code as a self-contained unit
* Allows other people to import and use your code
* Programming against an interface
* C headers exist to handle packaging
* All languages end up supporting packages in some form
  * Compilation Units in C/C++
  * Crates in Rust
  * .hi files in Haskell
* Languages end up here because they need packaging to support parallel compilation
  * Packaging is also vital to dependency management and having a concept of a library
* These are all Weak module systems 
  * Limits parallelism
  * Relies on implicit interfaces exported by dependency
    * Whenever a dependency changes its interface your module breaks

* SML/OCaml modules are Strong modules
  * Module interfaces are explicit
  * Modules can be compiled without their dependencies
  * Better dependency management guarantees
    * Any dependency that fufills your interface can be used
    * Interface you depend on must change to break your code 
    * Your dependency can't publish a new version that breaks your code

### Downsides of Strong Modules (as seen in SML/OCaml)
Purpose: Strong modules from SML/OCaml are verbose and complex.

* Why did most languages choose to use a Weak module system, despite the advantages of Strong modules.
  * At the time parallel compilation was less mandatory, codebases were smaller.
  * SML's module system is complex and a huge barrier to entry
* Quick summary of SML/OCaml modules
  * Signatures are module interfaces
  * Structures are module implementations
  * Functors allow parameterizing a module on other modules
* Signatures are verbose and introduce boilerplate by duplicating code (reference C headers).
* Generative Module Functors are complex and manually instantiating modules is unintuitive (and verbose).
* Abstract types have complicated semantics and require propagating a bunch of shraing clauses

## Mixin Modules
Purpose: Mixin Modules provide an improvement over SML/OCaml modules. 

* A lot of research into modules has been done since the SML module system was designed
* Mixin Modules are a promising approach that seeks to reduce the complexity of working with first-class modules
* Signatures are combined with structures to reduce boilerplate. 
* Linking automatically links up a modules dependencies, removing the need for functors and manually wiring up modules.
* Mixin Modules can use applicative semantics allowing for automatic module equality compared to generative module functors.


## What's in a Module?

> A module by any other namespace would encapsulate just as sweet
>                  - Shakespeare..._mostly_

[Modular programming](TODO) has become a cornerstone of modern software development. Software projects are only getting larger, and as they do the need to divide up monolithic codebases becomes indispensable. It is no surprise then, that some form of modules show up in all modern programming languages in use today:

  * Java calls their Modules Packages
  * Haskell has Modules
  * Rust has Modules (_and_ Crates)
  * Python has Modules
  * JavaScript notably lacked Modules, and that was so painful they now have [multiple](https://requirejs.org/) [competing](https://github.com/amdjs/amdjs-api/blob/master/AMD.md) [Modules](https://en.wikipedia.org/wiki/CommonJS)
  * C lacks Modules, but C++ has them as of C++20.
  * SML and OCaml have **First-Class** Modules

What is surprising however, is that all these modules implementations are different. As part of [designing my own language](series/making-a-language/), I've been thinking about how to design a module system. Imagine my distress to find out there's no consensus around what a module is, let alone what it should do! I really want to highlight the distinction between these implementations, so let's take a quick look at them:

  * Haskell/Rust have second-class modules. Allows nesting of modules inside other modules
  * Java packages are strictly tied to files and are just namespaces. They cannot be nested.
  * JavaScript/Python modules are just runtime objects that can be imported with special syntax support.

This stands in stark contrast to other language features. All of these languages also support structured control flow (while, for, if/else) and first-class functions (lambda, closures, etc.). If we know how those work in one language we have a pretty good idea how they work in every other language, barring some syntax differences. What's different about modules that splintered the implementations so heavily.

We've been highlighting the differences between these module systems, but they do have some common ground. If we blur the details we can see some shared features:

 * Allows separating code into independent shared pieces
 * Encapsulation - These all allow modules to have implementation details no one else can see

Clearly these systems started with the same goal in mind, but the devil is in the details.  At their core, modules are concerned with breaking up code into manageable pieces. We can wrap code up in a module and then import it wherever we need to use it.

## Modules, a little more formally

Looking at existing implementations helped us get a high level idea of modules. Nevertheless, to design a module system we're going to need a more precise understanding. There's a wealth of research into modules dating back decades. Academic research is known for being accessible and easily translated to a practical implementation, so let's see if they can give us a deeper understanding of modules.

Jokes aside, there are some great resources to learn about modules that we're going to distill:

  * [Modules Matter Most](TODO) - Blog post from Bob Harper one of the creators of SML
  * [Backpack](TODO) - Retrofitting more powerful modules on Haskell's existing module system
  * [F-ing Modules](TODO) - A paper that seeks to unify and simplify the various module implementations
  * [Advanced Types and Programming Languages; chapter 8](TODO) - Great introduction into module implementation and the theory of modules

You don't need to read all of these (and I desperately need you to finish this blog post). But if you're interested in learning more about modules they are a great reference.

One insight we gain from these papers is that modules are ultimately just a list of definitions. A module contains a series of functions (or types, classes, etc.) that other modules can import and reference. We can see a lot of similarities between modules and classes with their list of methods. In fact a lot of research has been done on the relationship modules and classes have in the OO paradigm. To be effective bags of functions, modules are responsible for two things:

    * Namespacing - control the visibility of it's definitions
    * Packaging - creating self-contained components of code that can be combined arbitrarily

We'll discuss what these mean in more detail, but first I need to introduce some terminology we'll need.

### Strong vs Weak Modules

When we're talking about module implementations it can be helpful to categorize them based on how they handle dependencies. Modules depending on other Modules is core functionality of any Module system. Because of this, it informs a lot decisions in the rest of the implementation. [Backpack](TODO) introduces a helpful distinction between module systems based on how they handle dependencies:

  * Weak - a module must have all dependencies available before it can start compilation.
  * Strong - a module can be compiled in isolation and its dependencies can be provided later

Based on the definition alone, Strong modules sound better. Why would I want to wait on my dependencies if I don't have to? The answer is complexity. Weak modules are, historically, much simpler than Strong modules. The tradeoff for this simplicity is that they prevent a lot of parallelization. If I need all my dependencies available, I have to wait until all my dependencies finish compiling. Conversely, since Strong modules don't require their dependencies, we can compile a module and its dependencies as the same time. We could even compile a module without its dependencies, and get its dependencies from a cache later when we need them.

Of the module systems we've seen so far, only SML/OCaml use Strong modules. Every other module system we've seen uses Weak modules. That's not just a landslide majority, it's so skewed there are folks that have gone their whole careers without seeing or learning about Strong modules. We'll talk some more about why Strong modules have seen such little adoption after we talk about our modules purposes.

### Namespacing
Namespacing is all about determining what code can be seen by other code. It shares a lot of similarity with OO visibility where an object's method can be declared public or private. To manage visibility of code, a module has an export list. Anything in the export list is visible outside the module. Definitions not on the list, cannot be accessed outside the module. We can see how this maps to public and private. Exported symbols are public everything else is private. However, modules can support more sophisticated forms of encapsulation. Rust even goes so far as to allow specifying a module is only visible in another specific module.

This encapsulation through namespacing is critical to modules. Every module system we've talked about supports this. Arguably if your modules don't provide encapsulation, they aren't truly modules.

### Packaging
In contrast to namespacing, most of our module systems do not support packaging (at least using modules). Packaging is concerned with how modules are compiled. All compilers have some concept of a minimal unit of compilation. Borrowing from C/C++ we'll call this a compilation unit. It is the minimal amount of code the compiler can process independently.

Modules that act as compilation units handle packaging well. We can compile a module independently within our compiler and 
