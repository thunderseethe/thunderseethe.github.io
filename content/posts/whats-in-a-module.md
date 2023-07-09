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
* List of resources on modules (Modules matter most, ATAPL, backpack, mixin modules)
* Modules are responsible for two main things
  * Namespacing
  * Packaging
* Strong vs Weak distinction introduced by backpack paper
    * Strong modules can be compiled in isolation
    * Weak modules have to have all dependencies available before they can be compiled

## Strong vs Weak Modules
Purpose: Most languages support weak modules, but not Strong modules. Strong modules sound strictly superior.

* Weak modules most have all of their dependencies available before they can be compiled.
  * Easier to implement
    * Probably why it is more common
  * Forces some amount of sequential work during compilation
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
    * Whenever a dependency changes it's interface your module breaks

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

## Mixin Modules
Purpose: Mixin Modules provide an improvement over SML/OCaml modules. 

* Signatures are combined with structures to reduce boilerplate. 
* Linking automatically links up a modules dependencies, removing the need for functors and manually wiring up modules.
* Mixin Modules can use applicative semantics allowing for automatic module equality compared to generative module functors.


## Hold off on this as well
## What's in a Module?

> A module by any other namespace would encapsulate just as sweet
>                  - Shakespeare..._mostly_

Modular programming has become a cornerstone of software development. Software projects are only getting larger, and as they do the need to divide up monolithic codebases into self contains chunks becomes indispensable. It is no surprise then, that some kind of module shows up in all modern programming languages we use today, although not always named as such:

  * Java has Packages
  * Haskell has Modules
  * Rust has Modules (_and_ Crates)
  * Python has Modules
  * JavaScript notably lacked Modules, and that turned out so painful they now have [multiple](https://requirejs.org/) [competing](https://github.com/amdjs/amdjs-api/blob/master/AMD.md) [Modules](https://en.wikipedia.org/wiki/CommonJS)
  * C lacks Modules, but C++ has them as of C++20.
  * SML and OCaml have **First-Class** Modules (we'll find out why that's important later!)

Wow, a lot of languages have modules (and what was that about JavaScript having a bunch of module systems?). I'm [designing my own language](series/making-a-language/), so it should also come as no surprise that I'm very interested in module design and implementation. However 

## Trashcan for now
* Designing my own language (TODO: Link series), and so am interested in designing a module system
* Looked to existing languages for guidance on what to put in a module system
* Every language does modules slightly differently:
    * Java packages are all in one flat namespace and don't allow for nesting (they pretend packages nest)
    * Python and JavaScript modules are like runtime objects, where most modules are compile time structures
    * Rust modules can be nested and can reference each other recursively
    * SML's first-class modules support parameterizing a module on other modules

* Module is clearly quite ambiguous and overloaded in programming language terminology.
    * How can there be so little consensus on what a module is, and what it does?
* Okay so if we're not going to be able to figure it out from looking at implementations, where can we look?
* Turns out a lot of places:
  * Bob Harper (one of the creators SML) has written a great blog post about them [Modules Matter Most](https://existentialtype.wordpress.com/2011/04/16/modules-matter-most/)
  * Advanced Topics in Types and Programming Languages has a whole chapter (8) devoted to talking about modules
  * Haskell found their module system lacking, and laid out a more power module system in a paper [Backpack: Retrofitting Haskell with Interfaces
](https://plv.mpi-sws.org/backpack/)
  * [Andreas Rossberg](https://people.mpi-sws.org/~rossberg/) has done a ton of cool work on modules, but I especially want to highlight [Mixin' Up the ML Module System](https://people.mpi-sws.org/~rossberg/mixml/)

* That's a lot to read, and while I'd recommend checking it out, I'm here to digest and summarize for you.
* A module is responsible for two things
    * Namespacing
    * Packaging
* Namespacing
    * Break up a codebase into multiple disjoint scopes
    * Control the public interface of a piece of code
* Packaging
    * A composble unit of compilation
    * Support for separate compilation
        * Separate compilation is where each module can be compiled in parallel independently and then combined later
        * Juxtapose with traditional compilation where all the dependencies of a module have to be compiled before compilation of that module begin
    * Programming against an interface
        * Moduels can be swapped out 
* Examples
    * Rust - modules and crates
        * Modules are just namespaces
        * Crates handle packaging
    * Haskell - modules, self ascribed weak modules
        * Modules are just namespaces
        * Implicit module signatures generated
    * SML - First class modules

