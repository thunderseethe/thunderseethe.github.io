+++
title = "What's in a Module?"
date = "2023-06-21T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Modules"]
keywords = ["Programming Languages", "Compiler", "Modules", "Namespaces", "Encapsulation"]
description = "A discussion of the overloaded use of modules in languages"
+++

## What's in a Module?

> A module by any other namespace would still encapsulate just as sweet
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

