+++
title = "Part 1: Type-Driven Design"
date = "2023-06-21T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Modules"]
keywords = ["Programming Languages", "Compiler", "Modules", "Namespaces", "Encapsulation"]
description = "A discussion of the overloaded use of modules in languages"
+++

## What's in a Module

> A module by any other namespace would still encapsulate just as sweet
>                  - Shakespeare?

* Module is an overloaded term
* Most languages have a concept of modules
    * A thing folks call module, even if the language itself does not
    * Not all these modules are the same, in fact they're drastically different
* So what is a module?
* A module is responsible for two things
    * Namespacing (Encapsulation)
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

