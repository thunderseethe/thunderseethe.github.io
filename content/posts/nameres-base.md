+++
title = "WIP Nameres"
date = "2025-11-12T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Name Resolution"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Name Resolution", "Scope", "Lexical Scoping", "Abstract Syntax Tree", "Error Recovery"]
description = "WIP Description"
+++

* What do we mean when we reference a Name?
  * References a value
  * Names reused
  * Based on context names change meaning

* Name Resolution
  * Names are helpful for humans, but not the compiler.
  * Resolution figures out what each name references and saves in a form that is easy to use downstream.
  * Why isn't it obvious what a name means?

* Shadowing
  * We allow for variables to shadow each other
  * Example code
  * One name shadows the other
  * Explain how this relates to scoping

* Shadowing tradeoffs
  * Shadowing complicates the interpretation of a name.
  * Shadowing avoids having to make up new names for intermediary values.
  * Shadowing marks when a value is no longer used.

* Scoping
  * Lexical vs Dynamic
  * We use Lexical
  * Value of names are determined by the syntax in the source file

* Shared State
  * `NameResolution`
    * `supply` produces our `Var`s as we need them.
    * `names` maps our `Var`s back to the names they were given in syntax.
    * `errors` accumulates 



