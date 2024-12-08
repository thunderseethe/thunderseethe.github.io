+++
title = "Part 5: Lowering our Base Typed AST"
date = "2024-12-08T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Lowering"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Lowering", "Elaboration", "IR", "System F", "Debruijn Indices"]
description = "Lowering our typed AST into an IR"
+++


## Goals of Lowering

### Why Lower at all
#### Couldn't we just use our typed AST directly

### What is an IR
#### More explicit than our AST
#### Saves work we did in the type checker explicitly.
#### Not as explicit as machine code
#### Allows us to reason about lower level optimizations without committing to a particular target architecture.


## Based on

## Debruijn Indices for Equality
