+++
title = "Part 4: Checking Top Level Items"
date = "2024-06-08T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Type Inference"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Type Inference", "Bidirectional Typechecking", "Unification", "Constraint Solving", "Skolemization", "Rigid Type Variables", "Type Checking", "Top Level Items", "Top Level Definitions"]
description = "Adding support for annotated top level items to our type checker"
+++

# Introduction

TODO: Outlining

* Check top level item defs

* Should be easy, we already have our `check` function

* Illustrate issue with unification where we can solve user-specified generics

* Explain difference between type/row variables and unification variables

* Remedy the issue
    * What are the code changes we make to do this?
    * Talk about substitution changes
    * Talk about why we introduce a new type for unification variables

