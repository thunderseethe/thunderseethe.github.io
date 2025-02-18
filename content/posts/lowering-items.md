+++
title = "Part 7: Lowering Top Level Items"
date = "2025-02-10T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Lowering"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Lowering", "Elaboration", "IR", "System F", "DeBruijn Index"]
description = "Lowering top level items into our IR. A more elaborate affair than anticipated."
+++

* Intro
  * Lowering items will be saccharine.
  * (What a relief)(replace this with something more specific) to see there's no letter in the title.

* New ItemId
  * We don't reuse `ast::ItemId`
  * Similar to `VarId` (and `Var`) our `ItemId` starts as a 1:1 mapping with `ast::ItemId`, but that will swiftly change.

* `ItemSupply` converts `ast::ItemId` to `ItemId`.
  * Twin to `VarSupply`.
  * TODO: Possibly talk about why this isn't a trait?
    * That might just be vamping for time though.

* No new types
  * Items make great use of our existing types.

* New node: `Item`
  * We cache type in `Item`
  * We could instead look up each item type and avoid the duplication. 
    * But then `type_of` would require taking some form of `HashMap<ItemId, Type>`.
  * We tolerate the `Type` replicas to secure our simple API.  

* Reveal ruse
  * Our lowering code never actually constructs a `TyApp`.
  * I've had you modifying code needlessly this whole time.
  * That is until now.
  * With the introduction of `Item`, `TyApp` finally has a purpose.

* Lower `ItemSource`.

* `lower_ast` new case
  * Handler wrapper


