+++
title = "Traits are a Local Maxima"
date = "2023-07-31T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Type Classes"]
keywords = ["Programming Languages", "Compiler", "Type Classes", "Traits", "Parametric Polymorphism", "Local Coherence", "Global Coherence"]
description = "TODO"
+++

* What's a trait
  * Should they be tied to typeclasses?
* What's the problem with traits
  * Orphan Instances
  * Anti-modular
* Coherence
  * Traits rely on Global Coherence
  * Explain global coherence
  * Local coherence instead of global coherence
  * Explain local coherence
* Fixes the issues with traits
 * No orphan intance
   * Figure out what X is
   * Really highlight X
 * Modular
* What do local coherence solutions look like
  * Modular Implicits
  * Scala Implicits
  * COCHIS (This is the one we like)
* Local coherence raises issues of it's own
  * Coherence
  * Stability
  * Set problem (two Sets must share the same Ord instance)
* COCHIS
  * Implicit bindings and implicit application
  * TODO: Flesh out what we to say about COCHIS
   * What is it?
   * Why is it cool?
* Walkthrough how cochis encodes type classes
  * This might be too long to be worth including (?)
  * Intro syntax
  * Encode `Ord` type class as struct and implicit in `cmp`
```
type Ord a = { le : a -> a -> Bool }
let cmp : forall a . Ord a => a -> a -> Bool = (?: Ord a).le
```
  * Intro "instances" as records of type `Ord`
```
let ordInt : Ord Int = { le = \x y. primIntLe x y }
let ordChar : Ord Char = { le = \x y. primCharLe x y }
let ordPair : forall a b . Ord a => Ord b => Ord (a, b) = 
    { le = \(a0, b0) (a1, b1). cmp a0 a1 && ((not (cmp a0 a1)) || cmp b0 b1) }
```
  * Install our "instances" as implicits and use them
```
let sort : forall a . Ord a => List a -> List a = //...you know how to sort
implicit ordInt in
  implicit ordChar in
    implicit OrdPair in
      sort [(3, 'a'), (2, 'c'), (3, 'b')]
```
  * This requires more boilerplate than typeclasses
  * We can alleviate boilerplate with syntax sugar
    * Surface language could have a construct that imports and installs an instance for an entire file

* Build on previous example to explain how implicits are more powerful than typeclasses
  * We can have multiple instances in a row
  * Because implicits are manually installed we can compile units separately

* NOTE: COCHIS employs a construct to let bind an implicit
  * In practice what does it look like to use this in a language 
  * It's rare I want to install an implicit locally
  * I'd rather install it in some file-based scope (with the option to override locally using the normal construct)
  * Need to figure out if this is tenable
  * Maybe not worth touching on since this is just explaining cochis as compared to typeclasses.

* Issues with COCHIS
  * Talk about what's required to make resolution deterministic
  * Bad example
    * Stability under inlining
  
