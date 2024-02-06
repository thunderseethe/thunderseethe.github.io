+++
title = "I'm betting on Call-by-Push-Value"
date = "2024-01-09T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages"]
keywords = ["Programming Languages", "Lazy", "Eager", "Call-by-value", "Call-by-name", "Call-by-need", "Call-by-push-value", "Evaluation Order", "Evaluation Strategy"]
description = "Why call-by-push-value is the best evaluation strategy, and why its going to take over"
+++

I have a function argument here.
Let's bet on the best way to evaluate it.
We can either evaluate it right now, or wait until it's passed to its function and evaluate it when it's actually used.
Whichever is faster wins the bet.

We might suspect this is a rather boring bet.
One strategy or the other is always faster, and you can always win the bet if you can figure out which one.
Fortunately for our gamble (and to the dismay of theorists everywhere), this is not the case.
We can always construct a situation where evaluating either eagerly or lazily is better.

Our gamble touches on an age-old question in programming language semantics, to [call-by-value]() (CBV) or to [call-by-name/call-by-need]() (CBN).
These are both evaluation strategies they determine the order expressions get evaluated.
CBV always evaluates a function argument before passing it to a function, often this is called eager evaluation.
CBN waits to evaluate function arguments until they are used in the function body, often this is called lazy evaluation.

Languages pick one and use it for all their function applications.
Rust, Java, JavaScript, Python, and C/C++ use CBV as their evaluation strategy.
Haskell and...uh...  
Haskell uses CBN for their evaluation strategy.
Alas, whichever one you pick the grass is always greener on the other side.
Our CBV languages introduce little spurts of lazy evaluation (closures, iterators, etc.). 
Our CBN language(s) introduce eager evaluation; data types usually want to be eagerly evaluated.

# Call By Push Value
So if both CBV and CBN languages end up wanting both eager and lazy evaluation, why are we forced to pick one and forego the other entirely?
It turns out we're not, we just didn't know that yet when we designed all those languages, whoops.
Levy's '99 paper [Call by Push Value: A Subsuming Paradigm](https://dl.acm.org/doi/10.5555/645894.671755) introduces a third option, Call-by-Push-Value (CBPV), to the CBV/CBN dichotomy.
'99 is basically the Cretaceous era in JavaScript years, but it's quite recent by research standards.
CBPV has just started to penetrate the zeitgeist, but I think it's a really promising field of research.
I'm betting it's going to be big in the future, and I'm excited to tell you why.

Before we can talk about why CBPV is cool, we have to talk about what it is.
The big idea of CBPV is to support both CBV and CBN with one set of semantics.
It accomplishes this by distinguishing between values and computations in the language semantics.
The paper provides a nice slogan to capture the intuition: "a computation does, a value is".

Great, but what does that actually look like?
Let's look at a traditional lambda calculus, and we can contrast that to our CBPV lambda calculus:

```hs
data Type 
    | Int
    | Fun Type Type

data Term
    = Var Text
    | Int Int
    | Fun Text Term
    | App Term Term
```

Depending on how we execute `App` this can be either CBV or CBN (but not both).
Our values are all mixed up with our computations as one term.
CBPV lambda calculus fixes this by sundering term and type in two:

```hs
data ValType 
    = Int
    | Thunk CompType

data CompType 
    = Fun ValType CompType -- !!
    | Produce ValType

data Value
    = Int Int
    | Var Text
    | Thunk Comp

data Comp
    = Fun Text Comp
    | App Comp Value
    | Produce Value
```

Wow, that's a lot more stuff, but it's not exceedingly clear what's a value and what's a computation.
One surprising thing is that variables are a value. 
What if our variable is bound to a computation?
We don't have to worry about it.

If we look at our new `App` node it can only apply a value.
When we want to apply a computation we first have to turn it into a value using `Thunk`.
Conversely, `Fun` only admits a computation as it's body.
If we want to use a value as our function body, we have to turn it into a computation with `Produce`.

# The Bet

Now that we know what CBPV _is_, we can finally talk about why CBPV _is...the future_.
* Flesh this out, this is the main point of the article so should be stronger
* Both the claim and tone need to be more intense here
* The next big thing.

CBPV, or ideas of lawsuit likeness to CBPV, show up in 3 separate domains:

  * Algebraic Effects
  * [Implicit Polarized F: Local Type Inference for Impredicativity](https://www.cl.cam.ac.uk/~nk480/implicit-polarized-f.pdf)
  * [Kinds Are Calling Conventions](https://www.microsoft.com/en-us/research/uploads/prod/2020/03/kacc.pdf)

Any idea that shows up that many times in the wild has to be onto something.
There's something to splitting up values and computations.
Let's look at our 3 data points to find out what.

# Algebraic Effects

Algebraic effects are concerned with tracking side effects in types.
An issue you encounter immediately upon trying to do this, is where can effects happen?
Functions can do effects sure, that's easy.
What about records, can they do effects?
Well that seems kind of silly, so no.
But what if the record contains a function that does an effect, what then?

CBPV deftly dispatches these quandaries.
Effects appear on any computation type, and cannot occur on value types.
With CBPV our record can't actually hold a function, only a closure of a function.
To get back our function we have to force the closure, and then we're in a computation, that's effect city.

# Implicit Polarized (System) F
This one has a daunting name.
It's talking about type inference (a subject [we're well versed in](/posts/type-inference/)).
A timeworn tradeoff for type infer-ers is generic types.
If you allow a generic type to be inferred to be another generic type, your type inference is undecidable.
This puts us in a bind though. 
A lot of cool types happen involve these nested generics (called Rank-2, Rank-N, or Impredicative types), and if we can't infer them we're forced to write them down by hand.
No one can be bothered to do that, so the types go sorely under-utilized.

This paper seeks to make a dent in that problem by allowing us to infer these types, sometimes.
Sometimes may seem underwhelming, but you have to consider it's infinitely better than never.
It does this with, you guessed it, CBPV.
Call by push value makes it explicit when a function is saturated vs when it returns a closure.
This turns out to be vital for type inference.
Saturated function calls have all their arguments and these arguments can provide enough information to infer our function type.
Even when our function type includes nested generics.
That's quite exciting! 
All of a sudden code no longer requires annotations because we made a smarter choice in language semantics.

# Kinds Are Calling Conventions

Kinds Are Calling Conventions is a fascinating paper. 
It employs kinds to solve issues that have plagued excessively generic languages since the first beta redux:

  * Representation - is my type boxed or unboxed
  * Levity - is a generic argument evaluated lazily or eagerly
  * Arity - how many arguments does a generic function take before doing real work

To solve these issues, types are given more sophisticated kinds.
Instead of type `Int` having kind `TYPE`, it would have kind `TYPE Ptr`.
Similarly, we ascribe the type `Int -> Int -> Int` the kind `TYPE Call[Int, Int]`.
Denoting that it is a function of arity 2 with its kind.
This is where CBPV enters the story.

To be able to provide the arity in a type's kind, we first have to know a functions arity.
This can be tricky in CBV or CBN languages that freely interchange functions and closures.
CBPV solves precisely this issue (albeit for different reasons).
Because functions aren't values, our types distinguish whether we're a function of arity 2 or a function of arity 1 that returns a closure:

```
unary :: Fun Int (Fun Int (Produce Int))
```
vs
```
binary :: Fun Int (Produce (Thunk (Fun Int (Produce Int))))
```

CBPV makes it abundantly clear what the arity of any function is, just from glancing at its type.
Kinds Are Calling conventions utilizes this to great effect to emit efficient calling code for higher order functions.
The paper also makes use of the fact that CBPV admits both eager and lazy evaluation to track how an argument is evaluated in the kind.
All in service of generating more efficient machine code.
Who could've guessed such a theoretical approach would serve such pragmatic goals.

If I had a nickel for every time CBPV shows up in the wild, I'd have 3 nickels.
That's not a lot, but it's weird that it happened 3 times.
Personally, I believe this is because CBPV hits upon a kernel of truth in the universe.
Being explicit about what's a computation and what's a value opens the door to reason about more programs.

For this reason, I'm laying my cards on the table.
Mark my words, CBPV is going to be big.
I'm betting all my internet points, come this time next decade the next generation of languages will be built on CBPV.


