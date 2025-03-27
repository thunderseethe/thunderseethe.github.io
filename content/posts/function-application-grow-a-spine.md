+++
title = "Function Application Needs to Grow a Spine Already"
date = "2025-03-18T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Type Inference"]
keywords = ["Programming Languages", "Type Inference", "Functional", "Impredicativity", "Higher-Rank types", "Function Application", "Spine", "Lambda Calculus"]
description = "Typechecking multiple applications at a time opens new opportunities for inferring types"
+++

Function application can be found nestled into the heart of basically every functional language. 
At the risk of aggrandizing, I would even say every programming language.
Unlike languages inheriting from the C family, function application in functional languages makes use of [currying](https://en.wikipedia.org/wiki/Currying).
Heralding all the way back to the dawn of the lambda calculus:

```hs
data LambdaCalc 
  = Var String
  | App LambdaCalc LambdaCalc -- <-- our boi
  | Fun String LambdaCalc
```

Many languages have been built atop the sacrosanct application: one argument applied to one function.
Every time we see it, it inspires the same feelings in all of us:

* Weak
* Feckless
* Inept INVERTEB--

What's that?
People don't feel that strongly about AST nodes?
They care way more about keyword length?
Alright, fair enough.
Personally I'm partial to the keyword `fun`.
A length of 3 is true, just, and it's literally fun.

## A Quick Look at Currying

Function application is integral to functional programming.
Curried function application is a long-standing tradition in functional languages.
So it might come as a surprise, that not currying your function, taking multiple parameters at a time, allows you to infer better types then just considering one application at a time.

Before we understand why, let's take a step back and talk about currying.
Currying is a feature that turns multi argument functions into a series of single argument functions.
Each function taking one parameter at a time and returning a new function that takes the next parameter.
Instead of a single application `f(x, y)`, we have two applications `(f x) y`.
`f x` returns a new function that takes our `y`.
This chain of applications `(. x) y` is called the application's spine.
`f` is called the head of the application.

For how common currying is in functional languages, it is quite divisive.
Some people swear by currying as elevating programming above mundane concerns such as function arity.
Others say it makes it impossible to reason about programs. 
You can never tell when a function does real work, rather than just returning a new function immediately.

At a glance, we might guess that `f` and `g` in `f (g x)` are functions of one parameter.
If we peak at `f`'s definition, `f x y = x y`, we discover that actually both `f` and `g` take two parameters.
In a language without currying, we'd have to write `f (g x)` as `(\y -> f (\y -> g x y) y)` making parameter count exceedingly obvious.

On the upside of currying, it can improve readability by removing noise.
Writing `map (update tallyValue) tallies` cuts right to the heart of matters and doesn't require any superfluous names.
This is subjective of course, but in my opinion currying can help highlight what's important by removing that which is not.

More objectively, it simplifies things in the static analysis of a language.
When we encounter an application `f x`, we check if `f` is a function.
If it is, we're good.
If it's not, we immediately have an error.
Compiler passes like typechecking and inlining benefit from this simplicity.

This scales pleasantly to applying multiple arguments.
When we encounter `(f x) y`, we recurse to check `f x` and then check that `y` is the right argument for `f x`.
Our application node, `App LambdaCalc LambdaCalc`, is correct by construction.
If we see an application, we know we have one argument and one function.

With multiple arguments we have to worry about do we have the right number of arguments and are the right types at the right positions in the function call.
It's not the end of the world, but certainly more involved to achieve the same end.
Multiple arguments also force some verbosity onto us.
If we want to pass a function with some arguments already applied, we must use a closure.
Instead of saying `map (f 4) ys`, we're forced to say the more verbose `map (\y -> f 4 y) ys`.

Classic tradeoffs apply here, of course, smart people will look at `map (\y -> f 4 y) ys` and say that its good to require the closure because it makes the runtime characteristics of the program more obvious.
Because we have to use a closure, it's apparent to everyone reading that a closure is being allocated.
`(f 4)` is still allocating (not a closure but a partial application or Pap), but that's no longer immediately apparent.
We have to go look at `f` to figure out that it needs more arguments and isn't returning a value immediately.

While they're not wrong, I harbor a fondness for currying.
I miss it whenever I find myself using Rust.
When I return to Haskell, I delight in indulging currying to construct [pointless](https://en.wikipedia.org/wiki/Tacit_programming) programs.
[`(.:.)`](https://hackage.haskell.org/package/composition-1.0.2.2/docs/Data-Composition.html#v:.:.) my beloved.

## A Point Against Currying

I'm here today, however, to place another point in the pile against currying.
Much as it pains me to do so.
I've noticed a trend in recent research that favors passing multiple arguments at a time.
Typechecking multiple arguments to a function gives us enough information to infer polytypes, sometimes.
Sounds great, but what's a polytype? 

When we talk about types we distinguish between two _types_ of type: monotypes and polytypes.
Monotypes are free to contain any number of type variables, but they cannot introduce them (aka bind them).
These are some of your favorite types like `Int`, `a -> a`, `Map k (Int, v)`, etc.
Polytypes are monotypes but free to introduce any number of type variables.
A classic example of a polytype is the type of `id`, which takes any type `a` and returns a value of that type:

```hs
id :: forall a. a -> a
id x = x
```

The `forall a` there introduces our type variable `a` and makes `forall a. a -> a` a polytype.
Haskell allows omitting the `forall` and just writing `a -> a`, but this is still a polytype when it appears as a top level annotation.
Haskell allows syntax sugar to avoid explicitly introducing `a`. 
We'll avoid that syntax sugar so our monotypes and polytypes are clearly distinguishable.

Polytypes, however, can put the `forall` in more interesting places as well:

* `forall a . a -> (a, forall b . b -> b)`
* `[forall x . x -> x]`
* `forall k . Map k (forall v . (Show v) => Maybe v)`

These are also all polytypes.
Except, unlike `id`, we've historically been unable to infer these polytypes.

At the end of inferring a top level declaration, like `id`, we have a monotype such as `a -> a`.
We find all the free variables in this monotype and bind them to create our polytype `forall a. a -> a`.
In a sense, this is inferring a polytype.
You provide a top level definition, and type inference returns its polytype.

The important distinction, however, is that this process never requires solving a type variable with a polytype.
At the end of inference we've only solved type variables with monotypes.
There's quite a lot that goes into type inference that I'm brushing over here, if you're curious about the details see [this post](/posts/type-inference/).
For our purposes today, it suffices to know that it's not possible to infer types with arbitrary foralls such as `forall a . a -> (forall b. b -> (a, b))`.

[This](https://dl.acm.org/doi/abs/10.1145/2641638.2641653) [is](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/practical-type-inference-for-arbitraryrank-types/5339FB9DAB968768874D4C20FA6F8CB6) [not](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/hmf.pdf) [for](https://www.microsoft.com/en-us/research/wp-content/uploads/2017/07/impredicative-pldi18.pdf) [lack](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/boxy-icfp.pdf) [of](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/fph-long.pdf) [trying](https://dl.acm.org/doi/abs/10.1145/3385412.3386003).
Many attempts have been made to allow for inference of arbitrary polytypes.
Even with the approach we're talking about today we can infer all polytypes, just some of them.
Inferring all polytypes requires annotations to disambiguate where `forall`s should appear.

A recent line of research, however, shows that if you make use of multiple arguments during inference you can infer polytypes without annotations:

  * [A quick look at impredicativity](https://www.microsoft.com/en-us/research/wp-content/uploads/2020/01/quick-look.pdf)

Interestingly enough, the idea appears to be independently applied in another paper:

  * [Sound and Complete Bidirectional Typechecking for
Higher-Rank Polymorphism with Existentials and
Indexed Types](https://www.cs.tufts.edu/~nr/cs257/archive/neel-krishnaswami/bidirectional-gadt.pdf)

It's always neat to see when disparate work converges on the same solution out of a set of shared constraints.
Finally, there's also a great talk that covers the idea if you'd rather learn in video format:

  * [Type inference for application spines](https://youtu.be/suktIDmFAbk?si=5jo7OI6hUhQSX5pd)

At a high level, by examining the arguments to a function we can sometimes glean enough information to infer a polytype.
The advantage of this approach, over previous attempts, is its lightweight and doesn't require annotations.
If we don't gather enough information, we're free to bail out and do inference normally with monotypes.

## Turns Out It's About Type Inference

Traditionally, type checking an application starts by inferring a function type.
Commonly this will be a function type with two variables `a -> b`, which we use to check our argument has type `a`.
Our new quick look approach proceeds in the opposite direction.
We collect all the types of our arguments and then use them to check our function type.
If our argument types provide enough information we can check our function has a polytype, rather than a monotype.

Some, admittedly contrived, examples from [A quick look at impredicativity](https://www.microsoft.com/en-us/research/wp-content/uploads/2020/01/quick-look.pdf) will help us grasp the idea.
Consider a list of `id` functions (don't ask why I need a list of `id` functions I assure you its important business logic!):

```hs
ids :: [forall a . a -> a]
ids = [id, id, id]
```

Let's say we want to add a new `id` to the list with `id : ids`.
Technically this is a function call with two parameters `(:) id ids`.
Where `(:)` has type:

```hs
(:) :: forall p . p -> [p] -> [p]
x : xs = -- the implementation doesn't matter
```

With normal inference, we have to give `id : ids` the type `[a -> a]`.
We're only allowed to solve `p` to a monotype, so we have to pick the monotype `a -> a`.
But we've lost important information, notably that the `id`s in our list work for different types.
They don't all have to be applied to the same type `a`.

By examining both our arguments, `id` and `ids`, we can determine a more specific type for `id : ids`.
We can deconstruct our application into its head `(:)`, and it's list of arguments `[id, ids]`.
This list of arguments is called the application's spine.
We can match up the argument types of `(:)`, `p` and `[p]`, with the types of our argument spines, `forall a . a -> a` and `[forall a . a -> a]`.

Our first match, `p` and `forall a . a -> a`, proves unfruitful.
[Quick look](https://www.microsoft.com/en-us/research/wp-content/uploads/2020/01/quick-look.pdf) details the technical reasons why, but we can't solve a naked type variable to a polytype `forall a . a -> a`.
Doing so makes typechecking undecidable, and people generally aren't willing to wait that long for their types.
Fortunately, our vertebrate application provides a second match to consider: `[p]` and `[forall a . a -> a]`.
With this pairing we can determine that `p` must be `forall a . a -> a`.

The reason for this is a little subtle.
Dressing `p` in `[p]` makes it unambiguous where the `forall` must go.
In contrast to our first pair `p` and `forall a . a -> a`, where the `forall` has two valid placements.
If we only had our first argument `(:) id`, our term would have two valid typings:

* `forall a . [a -> a] -> [a -> a]`
* `[forall a . a -> a] -> [forall a . a -> a]`

Our poor type inference doesn't have enough information to tell which type to choose.
But once we see `[forall a . a -> a]`, we can be certain only the second typing applies.

Multi argument application is instrumental to allowing this enhanced inference to take place on more terms.
Our approach relies on two critical pieces of information provided by our application spine:

* A polytype for our application head, `forall p . p -> [p] -> [p]` in the case of `(:)`.
* The type of each argument to match against our head's polytype.

We might suspect that we can determine these two things without application spines.
When we consider `((:) id) ids` we can give `(:) id` the type `forall f . [f] -> [f]` and then match it against `ids`'s type `[forall a . a -> a]`.
It's not valid, however, to infer `forall f . [f] -> [f]` for `(:) id`.
`(:) id` is an application node, it has to have a monotype.
We only get away with giving a polytype to `(:)` because it's a bound variable.

Bound variables can have polytypes because variables don't require any inference.
Again this runs into intricacies in how type inference is implemented.
Type inference works out a type for each bound variable in scope and save it in an environment.
All we have to do when we see `(:)` is lookup its type in that environment.

We also might wonder if we really need a polytype for our application head.
If `(:)` has the type `p -> [p] -> [p]` it looks like our approach would work just as well.
Our `p` looks the same but is distinct from the `p` in `forall p . p -> [p] -> [p]`.
The `forall` ensures us that `p` doesn't show up anywhere else in the expression we're currently inferring.
Without that guarantee, it's not safe to solve our type variables to polytypes. 
Our variable might be used elsewhere and lead to us accidentally inferring a polytype unexpectedly.

This leads us to an important caveat where this approach does not apply.
If the head of our application spine is a more complicated expression than a variable, we can't apply this tactic.
For example if instead of `(:) id ids`, we had `(\ x y -> (:) x y) id ids` that would stop us in our tracks.
`(\x y -> x y)` has to be given a monotype, not a polytype, and that prevents us from inferring a polytype.

I worry opportunities to apply quick look won't arise in practice.
We need our applications be headed by a variable and contain enough arguments to unambiguously determine a polytype.
Thankfully, not a lot of people write code like `(\ x y -> (:) x y) id ids` in practice.

Most applications have the shape we need, some number of applications with a variable at the head.
Some number of arguments can be one (it can even be zero), consider `head ids`.
Our single argument `ids` is enough to determine that the type of `head` should be `[forall a . a -> a] -> (forall a . a -> a)`.
If our application doesn't have that shape, we ship it off to normal inference to receive its monotype.
Because we can always fall back to normal inference, there isn't a lot to be lost by trying this and seeing if it works.

I don't know if this is enough for me to forsake currying entirely.
I viewed the trusty `App LambdaCalc LambdaCalc` node as kind of the default for functional languages (in no small part due to Haskell being my formative functional language).
I'm rethinking my view now that application spines are beneficial not only practically but also theoretically.
Maybe functional languages should rethink application moving forward and consider outright multi argument applications ala `f(x, y, z)`. 
SML already has the convention that multiple parameters should be passed in a tuple rather than use currying: `f (x, y, z)`

Okay that's a little too far, I love currying too much for that.
But at least consider explicit currying.
That makes the multi argument structure immediately obvious.
The advantages of the application spine can't be ignored.
