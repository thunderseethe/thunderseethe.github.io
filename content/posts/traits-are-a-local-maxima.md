+++
title = "Traits are a Local Maxima"
date = "2023-07-31T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Type Classes"]
keywords = ["Programming Languages", "Compiler", "Type Classes", "Traits", "Parametric Polymorphism", "Local Coherence", "Global Coherence"]
description = "TODO"
+++

* Today we're talking about [Rust traits](TODO).
* As you might've guessed from the title, we will not be talking about them positively.
* Woah woah, hang on, put down your pitchfork.
* A local maxima is still a maxima, I love traits as much as anyone.

Traits are one of the few programming language concepts beloved enough to earn multiple names.
You might have heard about typeclasses in Haskell, or protocols in Swift.
Heck, if the teacher's not watching, even interfaces can be considered a kind of trait.
Elm famously left out traits, and it was so requested they have an [FAQ](https://faq.elm-community.org/#does-elm-have-ad-hoc-polymorphism-or-typeclasses) explaining their absence.

Traits have a rich history dating back to 80s.
How could such a mature and cherished feature have problems?
Especially, problems substantial enough to warrant a blog post?
I'm so glad you asked.

## The problem with Traits

We can elucidate the first of our woes by conjuring some contrived Rust code:

```rs
use a_crate::A;
use b_crate::BTrait;

fn frobinate<T: BTrait>(frob: T) -> usize {
  // Call a method of our trait
  let mut value = frob.random_number();
  // ... forbinate value
  value
}

fn somewhere_else(a: A) {
  //...doing stuff
  let x = frobinate(a);
  //...off to do more stuff
}
```

We have two dependencies `a_crate` and `b_crate` (we'll worry about naming another day).
One of them provides us a type `A`, and the other a trait `BTrait`.
Our crates job is to frobinate things, and we'd like to frobinate `A`s.
Clearly the code above frobinates an `A`.
Alas, a problem arises when we go to compile:

```
error[E0277]: the trait bound `A: BTrait` is not satisfied
  --> src/main.rs:FC:23
   |
FC |     let x = frobinate(a);
   |             --------- ^ the trait `BTrait` is not implemented for `A`
   |             |
   |             required by a bound introduced by this call
   |
```
What a descriptive error message, thanks Rust. 
We know how to solve that issue, implement `BTrait` for `A`:

```rs
impl BTrait for A {
  fn random_number(&self) -> usize {
    4 // chosen by fair dice roll. 
  }
}
```
Easy, and guaranteed to be random.
Unfortunately as we attempt to get back to frobinating we find another issue:

```
error[E0117]: only traits defined in the current crate can be implemented for types defined outside of the crate
  --> src/main.rs:FF:1
   |
FF | impl BTrait for A {
   | ^^^^^^^^^^^^^^^^-
   | |               |
   | |               `A` is not defined in the current crate
   | impl doesn't use only types from inside the current crate
   |
   = note: define and implement a trait or new type instead
```

Wow, this error message even tells us how to fix itself.
I'd pay good money to be able to tell you how to fix myself.
Our helpful error message is telling us our implementation isn't valid.
Not due to anything implicit to the `impl` mind you, but simply because of where it resides.

This is one of our fatal flaws with traits.
A trait implementation must be either in the crate that defines the type, or the crate that defines the trait.
This error isn't the end of the world, it even tells us it'll go away if we make a new type.
We can create a new and improved type `A2` in our current crate to implement our trait:

```rs
struct A2(A);
impl BTrait for A2 {
  fn random_number(&self) -> usize {
    4 // chosen by fair dice roll, still!
  }
}
```

I'm sure it won't take much to convince you, this is unsatisfying.
It's straightforward in our contrived example.
In real world code, it is not always so straightforward to wrap a type.
Even if it is, are we supposed to wrap every type for every trait implementation we might need?
People love traits, that could be a lot of pairings. 
Wrapper types themselves aren't free either, `a_crate` has no idea `A2` exists. 
We'll have to unwrap our `A2` back into an `A` anytime we want to pass it to code in `a_crate`.
Now we have to maintain all this just to add our innocent implementation.

Haskell christens this the [orphan instance](https://wiki.haskell.org/Orphan_instance) problem.
It's a well known downside to traits, dating back to their invention.
If that hasn't convinced you, check out this [post](https://smallcultfollowing.com/babysteps/blog/2022/04/17/coherence-and-crate-level-where-clauses/) from Niko (of Rust fame) about why orphan instances pose a problem in Rust.
Now we know what the problem is.
But let's put our empathy to use, and ask why the problem is.

What breaks when we allow orphan instances?
Reimagine our `a_crate`/`b_crate` example with orphan instances allowed.
Our `frobinate` library implements `BTrait` for `A`, and we stop worrying about it.
Now let's say someone downstream needs to `frobinate` stuff, so they depend on our library.
They also need to `swizzle` things, so they pull in a dependency on a `swizzle` library.

Oh no! `swizzle` ran into the same problem as `frobinate`, and solved it the same way by implementing `BTrait` for `A`.
Now our poor downstream user has two instances of `BTrait` for `A`.
What is the compiler supposed to do here?
Removing an instance won't work.
The user can't modify either of their dependencies (they don't own the code).
It's not safe to just pick one of the implementations to use. 
`frobinate` won't necessarily work as expected with `swizzle`'s implementation of `BTrait` for `A` (or vice versa).
Our only hope is to prevent this situation from arising in the first place, hence why orphan instances are not allowed.

## Global Coherence

This leads us to a key requirement of traits: there can only ever be one instance of a Trait for a Type.
Enforcing a single implementation of a trait for a type is important for maintaining _coherence_, sorry for the technical term.
Skimming over _a lot_ of details, coherence is a property of well-behaved programs.
It states that no matter what trait implementation we pick (or what order we type check our code), our program behaves the same way.
We like this property as users of the language.
If our typechecker decided to pick a different implementation one day, and suddenly our otherwise unchanged code executed differently we'd be quite surprised.

As you might imagine, enforcing one implementation per trait/type combo makes it very easy to ensure coherence.
If our compiler only ever has one implementation to pick from, it's free to pick any implementation and behavior never changes.
Kind of feels like cheating.
Lack of orphan instances isn't just a pain point as a user.

Maintaining one implementation per trait/type combo presents a problem on the language implementation side, as well.
As a compiler if I want to make sure that I don't have overlapping implementations, I open up every module and check if any of them implement the same trait for the same type.
Brushing aside any concerns about `O(n^2)` trait/type pairings, "open up every module" sends chills down the spine of any compiler interested in compiling by the end of this century.
While this is the technically correct thing to do, this is intractable in practice.
In fact, this presents such a performance problem, GHC simply [doesn't](https://gitlab.haskell.org/ghc/ghc/-/issues/2356).
It's better to allow overlapping implementations to sneak into your codebase rather than force the compiler to touch every module to keep them out.

We're now more well versed in some of the issues around traits, and we even got to learn some fun terminology.
If you'll notice a running theme in our issues, they all stem from traits requiring a kind of global consensus.
I'm going to call enforcement of coherence in this way, global coherence.
Because we're required to construct a global set of traits to determine if we're coherent or not.
[This blogpost](http://blog.ezyang.com/2014/07/type-classes-confluence-coherence-global-uniqueness/) explains this is actually a perversion of terminology, please excuse my transgression and stick with me.
It's obvious from here, if global coherence is the problem, local coherence is the solution.
Now all we have to do, is figure out if, and what, local coherence is.

Our quest is set, we seek the mythical local coherence.
As a jumping off point, what would local coherence even look like?
Global coherence requires that we have a unique implementation in the global scope of our program.
Conversely, it makes sense that local coherence would instead require a unique implementation in a local scope...for some meaning of local and scope.
By loosening the global restriction, we're allowed to introduce overlapping implementations.
As long as we don't import the overlapping instances in the same scope.

Going back to our `frobinate` and `swizzle` example, local coherence solves our problem.
`frobinate` and `swizzle` each have their own implementation of `BTrait`, and as long as they don't import each other everything works. 
Great!
TODO: I should explain implicits better, or atleast introduce the overlap between implicits and typeclasses that they both lookup implementations.
I can't be the first one to have thought of this right?

* [Modular Implicits](https://arxiv.org/pdf/1512.01895)

Oh, neat see I knew I couldn't be-

* [Scala Implicits](https://docs.scala-lang.org/tour/implicit-parameters.html)
* [Agda Implicits](https://agda.readthedocs.io/en/latest/language/implicit-arguments.html)
* [Lean Implicits](https://lean-lang.org/lean4/doc/implicit.html)
* [COCHIS: Stable and Coherent Implicits](https://i.cs.hku.hk/~bruno/papers/JFPImplicits.pdf)

Okay okay, I get it, excuse my ignorance, geez. 
Clearly this is not a new idea, everyone has already settled on calling it implicits.
While there are a lot of examples of implicits out there, the ideas haven't made their way into the mainstream yet.
Modular implicits in OCaml and Implicits in Scala are the most popular on this list (you can pick which one is the most popular), and both are niche compared to the Rust userbase.
There implicit implementations also leave something to be desired.

On the OCaml side, modular implicits haven't actually been implemented in the language.
[The paper](https://arxiv.org/pdf/1512.01895) lists some of the open design considerations that remain to be solved.
To summarize, once you allow local implementations, type checking becomes order dependent between typechecking and resolving implicits.

On Scala's end implicits are unstable.
Martin Odersky (core contributor and one of the original developers of Scala) gave a great keynote on some of the issues with Scala implicits: [What to leave implicit](https://www.youtube.com/watch?v=br6035SKu-0).
To skip a lot of detail, unstable implicits mean that are program might behave differently depending on how we infer types or inline terms.
This can cause headaches for end users (and compilers), as generally we expect our program to behave the same way regardless of things like inlining.

## Stable and coherent Implicits

The final paper on that list [COCHIS](https://i.cs.hku.hk/~bruno/papers/JFPImplicits.pdf) seeks to solve these issues with their _Calculus of Coherent Implicits_ (say that 3 times fast).
TODO: Explain what I like about COCHIS.

COCHIS employs 3 new constructs to implement their implicits: 

* Query: `?Int`
* Rule function: `|?Int| <body>`
* Rule application: `<implicit> with <argument>`

This might a look a similar to the 3 cases of our good friend: the lambda calculus.

* A query looks up the nearest lexically bound implicit value by type, similar to a lambda variable.
* A rule function binds an implicit value for a type in the scope of `<body>`, similar to a lambda function binding a variable.
* A rule application provides a value of the right type for a rule function, similar to a lambda application.

Also like the lambda calculus, we can introduce a `let` binding construct as syntax sugar:

```
implicit <argument> in <body> = (\?T. <body>) with <argument>
```

where `T` is the type of our argument. 
We'll adopt this syntax sugar to make our examples shorter.

Next let's take a look at how we can use COCHIS' constructs to replicate trait behavior.
We're going to translate the `Ord` trait to COCHIS implicits (this is an example from [the paper](https://i.cs.hku.hk/~bruno/papers/JFPImplicits.pdf) we're retooling, find more details there).
The first step on our journey, is to create a value to represent our implementation of the `Ord` trait.
This is already a departure from traits, where we ask for a `T: Ord`, and then we're able to order `T`s willy-nilly.
Recall with implicits we bind an `Ord<T>` with a rule function, and then use a rule application to give that a value.
We can use a struct to represent our implementation:

```rs
struct Ord<T> {
  cmp: fn(&T, &T) -> std::cmp::Ordering
}
```

Our `Ord` trait has one required method `cmp`, we can then use the implicit to write the provided methods of the `Ord` trait.
We'll define our functions as closures since our COCHIS constructs are expressions.
We could make up syntax for our constructs at the top level, that all works fine, we'll avoid it to minimize the amount of new things we have to introduce.
As an example let's look at the `min` function that's provided by `Ord`:

```rs
let min = |v1: T, v2: T| |?Ord<T>| {
  match (?Ord<T>).cmp(v1, v2) {
    Ordering::Less | Ordering::Equal => v1,
    Ordering::Greater => v2,
  }
};
```

If you take a look at the standard library's implementation of `min`: 

```rs
fn min(self, other: Self) -> Self
where
    Self: Sized,
{
  min_by(self, other, Ord::cmp)
}
```

it calls out to a `min_by` function:

```rs
pub
pub fn min_by<T, F: FnOnce(&T, &T) -> Ordering>(v1: T, v2: T, compare: F) -> T {
  match compare(&v1, &v2) {
    Ordering::Less | Ordering::Equal => v1,
    Ordering::Greater => v2,
  }
}
```

That function looks quite familiar to our example, in fact we can rewrite the standard libraries `Ord` to use implicits in place of passing `Ord::cmp` explicitly:

```rs
fn min(self, other: Self) -> Self
where
    Self: Sized,
{
  // Imagine our implicit `min` example is defined in scope
  let min = ...;
  implicit Ord { cmp: std::cmp::Ord::cmp } in
    min(self, other)
}
```

Hopefully you're starting to see Anything Traits Can Do (Implicits Can Do Better).
Another great feature of trait implementations is that they compose, I can construct a new trait implementation out of other implementations.
We can see this in the `Ord` implementation for tuples (this isn't how the standard library does it, but they define `Ord` for tuples up to length 12):

```rs
impl<A: Ord, B: Ord> Ord for (A, B) {
  fn cmp(self, other: Self) -> Ordering {
    self.0.cmp(&other.0)
      .then(self.1.cmp(&other.1))
  }
}
```

I take this for granted, but this is quite the superpower.
As long as you can tell me how to order `A`s and `B`s, I can tell you how to order any tuple with just one implementation.
Obviously we want the same superpower for our Implicits, and fortunately COCHIS has heeded our call.
Rule functions can depend on queries, so we're able to construct a similar `Ord` instance for pairs (again with bastardized Rust syntax):

```rs
let ordPair = |?Ord<A>||?Ord<B>| {
    Ord {
      // Pretend we cast our closure to a function pointer here.
      cmp: |v1: (A, B), v2: (A, B)| {
        (?Ord<A>).cmp(v1.0, v2.0)
          .then((?Ord<B>).cmp(v1.1, v2.1))
      }
    }
};
```

If we squint we can see the similarities, our implicit asks for two `Ord` implementations and produces a new `Ord` implementation that orders pairs.
Finally, let's look take a look at how we can use implicits to sort a list of pairs.
Imagine `Vec`'s `sorted` method made use of an implicit `Ord` implementation (instead of a trait), then we could sort a `Vec` using:

```
implicit Ord { cmp: usize::cmp } in
implicit Ord { cmp: char::cmp } in
implicit ordPair in {
  let mut v = vec![(3, 'a'), (2, 'c'), (3, 'b')];
  v.sort();
  assert_eq!(v, vec![(2, 'c'), (3, 'a'), (3, 'b')]);
}
```

We create implicit implementations for `usize` and `char` (we cheat a little by reusing the standard libraries `cmp` for those types).
Those implementations are used by `ordPair` to create a new implementation for pairs, which is used to sort our `Vec`.
Putting it all together we can see how we construct a coherent set of implementations for `(usize, char)` but _local_ to this scope.
Someone else can construct their own implementation of orderings for pairs (doing god knows what) and it won't interfere with ours at all.
We've escaped the Orphan Instance problem!

With our newfound understanding of COCHIS's constructs, we can understand the design decisions COHCIS makes to ensure stability and coherence.
Due to implicits being lexcically scoped, coherence is relatively straightforward.
If we install two implicit values for the same type the lexically nearest implicit value is selected:

```rs
implicit Ord { cmp: usize::cmp } in
implicit Ord { cmp: |v1: uszie, v2: usize| usize::cmp(v1, v2).reverse() } in {
  let mut v = vec![4, 1, 5, 2];
  v.sort();
  assert_eq!(v, vec![5, 4, 2, 1]);
}
```

We can see in this example, our vector is sorted in reversed order.
This is because our reversed `Ord` implementation is nearer in scope and shadows our standard `Ord` implementation.
Stability is a litter more nuanced.
Recall for our implicits to be stable they need to behave the same way regardless of type inference or inlining.
[The paper](https://i.cs.hku.hk/~bruno/papers/JFPImplicits.pdf) offers a tricky example to illuminate our predicament:

```rs
fn bad<T>() -> fn(T) -> T {
  implicit (|x| x) in
  implicit (|n: usize| n + 1) in
    (?T -> T)
}
```

What is the right thing to return from our `bad` function?
Naively, I'll say we should return `|x| x` because it works for any type. 
Working with more types has to be better right?
Unfortunately, this causes our `bad` function to become unstable under inlining.

Consider a call (somewhere else in our codebase) `bad()(3)`.
If our helpful compiler decides to inline this call it becomes:

```rs
let inline_bad = 
  implicit (|x| x) in
  implicit (|n: Int| n + 1) in 
    (?usize -> usize);
inline_bad(3)
```

We've shot ourselves in the foot!
By the very rules we devised, it's clear the only just thing to return here is `|n: usize| n + 1`.
Our program is at the whims of the inlining winds.
I'd love to tell you we have a slick answer to this that resolves everything neatly.

Alas, the reality is more mediocre.
The solution employed by COCHIS is to disallow this example.
How they manage to detect and ban this example, however, is quite interesting.
It's also quite technical, so I won't be detailing here.
As always, I implore you to check out [the paper](https://i.cs.hku.hk/~bruno/papers/JFPImplicits.pdf).

## There's no free lunch

I've been evangelizing implicits and local coherence, so I couldn't besmirch you for thinking they're perfect.
Let me be clear, lest I besmirch myself, _I_ think they're perfect.
However, I'm honor bound as a guy with a blog™ to tell you the full story.
Some rough edges remain, even with all the problems COCHIS solves over its predecessors:

* They require more repetition
* They lose some correctness guarantees Traits have built-in
* Code legibility is hindered

### Repetition
A big goal of local implicits is to solve the orphan instance problem, and allow multiple implementations for a trait and type.
That's great, but succeeding at that goal kind of by definition means repeating ourselves more than traits.
When you can only have one implementation of a trait for a type, there's a low ceiling on how many implementations you can have.

We can already start to see this in our `ordPair` example above.
We install all our implicits to sort our pairs and it works great.
But what about the next time we want to sort pairs?
Do we just...install the same implicits over again in the new scope?

Clearly not, and this problem isn't entirely intractable.
Programming languages have a long legacy of abstracting stuff, so you can pass it around and reuse it elsewhere.
However, it's not free, there is design work to be done and tradeoffs to be considered on what such a system actually looks like.

We probably don't want to have to install an implicit at the function level every time we use it.
We might want to install an implicit for an entire module and every function in that module makes use of it.
Maybe it'd be helpful to export an implicit from a module.
Then when we import our standard library it installs the exported set of implementations it contains automatically.

There are solutions that sound viable here. 
There's also a vast chasm between sounding viable and being viable.
OCaml's work on modular implicits has shown that it's not trivial to graduate implicits to work with modules seamlessly.
Work on modular implicits started in 2014, and continues [on today](https://modular-implicits.github.io/report.pdf).

### Correctness
Local implicits have a problem that simply cannot arise with Traits.
Consider a `Set` data structure.
For our purposes we'll use an ordered set, but the same quagmire arises for hash sets.
`Set`s support unioning two `Set`s together to form a new `Set`, in Rust the signature is:

```rs
pub fn union<'a>(&'a self, other: &'a BTreeSet<T, A>) -> Union<'a, T> ⓘ
where
    T: Ord
```

How do we know that `self` orders `T`s the same way as `other`?
The answer is obvious with traits, there can only be one implementation of `Ord` for `T`.
Any `Set` we could ever create will order `T`s the sole way we can order them.

By design, this is not the case for local implicits.
We don't know what `Ord` implementation was used to construct our `Set` of `T`s.
`union` relies on the assumption that both it's input `Set`s are ordered the same way.
Violating that assumption will lead to a silently broken `Set`.

One simple solution is available.
`union` can take its own `Ord` value, implicitly, and reorder every element in the resulting set.
While this is correct, it is very slow.
Again this is not the end of the line for local implicits.
This is a solvable problem.

But the solution requires answering more design questions.
Much like we parameterize our `Set` by its element type `T`, we can parameterize `Set` by a second parameter it's `Ord` value.
If you take a look at [`HashMap`](https://doc.rust-lang.org/std/collections/struct.HashMap.html) it already does something like this.
`HashMap` takes a `K` and `V` parameter for key and value, and then has a third parameter `S` that defaults to `RandomState`.
Our new embellished `Set<T, O>` solves our issue for union:

```rs
impl BTreeSet<T, O> {
  pub fn union<'a>(&'a self, other: &'a BTreeSet<T, O>) -> Union<'a, T, O>;
}
```

Now we can be sure our `Set`s are ordered the same because they both use the same `O`.
But uh...what is `O` exactly?
Our `Ord` implementation is a value, we can't put that in a type (sorry dependent typing).
The type of our implementation is `Ord<T>`, putting that in the type isn't super helpful.
Every `Ord` implementation has that type, we'd be right back where we started.

There is a line of work around implicits based on named implementations.
You can name an implementation and then export it and import it like any declaration.
If we had a name for our implementation we could put that in our `Set`.
However, that's another thing we have to rope into our design to solve the issue.
Same as the repetition point, I believe these are solvable problems, but the solutions certainly aren't in hand.

### Legibility

Imagine you're reading some code that uses a trait on a type, and you want to know how it's implemented.
How do you find the trait implementation?
You look in the crate that defines the trait then look in the crate that defines the type.
It's either in one of those locations, or you have a compiler error.

The story is a little more circuitous for implicits.
The freedom to put an implementation anywhere is a double-edged sword.
We're now burdened with looking _anywhere_ to find where our implementation lives.
Extending our implicits to support being able to import them from another module compounds this problem further.
At least with COCHIS as described in the paper, our implicits have to be installed somewhere in lexical scope.

You can make arguments that this problem is solved by a sophisticated IDE.
Certainly there's some truth to that, but it echoes the sufficiently smart compiler arguments.
Even if such an IDE exists, there are still plenty of contexts we read code where goto definition isn't available.
TODO: transition

Here's where I reveal my ruse.
I've come to you today to disparage traits as a local maxima, and proselytize implicits as the peak we should be climbing.
When in fact, it's quite possible implicits are just another local maxima as well.
As is oft the case in engineering, it's all about tradeoffs.

I think traits are great. 
I used traits today.
I'll use traits tomorrow.
That being said, they have some issues.
Trait alternatives are relatively unexplored by comparison.
I think the local coherence of COCHIS is a really promising approach that more people should know about.


* What's a trait
  * Should they be tied to typeclasses?
* What's the problem with traits
  * Orphan Instances
  * Anti-modular
* Coherence
  * Traits rely on Global Coherence
  * Explain global coherence
  * Local coherence instead of global coherence
    * If global coherence is bad, clearly the answer is local coherence.
    * Now we just gotta figure out what local coherence is.
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
  * Talk about what's required to make resolution deterministic.
  * Bad example.
    * Stability under inlining.
  * Reconstruct your trait instance each time you want to use it.
  * ... I need something here desparetly
  
