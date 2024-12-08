+++
title = "Traits are a Local Maxima"
date = "2024-11-18T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Traits"]
keywords = ["Programming Languages", "Compiler", "Type Classes", "Traits", "Parametric Polymorphism", "Local Coherence", "Global Coherence"]
description = "An expose on Implicits and Local Coherence"
+++

Today we're talking about [Rust traits](https://doc.rust-lang.org/book/ch10-02-traits.html).
As you might've guessed from the title, we will not be talking about them positively.
Woah woah, hang on, put down your pitchfork.
A local maximum is still a maximum, I love traits as much as anyone.

Traits are one of the few programming language concepts beloved enough to earn multiple names.
You might have heard about typeclasses in Haskell or protocols in Swift.
Heck, if the teacher's not watching, even interfaces can be considered a kind of trait.
Elm famously left out traits, and it was so requested it spawned an [FAQ](https://faq.elm-community.org/#does-elm-have-ad-hoc-polymorphism-or-typeclasses) explaining their absence.

Traits have a rich history dating back to 80s.
How could such a mature and cherished feature have problems?
Especially, problems substantial enough to warrant a blog post?

I'm so glad you asked.

## The problem with Traits

We can illustrate our woes by conjuring some contrived Rust code:

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

We have two dependencies `a_crate` and `b_crate`. (We'll worry about naming another day.)
One of them provides us a type `A`, and the other a trait `BTrait`.
Our crate's job is to frobinate things, and we'd like to frobinate `A`s.
Clearly the code above frobinates an `A`.
At least until we try to compile it:

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
Solving that issue is a simple matter of implementing `BTrait` for `A`:

```rs
impl BTrait for A {
  fn random_number(&self) -> usize {
    4 // chosen by fair dice roll. 
  }
}
```
Easy, and guaranteed to be random.
Unfortunately, when we attempt to get back to frobinating, we discover another issue:

```
error[E0117]: only traits defined in the current crate can 
  be implemented for types defined outside of the crate
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

This error message even tells us how to fix itself.
I'd pay good money to be able to tell you how to fix myself.
Our helpful error message is telling us our implementation isn't valid.
Not due to anything implicit to the `impl` mind you but simply because of where it resides.

This is the fatal flaw of traits.
A trait implementation must either be in the crate that defines the type or the crate that defines the trait.
This error isn't the end of the world.
It even tells us it'll go away if we make a new type.
We can create a new and improved type `A2` in our current crate to implement our trait:

```rs
struct A2(A);
impl BTrait for A2 {
  fn random_number(&self) -> usize {
    4 // chosen by fair dice roll, still!
  }
}
```

I'm sure it won't take much to convince you; this is unsatisfying.
It's straightforward in our contrived example.
In real world code, it is not always so straightforward to wrap a type.
Even if it is, are we supposed to wrap every type for every trait implementation we might need?
People love traits.
That would be a stampede of new types.

Wrapper types aren't free either.
`a_crate` has no idea `A2` exists. 
We'll have to unwrap our `A2` back into an `A` anytime we want to pass it to code in `a_crate`.
Now we have to maintain all this boilerplate just to add our innocent implementation.

Haskell christens this the [orphan instance](https://wiki.haskell.org/Orphan_instance) problem.
It's a well known downside to traits, dating back to their invention.
Rust is also well aware of the issue.
Niko (of designing Rust fame) has written up a [post](https://smallcultfollowing.com/babysteps/blog/2022/04/17/coherence-and-crate-level-where-clauses/) about why orphan instances pose such a problem.

Now we know what the problem is.
But let's put our empathy caps on and ask why the problem is.
Reimagine our `a_crate`/`b_crate` example except now with orphan instances allowed.
Our `frobinate` library can implement `BTrait` for `A`, and we stop worrying about it.
Understandably, our `frobinate` library is a huge hit with millions of users.
One of those satisfied users is shipping an app that's `frobinating` till the venture capital runs dry.
Til one day they need to `swizzle` things, so they pull in a dependency on the popular `swizzle` library.

Catastrophe ensues!
`swizzle` ran into the same problem as `frobinate` and solved it the same way by implementing `BTrait` for `A`.
Now our poor downstream user has two instances of `BTrait` for `A`.
What is the user supposed to do here?

They don't own these libraries; they can't remove either of the implementations.
It's not safe for the compiler to just pick one of the implementations.
`frobinate` won't necessarily work as expected with `swizzle`'s implementation of `BTrait` for `A` (or vice versa).
We're stumped here.
Our only hope is to prevent this situation from arising in the first place.

## Global Coherence

This leads us to a key requirement of traits: there can only ever be one instance of a Trait for a Type.
Only allowing a single implementation per type is why Rust forces trait implementations to live in specific crates.
It ensures that libraries can't introduce overlapping implementations incidentally.
If we only have one implementation, our compiler always knows what implementation to pick.

This property that our compiler can't pick the wrong implementation has a name.
It's _coherence_, [sorry for the technical term](https://youtu.be/T7jH-5YQLcE?si=6xGv62wLYRFTWC-D).
Skimming over _a lot_ of details, a program is coherent if it can pick any trait implementation and behave the same way.

Compilers revel in coherent programs.
Having the freedom to pick any trait implementation allows type checkers and optimizers to frolic across your code with reckless abandon.
Users of the language also benefit from coherence.
If one day our typechecker decided to pick a different implementation and suddenly our, otherwise unchanged, code executed differently we'd be bewildered.

Enforcing one implementation per trait/type combo makes it very easy to ensure coherence.
If our compiler only ever has one implementation to pick from, it's free to pick any implementation and behavior never changes.
Kind of feels like cheating.
It feels like there's a higher peak out there in the landscape of language design.

Coherence is great for compilers but maintaining one implementation per trait/type combo presents its own complications.
How do you check you don't have overlapping implementations?
Open up every module and check if any of them implement the same trait for the same type.
"Open up every module" sends chills down the spine of any compiler interested in finishing this century.

While this is the technically correct thing to do; this is intractable in practice.
In fact, this presents such a performance problem, GHC simply [doesn't](https://gitlab.haskell.org/ghc/ghc/-/issues/2356).
It's better to allow overlapping implementations to sneak into your codebase rather than force the compiler to touch every module to keep them out.

Notice a running theme in our issue -- they all stem from traits requiring a kind of global consensus.
We can imagine we have a global scope of traits, and we only ever want one implementation per type in that scope.
I'm going to call enforcing coherence in this way: global coherence.

[This blogpost](http://blog.ezyang.com/2014/07/type-classes-confluence-coherence-global-uniqueness/) explains this is actually a perversion of terminology.
Please excuse my transgression and stick with me.
Our issues with traits all orbit around requiring global coherence.
Ironically, global coherence is what prevents traits from being a global maximum.

It's obvious from here, if global coherence is a local maximum, local coherence is a global maximum.
Now all we have to do is figure out if, and what, local coherence is.

## Local Coherence

Our quest is set; we seek the mythical local coherence.
Defining it by contrast doesn't tell us a lot about what we're looking for though.
Global coherence requires that we have a unique implementation in the global scope of our program.
Conversely, it makes sense that local coherence would loosen that restriction to a unique implementation in a local scope...for some meaning of local and scope.
We're allowed to introduce overlapping implementations as long as they are in different scopes.

Revisiting our `frobinate` and `swizzle` example, local coherence solves our problem.
`frobinate` and `swizzle` each have their own implementation of `BTrait`.
As long as they don't import each other, everything works. 
Great!

Outside that we don't have to change too much from traits.
We still want to create implementations.
Functions still want to ask for trait implementations on their generics.
Except now, they'll look in their local scope instead of a global scope.
I can't be the first one to have thought of this right?

* [Modular Implicits](https://arxiv.org/pdf/1512.01895)

Oh, neat. See I knew I couldn't be-

* [Scala Implicits](https://docs.scala-lang.org/tour/implicit-parameters.html)
* [Agda Implicits](https://agda.readthedocs.io/en/latest/language/instance-arguments.html)
* [Lean Implicits](https://lean-lang.org/lean4/doc/typeclass.html)
* [COCHIS: Stable and Coherent Implicits](https://i.cs.hku.hk/~bruno/papers/JFPImplicits.pdf)

Okay. Okay. I get it, excuse my ignorance. 
Clearly this is not a new idea. 
Everyone has already settled on calling it implicits.
Despite the many examples of implicits out there, the ideas haven't made their way into the mainstream.
Modular implicits in OCaml and Implicits in Scala are the most popular on this list (you can pick which one is the most popular), and both are niche compared to the Rust userbase.
Their implicit implementations also leave something to be desired.

On the OCaml side, modular implicits haven't actually been implemented in the language.
Traditionally, a sizeable downside.
[The paper](https://arxiv.org/pdf/1512.01895) lists some of the open design considerations that remain to be solved.
To summarize, once you allow local implementations, type checking becomes order dependent between typechecking and resolving implicits.
This is a problem for staying coherent, which is a must.

On Scala's end implicits are unstable.
Martin Odersky (core contributor and one of the original developers of Scala) gave a great keynote on some of the issues with Scala implicits: [What to leave implicit](https://www.youtube.com/watch?v=br6035SKu-0).
To skip a lot of detail, unstable implicits mean that our program might behave differently depending on how we infer types or inline terms.
This can cause headaches for end users (and compilers), as generally we expect our program to behave the same way regardless of things like inlining.

## Stable and coherent Implicits

The final paper on that list [COCHIS](https://i.cs.hku.hk/~bruno/papers/JFPImplicits.pdf) seeks to solve these issues with their _Calculus of Coherent Implicits_.
COCHIS lays out a minimal language with the bare essentials to achieve implicits.
This is because it is not a full language like Scala or OCaml. 
It is a new spin on implicits that only exists in the whitepaper (for now).
COCHIS's language is a small functional language with a lot of the constructs we know and love: variables, functions, applications, etc.

Standard stuff aside, COCHIS employs 3 new constructs to implement their implicits: 

* Query: `?Int`
* Rule function: `|?Int| <body>`
* Rule application: `<implicit> with <argument>`

This might look similar to the 3 cases of our good friend: the lambda calculus.

* A query looks up the nearest lexically bound implicit value by type, similar to a lambda variable.
* A rule function binds an implicit value for a type in the scope of `<body>`, similar to a lambda function binding a variable.
* A rule application provides a value of the right type for a rule function, similar to a lambda application.

Also like the lambda calculus, we can introduce a `let` binding as syntax sugar:

```rs
implicit <argument> in <body> ⇒ (\?T. <body>) with <argument>
```

where `T` is the type of our argument.
We'll adopt this syntax sugar to make our examples shorter.
COCHIS employs their own syntax to explain their constructs.
I'm instead going to bolt these 3 implicit constructs onto Rust's syntax to help us see the parallels with traits.
It helps me by avoiding introducing new syntax as well.

Speaking of parallels, let's take a look at how we can use COCHIS's constructs to replicate trait behavior.
We're going to translate the `Ord` trait to COCHIS implicits (this is an example from [the paper](https://i.cs.hku.hk/~bruno/papers/JFPImplicits.pdf) we're retooling, find more details there).
The first step on our journey is to create a value to represent our implementation of the `Ord` trait.
This is already a departure from traits, where we ask for a `T: Ord`, and then the functions of `Ord` are automagically available on values of type `T`.
With implicits we bind an `Ord<T>` with a rule function and then use a rule application to give that a value.

We can use a struct to represent our implementation:

```rs
struct Ord<T> {
  cmp: fn(&T, &T) -> std::cmp::Ordering
}
```

Our `Ord` trait has one required method `cmp` that translates into an implicit value with one field for that method.
We can use the implicit to write the provided methods of the `Ord` trait (`min`, `max`, etc.).
As an example, let's look at how we can construct the `min` function of the `Ord` trait using implicits.
In the interest of avoiding new syntax, we'll define our `min` as a closure not a top level function.
It would be just as straightforward to define `min` as a top level function if we wanted to invent syntax for Rust functions to take implicits:

```rs
let min = |?Ord<T>| |v1: T, v2: T| {
  match (?Ord<T>).cmp(v1, v2) {
    Ordering::Less | Ordering::Equal => v1,
    Ordering::Greater => v2,
  }
};
```

Here we can see a usage of a rule function and a rule application.
`min` uses a rule function to bind an implicit of type `Ord<T>`.
That implicit is used in the match where we query for it using `(?Ord<T>)`.
Recall `Ord<T>` is a struct with a single member `cmp` that we use to compare our `v1` and `v2`.
If you take a look at the standard library's implementation of [`min`](https://doc.rust-lang.org/std/cmp/fn.min.html): 

```rs
fn min(self, other: Self) -> Self
where
    Self: Sized,
{
  min_by(self, other, Ord::cmp)
}
```

It calls out to a `min_by` function:

```rs
pub fn min_by<T, F: FnOnce(&T, &T) -> Ordering>(
  v1: T, v2: T, 
  compare: F
) -> T {
  match compare(&v1, &v2) {
    Ordering::Less | Ordering::Equal => v1,
    Ordering::Greater => v2,
  }
}
```

That function looks quite similar to our example.
`min_by` takes an explicit parameter `compare` instead of an implicit, but otherwise they are the same.
In fact, we can rewrite the standard library's `min` to use implicits in place of passing `Ord::cmp` explicitly:

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
Another great feature of trait implementations is that they compose. 
I can construct a new trait implementation out of other implementations.
We can see this in the `Ord` implementation for tuples. 
This isn't actually how the standard library does it, but they define `Ord` for tuples up to length 12:

```rs
impl<A: Ord, B: Ord> Ord for (A, B) {
  fn cmp(self, other: Self) -> Ordering {
    self.0.cmp(&other.0)
      .then(self.1.cmp(&other.1))
  }
}
```

As long as you can tell it how to order `A`s and `B`s, traits can tell you how to order any tuple with just one implementation.
Obviously we want the same superpower for our implicits, and fortunately COCHIS has heeded our call.
Rule functions can depend on rule functions, so we're able to construct a similar `Ord` instance for pairs:

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

If we squint, we can see the resemblance our `ordPair` asks for two `Ord` implicits and produces a new `Ord` implicit that orders pairs.
Putting everything together, let's use our implicits to sort a vector of pairs.
Imagine `Vec`'s `sorted` method made use of an implicit `Ord` implementation (instead of a trait) then we could sort a `Vec` using:

```rs
implicit Ord { cmp: usize::cmp } in
implicit Ord { cmp: char::cmp } in
implicit ordPair in {
  let mut v = vec![(3, 'a'), (2, 'c'), (3, 'b')];
  v.sort();
  assert_eq!(v, vec![(2, 'c'), (3, 'a'), (3, 'b')]);
}
```

We create implicit implementations for `usize` and `char`. 
(We cheat a little by reusing the standard libraries `cmp` function for those types.)
`ordPair` uses those implementations to create a new implementation for pairs, which is used to sort our `Vec`.
Our example constructs a coherent set of implementations for `(usize, char)`, but _local_ to this scope.
Someone else can construct their own implementation of orderings for pairs (doing god knows what), and it won't interfere with ours at all.
We've escaped the Orphan Instance problem!

With our newfound understanding of COCHIS's constructs, we can understand the design decisions it makes to ensure stability and coherence.
Certifying coherence is relatively straightforward. 
Implicits are lexically scoped making it apparent which one to pick.
If we install two implicit values for the same type, the lexically nearest implicit value is selected:

```rs
implicit Ord { cmp: usize::cmp } in
implicit Ord {
  cmp: |v1, v2| usize::cmp(v1, v2).reverse() 
} in {
  let mut v = vec![4, 1, 5, 2];
  v.sort();
  assert_eq!(v, vec![5, 4, 2, 1]);
}
```

We can see in this example, our vector is sorted in reverse order.
This is because our reversed `Ord` implementation is nearer in scope and shadows our standard `Ord` implementation.
Stability is a little more nuanced.
Recall for our implicits to be stable they need to behave the same way regardless of type inference or inlining.
[The paper](https://i.cs.hku.hk/~bruno/papers/JFPImplicits.pdf) offers a tricky example to illuminate our predicament:

```rs
fn bad<T>() -> fn(T) -> T {
  implicit (|x: T| x) in
  implicit (|n: usize| n + 1) in
    (?T -> T)
}
```

We have two implicits our query `(?T -> T)` could select from: one of type `T -> T` and one of type `usize -> usize`.
What is the right thing to return from our `bad` function?
Naively, I'll say we should return `|x| x` because it works for any type. 
Working with more types has to be better right?
Unfortunately, this causes our `bad` function to become unstable under inlining.

Consider a call (somewhere else in our codebase) `bad()(3)`.
If our "helpful" compiler decides to inline this call, it becomes:

```rs
let inline_bad = 
  implicit (|x| x) in
  implicit (|n: usize| n + 1) in 
    (?usize -> usize);
inline_bad(3)
```

By the very rules we devised, it's clear the only just thing to return here is `|n: usize| n + 1`.
Our program is at the whims of the inlining winds.
I'd love to tell you we have a slick answer to this with a happy ending.

Alas, the reality is more mediocre.
The solution employed by COCHIS is to disallow this example entirely.
How they manage to detect and ban this example, however, is quite interesting.
It's also quite technical, so I won't be detailing it here.
As always, I implore you to check out [the paper](https://i.cs.hku.hk/~bruno/papers/JFPImplicits.pdf).

## There's no free lunch

I've been evangelizing implicits and local coherence, so I couldn't besmirch you for thinking they're perfect.
Let me be clear, lest I besmirch myself, _I_ think they're perfect.
However, I'm honor bound as a guy with a blog to tell you the full story.
Some rough edges remain, even with all the problems COCHIS solves over its predecessors:

* They require more repetition.
* They lose some correctness guarantees Traits have built-in.
* Code legibility is hindered.

### Repetition
A big goal of local implicits is to solve the orphan instance problem and allow multiple implementations for a trait and type.
That's great, but succeeding at that goal kind of by definition means repeating ourselves more than traits.
When you only have one unique implementations of a trait for a type, you can never repeat yourself.

We can already start to see this in our `ordPair` example above.
We install all our implicits to sort our pair, and it works great _in that scope_.
But what about the next scope?
Do we just...install the same implicits over again?

I sure hope not.
I got into this trait and implicit business to avoid repeating myself.
Programming languages have a long legacy of abstracting stuff, so you can pass it around and reuse it elsewhere.
We can use that here to allow abstracting "sets" of implicits that can be passed around and reused.
However, it's not free. 
There is design work to be done and tradeoffs to be considered on what such a system actually looks like.

We probably don't want to have to install an implicit at the function level every time we use it.
Heck, we might want to install an implicit for an entire module and every function in that module makes use of it.
Maybe it'd even be helpful to export an implicit from a module.

With all these features, we could have a standard library that exports a "set" of implicit implementations that are installed by default when you import the standard library.
That gets us pretty close to the experience we have today with traits, but they're still local implicits.
We're free to install our own implementations when we want to override the standard library's implementations.

There are solutions that sound viable here.
There's also a vast chasm between sounding viable and being viable.
OCaml's work on modular implicits has shown that it's not trivial to graduate implicits to work with modules seamlessly.
Work on modular implicits started in 2014 and continues [on today](https://modular-implicits.github.io/report.pdf).

### Correctness
Local implicits have a problem that simply cannot arise with Traits.
Consider a `Set` data structure.
For our purposes we'll use an ordered set, but the same quagmire arises for hash sets.
`Set`s support unioning two `Set`s together to form a new `Set`. 
Rust's `BTreeSet.union` signature is:

```rs
pub fn union<'a>(&'a self, other: &'a BTreeSet<T, A>) -> Union<'a, T> ⓘ
where
    T: Ord
```

How do we know that `self` orders `T`s the same way as `other`?
This question simply doesn't arise with traits.
There can only be one implementation of `Ord` for `T`; that's how they are ordered.
Any `Set` we could ever create will order `T`s the sole way they're ordered.

By design, this is not the case for local implicits.
We don't know what `Ord` implementation was used to construct our `Set` of `T`s.
`union` relies on the assumption that both of its input `Set`s are ordered the same way.
Violating that assumption will lead to a silently broken `Set`.

One simple solution is available.
`union` can take its own `Ord` value, implicitly, and reorder every element in the resulting set.
While this is correct, it is very slow.
Like repetition, this is not the end of the line for local implicits.
This is a solvable problem.

But the solution requires answering more design questions.
Much like we parameterize our `Set` by its element type `T`, we can parameterize `Set` by a second parameter it's `Ord` value.
If you take a look at [`HashMap`](https://doc.rust-lang.org/std/collections/struct.HashMap.html), it already does something like this.
`HashMap` takes a `K` and `V` parameter for key and value and then has a third parameter `S` that defaults to `RandomState`.
Our new embellished `Set<T, O>` solves our issue for union:

```rs
impl BTreeSet<T, O> {
  pub fn union<'a>(&'a self, other: &'a BTreeSet<T, O>) -> Union<'a, T, O>;
}
```

Now we can be sure our `Set`s are ordered the same because they both use the same `O`.
But uh...what is `O` exactly?
Our `Ord` implementation is a value. 
We can't put that in a type (sorry dependent typing).
The type of our implementation is `Ord<T>`, putting that in the type isn't super helpful.
Every `Ord` implementation has that type. 
We'd be right back where we started.

There is a line of work around implicits based on named implementations.
You can name an implementation and then export it and import it like any declaration.
If we had a name for our implementation, we could put that as our `O`.
However, now that's another thing we have to rope into our design to solve the issue.

### Legibility

Imagine you're reading some code that uses a trait on a type, and you want to know how it's implemented.
How do you find the trait implementation?
You look in the crate that defines the trait then look in the crate that defines the type.
It's either in one of those locations, or you have a compiler error.

The story is a little more circuitous for implicits.
The freedom to put an implementation anywhere is a double-edged sword.
We're now burdened with looking _anywhere_ to find where our implementation lives.
Extending our implicits to support being imported from another module compounds this problem further.
At least with COCHIS, as described in the paper, our implicits have to be installed somewhere in lexical scope.

You can make arguments that this problem is solved by a sophisticated enough IDE.
Certainly there's some truth to that, but it echoes the sufficiently smart compiler arguments.
Even if such an IDE exists, there are still plenty of contexts we read code where goto definition isn't available.
You're sick of hearing it by now; these are solvable problems.
But a solvable problem isn't a solved problem.

Here's where I reveal my ruse.
I've come to you today disparaging traits as a local maximum and proselytizing implicits as the peak we should be climbing.
When in fact, it's quite possible implicits are just another local maximum as well.
As is often the case in engineering, it's all about tradeoffs.
