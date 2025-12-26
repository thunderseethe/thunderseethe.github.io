+++
title = "Name Resolution, Resolutely"
date = "2025-11-12T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Name Resolution"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Name Resolution", "Scope", "Lexical Scoping", "Abstract Syntax Tree", "Error Recovery"]
description = "Resolving scoping and shadowing to produce unique names for our variables"
+++

{{< accessory title="Making a Language" >}}
This post is part of the [making a language series](/series/making-a-language).
A series that teaches you how to implement a programming language using Rust.

This post covers name resolution.
Desugaring left us with an `Ast<String>` that we can't yet feed to type inference.
Name resolution will figure out what the names in our program mean and turn that into a `Ast<Var>` that is ready for type checking.
{{</ accessory >}}

Why do we name things?
A silly question, perhaps.
No one had to tell me to name my pet rock Francis.
One look at his sedimentary exterior was all it took to christen him.

Names help us identify with things.
This is just as true in computers as it is in rock companions.
In programming languages, names help us identify and share values.
Rather than repeat `1 + 2` every time we need it, we can name it `three` and reference it by name.

Naming helps make our program more legible to our fellow programmers.
Names bundle up complexity and offload it, allowing us to comprehend bigger and better programs.
They also provide a means of sharing.

When we name a value, in most languages, we give it a place in memory.
`three` doesn't just let us skip saying `1 + 2` every time, it also let's the computer skip recomputing `1 + 2` every time.
Naming places in memory is very handy for keeping track of them as a human.
It turns out it's less handy to the computer.

## Computer Friendly Names

The computer is solely interested in knowing where a value lives in memory, and by extension when two values are actually the same because they live in the same place.
Human readable names aren't great for this purpose.
Our language includes scoping and shadowing.
Two features that help humans manage names but hinder the computer.

A scope is a map from names to their values at a particular point in our program.
If we write the program:

```rs
let one = 1;
let two = 2;
let three = add one two;
add three two
```
Then our scope at `let two = 2;` would contain `one`.
But our scope at `add three two` would contain: `one`, `two`, and `three`.

Scopes can be stacked on top of each other.
If we introduce a function, its body is in a new scope:

```rs
let y = 2;
let f = |x| add x y;
f 1
```

`|x| add x y` introduces a new scope that stacks on top of the existing scope containing `y`.
When we finish defining `f`, our `|x|` scope is popped off the stack and we can no longer access those names.

We can see that `y` is accessible from within `|x| add x y`.
If we can't find a name within a scope, we go up the stack and see if we can find it there.
This behavior gives rise to our second feature: shadowing.

Introducing a new scope lets us redefine a previously defined name.
Imagine our previous code example was:

```rs
let y = 2;
let f = |y| add y y;
f 1
```

Our function's scope introduces the name `y`.
We can no longer refer to `y = 2` within the function body.
We've shadowed that name with our function parameter `|y|`.

Shadowing is convenient.
Reusing names reduces the number of names we have to make up just to name temporary intermediate values.
Each let expression introduces their own scope, which makes sense given we know they're actually just functions under the hood.
It's perfectly valid to write:

```rs
let x = 1;
let x = add x x;
let x = add x x;
x
```

It's nice to not have to make up names like `x'` or `x0` for values that are immediately consumed.
As a reader, it's helpful to see that `x = 1` gets shadowed.
I know for the remainder of that expression `x = 1` won't be referenced again and I can put it out of mind.

Shadowing is also controversial.
It can make it harder to tell what a name references and it can be easy to miss that a name was shadowed.
Plenty of production languages eschew shadowing and are fine.
But that's a design discussion for a different day.
Our language will have shadowing, and I like it that way.

## Resolving the Names

Back to names.
Scoping complicates determining what a name references.
When we see a name, we have to walk our stack of scopes searching for where it's defined.
Making this trek every time we see a name does not sound very efficient.

Name resolution resolves our woes.
It does the work of figuring out what each name references and then saves it in a form that is more amenable to the computer's needs.
Because we've already written type inference, we actually know what this form is.
Each name will be replaced by a unique variable that is essentially just an integer counter:

```rs
struct Var(usize);
```

After name resolution our shadowing example becomes:

```rs
let v0 = 1;
let v1 = add v0 v0;
let v2 = add v1 v1;
v2
```

We assign each name a unique variable and those variables represent unique locations in memory, unlike `x`.
We no longer have to worry about stacks of scopes.
We can shove every name in our program in a big `HashMap<Var, T>` without worry that shadowing will cause two names to overlap.
And in fact, we'll do just that in name resolution.

There's one more task we accomplish during name resolution, ensuring all our names are defined.
It's perfectly legal to write:

```rs
let f = |y| add x y;
g 1
```

But it's utterly nonsensical to the compiler.
`g` has no definition and may not even be a function.
`x` is referenced with nary a let binding in sight.
As we resolve our names, such errors will make themselves abundantly clear and we'll keep track of them.
They won't stop our progress, we're resilient, but we will replace undefined names with a `Hole`.

## Setup our Pass

Let's see some code.
We start the same place we do for almost every pass: a shared struct.

```rs
struct NameResolution {
  supply: VarSupply,
  names: HashMap<Var, String>,
  errors: HashMap<NodeId, NameResolutionError>,
}
```

`supply` supplies all of our variables:

```rs
#[derive(Default)]
struct VarSupply {
  next: usize,
}
impl VarSupply {
  fn supply(&mut self) -> Var {
    let id = self.next;
    self.next += 1;
    Var(id)
  }
}
```

It's essentially a counter, but we wrap our count up as a `Var`.

`names` maps our unique variables back onto the names they came from.
We'll use this in diagnostics.
We'd like to avoid showing the user `var12`, if we can help it.

`errors` tracks errors that arise during name resolution.
There's actually only one error that can arise:

```rs
enum NameResolutionError {
  UndefinedVar(NodeId, String),
}
```

When we encounter an undefined variable, we'll emit this error.
We can do this at most one time per AST node, so we keep them all in a map.
Although between you and me, I think this will only happen for `Ast::Var` nodes.

## Resolve Method

`NameResolution` has one method `resolve` that does all our resolving.

```rs
impl NameResolution {
  fn resolve(
    &mut self, 
    ast: Ast<String>, 
    env: im::HashMap<String, Var>
  ) -> Ast<Var> {
    match ast {
      // Put some cases here...
    }
  }
}
```

`resolve` takes an `env` that maps our names to their unique `Var`s.
`env` tracks our scopes.
Because it's a persistent map, we don't need a stack of scopes.
Adding a value to our map produces a new map, leaving the old map intact.
When we recursively call `resolve`, we pass it our updated map, reusing the call stack as our stack of scopes.

From there `resolve` matches on our `ast`.
We'll handle it case by case, starting with functions:

```rs
Ast::Fun(id, name, body) => {
  let var = self.supply.supply();
  self.names.insert(var, name.clone());
  let body = self.resolve(*body, env.update(name, var));
  Ast::fun(id, var, body)
}
```

Because we desugar let expressions into applied functions, this is our only case that can introduce names.
We create a `Var` for our function parameter and map it back to its name.
Upon updating `env` to map our name to our `Var`, we resolve our names within `body`.
We then reconstruct our name-resolved function.

Our `Var` case immediately uses the vars introduced by `Fun`:

```rs
Ast::Var(id, name) => match env.get(&name).copied() {
  Some(v) => Ast::Var(id, v),
  None => {
    self
      .errors
      .insert(id, NameResolutionError::UndefinedVar(id, name));
    let var = self.supply.supply();
    Ast::Hole(id, var)
  }
}
```

When we find a `Var` for our name, we successfully resolve it and move on.
Upon failing to find a `Var`, we have an error.
We don't stop at our error, however.
We record the error and treat our variable as a hole.

We also, shockingly, emit a hole for our `Hole` case:

```rs
Ast::Hole(id, name) => {
  let var = self.supply.supply();
  self.names.insert(var, name.clone());
  Ast::Hole(id, var)
}
```
If we start with a hole, we have a name, so we map that back to the `Var` we assign our hole.
This doesn't matter for our program execution.
We'll never compile a hole to executable code, but it's helpful for error reporting and diagnostics.

`App` doesn't have any names, but we have to resolve any names in its children:

```rs
Ast::App(id, fun, arg) => {
  let fun = self.resolve(*fun, env.clone());
  let arg = self.resolve(*arg, env);
  Ast::app(id, fun, arg)
}
```

Int doesn't have names and doesn't have children, so there's nothing to do:

```rs
Ast::Int(id, i) => Ast::Int(id, i)
```

Never change `Int`.
You are my rock.

`resolve` is our main and only method on `NameResolution`.
We're done.
Why did we setup a whole struct to share our state between one method?
I didn't want to write `resolve(supply, names, errors, ast, env)` at every call site, sue me.
Also, consistency is key.
We've established the pattern and it's helpful to employ it, even for one method.

Everything gets put together in our top level `name_resolution` method:

```rs
fn name_resolution(
  ast: Ast<String>
) -> NameResolutionOut {
  let mut nameres = NameResolution::default();
  let ast = nameres.resolve(ast, im::HashMap::default());
  NameResolutionOut {
    ast,
    errors: nameres.errors,
    names: nameres.names,
  }
}
```

Recall `names` tracks our mapping from `Var` back to the human names.
We'll need that in our overarching compiler, so we include it in the output for our pass.

## Example

Let's see name resolution in action.
We'll start with a program that reuses a lot of names:

```rs
let x = |x| x;
let y = |y| x y;
let x = 3;
y x
```

That's enough X's and Y's to send me hurtling back to algebra class.
Despite our only two names being `x` and `y`, scoping and shadowing mean those names reference disparate values throughout our expression.

Our first binding `let x = |x| x;` already exemplifies both features.
`let x` introduces a new scope where `x` is defined as `|x| x`.
Functions introduce their own scope, however, so our `let x` is immediately shadowed by our function parameter `x`.
The body of our function the returns our function parameter, not our let binding.

In our following `let y = |y| x y;` binding we've left the `|x| x` scope.
Now `x` does refer to our let bindings, which is good because we're using it as a function.
We see a similar shadowing of `y` for our parameter.
Our function parameter `y` shadows the let bound `y`.

After that we shadow `x` once again with the let binding `let x = 3;`.
Since this shadowing is in the same scope as `let x = |x| x;`, that binding is no longer available.
Note that the loss of a name does not prevent us from accessing `let x = |x| x;`.
Anytime we evoke `y`, it's `x` still refers to `let x = |x| x;` and not `let x = 3;`.
Because our scopes are lexical, names refer to their nearest preceding definition even if we execute them after those names are shadowed.

Our final `y x` uses the latest bindings for each name and will be the same as `(|y| (|x| x) y) 3` at runtime.
After name resolution, we'll have figured all this out and left behind unique `Var`s for each name:

```rs
let v0 = |v1| v1;
let v2 = |v3| v0 v3;
let v4 = 3;
v2 v4
```

Name resolution was our final keystone.
We've completed the arch!
It's been quite the journey but we now have every pass of our compiler.
As always, you can find the [full code in the repo](https://github.com/thunderseethe/making-a-language/tree/main/name_resolution/base).
We can feed the `Ast<Var>` produced by name resolution into type inference and create a complete pipeline.
In fact, that's precisely what we'll do next.
