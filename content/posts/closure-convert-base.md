+++
title = "Closure Conversion Takes The Function Out Of Functional Programming"
date = "2025-05-14T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "ClosureConvert"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Functions", "IR", "Compiler", "Runtime", "Closure", "Closure Conversion", "Lambda Lifting"]
description = "Converting our lambda functions into closures for the Base IR."
+++

{{< accessory title="Making a Language" >}}
This post is part of the [making a language series](/series/making-a-language).
A series that teaches you how to implement a programming language using Rust.

Today's post is preceded by the [base monomorphization pass](/posts/monomorph-base).
Like monomorphization, closure conversion does not rely on anything from its prior pass.
It relies on the `IR` (and accompanying types) introduced in [lowering](/posts/lowering-base-ir).
We'll review `IR` before the action starts.
{{</ accessory >}}

Our [previous pass](/posts/monomorph-base), monomorphization, stripped polymorphism away from our intermediate representation (IR).
Today we've come to cut down an even closer confidant, functions.
How many friends must we lose on our conquest of compilation.
This may come as a shock, functions are entwined deeply in our language.
I mean it's right there in the name, functional programming.
I don't know what al programming is, and I don't want to.

Without functions, only integers remain.
It's all ones and zeroes at the end of the day, but I'm not sure how to compute with just integers.
I'd at least need a possibly infinite tape.
Allow me to assuage your concerns.
We're removing functions, but we'll be replacing them with something new.
Before looking at what replaces functions, let's talk about why we're replacing functions.

Unlike polymorphism, you will find instructions on a machine for functions.
On the contrary, they're hardly exotic. 
You'll be hard-pressed to find an architecture that lacks instructions for returning and passing function arguments, begging the question "why are we removing functions?".
The dissonance arises from a distinction in definition of function.
The functions used by our language are not the same functions implemented by the hardware.

Our language's functions are actually lambdas.
Lambdas are functions that capture their surrounding lexical scope as an environment.
Hardware functions lack this capability.
Some rust code helps us highlight the difference:

```rs
// A hardware function
fn foo(
  x: usize
) -> usize {
  x + 1
}

let a = 1;
let y = 10;
// A lambda
let bar = |x: usize| x * y + a;
```

`foo` a hardware function requires no environment.
We pass it the requisite argument and receive our answer.
`bar`, on the other hand, captures the variables `a` and `y` in an environment that is made available every time we call `bar`.

The convenience of variable capture cannot be overstated.
A myriad of programming patterns can be boiled down to lambdas capturing other lambdas.
Lambda are worthwhile, but we need to convert them to a more compilable form: the closure.

Closures make the implicit capturing of lambdas explicit.
A closure is a struct with a field for our lambda body and a field for each captured value in our lambda's environment.
Returning to our `bar` example, `bar` would become the closure:

```rs
fn bar_impl(env: Bar, x: usize) -> usize {
  x * env.y + env.a
}

let bar = Bar {
  fun: bar_impl,
  a: 1,
  y: 10,
}
```

What was originally the body of our lambda now resides in a top level function `bar_impl`.
This is a hardware function we know how to execute, a step in the right direction.
Within the body of the lambda we've substituted our accesses to free variables `a` and `y`.
They now reference our new `env` parameter.

When we create a top level function from our lambda body, we add a new parameter `env`.
It provides explicit access to our, previously implicitly accessed, captured variables.
Our `env` parameter shares its type, `Bar`, with our closure.
Because `Bar` has one field per captured variable and also a field `fun`, which holds a reference to our generated top level function, it serves dual duty as both our closure type and our environment type.

The translation from lambda to closure permeates into application.
Our closures are a struct, not a function.
Accordingly, where previously we would pass an argument to our lambda:

```rs
bar(3)
```

We must now call our closures `fun` field and pass along our `env` (the closure itself):

```rs
bar.fun(bar, 3)
```

## Lambda Lifting

In the abstract, that's all there is to closure conversion.
We transmogrify every function and application we encounter, and we're done.
It's an invasive operation, so invasive in fact, that originally I planned to avoid it entirely.
At first, this pass used lambda lifting, an operation that also removes lambdas but eschews closures.
Rather than creating a struct to hold our captured variables, lambda lifting lifts each `Fun` into a new top level function and adds a new parameter per free variable.
We can see the distinction by lambda lifting our `bar` example:

```rs
let a = 1;
let y = 10;
// A lambda
let bar = |x: usize| x * y + a;
bar(3)
```

becomes:

```rs
fn bar(
  a: usize, 
  y: usize, 
  x: usize
) -> usize {
  x * y + a
}

bar(1, 10, 3)
```

We adjust our `bar` call to pass `a` and `y` directly and our lambda vanishes, in its place a stateless top level function.
There's no need for structs or closures.
Lambda lifting is attractive because of this simplicity, but it comes at a cost.
Consider another common usage of lambdas:

```rs
fn foo(
  a: usize
) -> impl Fn(usize) -> usize {
  |b: usize| a + b
}
```

Lifting this lambda doesn't go great:

```rs
fn foo_lambda(a: usize, b: usize) -> usize {
  a + b
}

fn foo(
a: usize
) -> impl Fn(usize) -> usize {
  foo_lambda(a, ???)
}
```

Our lambda exists to save `a` until a later point when `b` is available.
A closure handles this just fine because it's a struct that can hold onto `a`.
But there's no valid top level function that fulfills this role, precluding the use of lambda lifting.
A top level function must take all its parameters at once which is incompatible with returning a lambda.

Lambda lifting, unlike closure conversion, only provides a partial solution.
Not to say it's not worthwhile, avoiding closures is admirable, to the point production compilers perform [selective lambda lifting](https://arxiv.org/abs/1910.11717).
But for our simple compiler here, we're utilizing closure conversion, since it handles all cases.
Our selection, however, comes at a cost.


## New IR

The price we pay for closure conversion is a new `IR`
It will be quite similar to our existing `IR` from [lowering](/posts/lowering-base-ir), but in place of `Fun` we'll find `Closure`.
The reason for this new `IR` is twofold:

* Closures are a different construct from functions, and we want to capture that in our `IR`.
* Closure conversion makes use of new types not available in our lowering `IR`.

With those goals in mind our new `IR` looks like:

```rs
enum IR {
  Var(Var),
  Int(i32),
  Closure(Type, ItemId, Vec<Var>),
  Apply(Box<Self>, Box<Self>),
  Local(Var, Box<Self>, Box<Self>),
  Access(Box<Self>, usize),
}
```

`Fun` has become `Closure` and `App` has become `Apply`.
We also have an entirely new node `Access`.
`Access` is how we query captured variables out of our `env` parameter.
Aside from that, the rest of our `IR` remains the same.

While we introduced closures as structs, we find no `Struct` node among our `IR`.
Make no mistake, closures are structs.
We sequester them as their own `Closure` node to smooth over typing issues presented by closures.

All lambdas are considered the same type, regardless of captures.
Closures explicitly do away with this, but we'd still like to treat closures like opaque functions when we apply them.
We'd like to treat the closure types:

* `{ fun: fn(usize) -> usize }`
* `{ fun: fn(usize) -> usize, a: usize }`
* `{ fun: fn(usize) -> usize, a: usize, b: usize }`

Uniformly as `fn(usize) -> usize` when we type check them during application.
Accommodating this wrinkle is most easily accomplished by introducing a specific node for it, so we know to type check closures differently from normal structs.

Our closure node introduces two other data types we'll need for conversion: `Type` and `ItemId`.
`ItemId` mirrors `VarId`, but for top level functions rather than local variables.
Closures require top level functions, so we add items here despite their absence in the AST and lowering IR.
When we turn our lambda body into a top level function, our closure will reference that top level function via `ItemId`.

`Type` is our new IR's parallel to lowering's type:

```rs
enum Type {
  Int,
  Closure(Box<Self>, Box<Self>),
  ClosureEnv(Box<Self>, Vec<Self>),
}
```

Just like IR, we've replaced `Fun` types with `Closure` types.
Our new type, `ClosureEnv`, is the type we'll give to `env` parameters.
`Closure` and `ClosureEnv` are two types for the same value.
Any given closure receives a `Closure` type, used to check it's passed the right argument, but also receives a `ClosureEnv` type which is used when we pass the closure to its top level definition as `env`.

## Implementation

{{< accessory title="Quick Refresher" >}}

Our middleend passes all work on a common `IR`:

```rs
enum IR {
  Var(Var),
  Int(i32),
  Fun(Var, Box<Self>),
  App(Box<Self>, Box<Self>),
  TyFun(Kind, Box<Self>),
  TyApp(Box<Self>, Type),
  Local(Var, Box<Self>, Box<Self>),
}
```

Our IR is explicitly typed and represents generics using type functions and type applications.
It also introduces `Locals` which are not strictly required, but we'll see are very useful in simplification.
Alongside our IR, we have a `Type` (that is lowered from our AST's type):

```rs
enum Type {
  Int,
  Var(TypeVar),
  Fun(Box<Self>, Box<Self>),
  TyFun(Kind, Box<Self>),
}
```

Our `Type` has a `Kind`, unparalleled in our AST.
The same way values have types, types have kinds.
Our base `Kind` is vacuous:

```rs
enum Kind {
  Type
}
```

A `Type` is of kind `Type`.
I believe it to be true, but I question if we really need to postulate such a circular statement.
That's everything we need to know about our IR, back to closure conversion.

{{</ accessory >}}

The start of the implementation is upon us.
We take this one from the top starting with our entry point `closure_convert`:

```rs
fn closure_convert(
  ir: lowering::IR
) -> ClosureConvertOutput {
  todo!()
}
```

`ir` is the IR we've known till now.
We have to specify it's `lowering::IR`, since closure conversion introduces its own `IR`.
`closure_convert` returns a `ClosureConvertOutput`, which is a bundle of items:

```rs
struct ClosureConvertOutput {
  item: Item,
  closure_items: BTreeMap<ItemId, Item>,
}
```
We're going to turn our input `ir` into a top level function. 
That function will be `item` in our output.
The rest of the items in `closure_items` are created to hold lambda bodies we encounter during conversion.
An item, as a top level function, is a series of parameters and a body:

```rs
struct Item {
  params: Vec<Var>,
  ret_ty: Type,
  body: IR,
}
```

For convenience, we keep the return type in our `Item`, but this is not strictly necessary.
`Item` looks a lot like a function except we know it will never capture variables, making it easier to emit code for items.
Now that we know where we're going, let's start on how to get there

```rs
fn closure_convert(ir: lowering::IR) -> ClosureConvertOutput { 
  let (params, ir) = ir.split_funs();
  let mut var_supply = VarSupply::default();
  let mut env = im::HashMap::default();

  todo!()
}
```

Our first line makes use of `split_funs`, a function we've seen before in [simplification](/posts/simplify-base).
It collects contiguous functions nodes at the root of our `IR` and returns a list of their parameters and the body inside those functions nodes.
If our IR lacks functions at its root, `params` will be an empty list and body will be the IR itself.
We separate our top level parameters like this to avoid converting them to closures.

When we convert our IR to an item, we don't want its body to be a series of nested closures.
We only want to start converting functions to closures once we're inside all the root functions.
After that we have some rote supporting structures:

* `var_supply` will provide any `Var`s we need throughout conversion
* `env` maps `lowering::Var`s to our new `Var`.

With whetted whistles, our next task is determining the parameters and return type of our final item:

```rs
let params = params
  .into_iter()
  .map(|param| match param {
    Param::Ty(_) => panic!("ICE: Type function encountered after monomorphizing"),
    Param::Val(lower_var) => {
      let id = var_supply.supply_for(lower_var.id);
      let var = Var {
        id,
        ty: lower_ty(&lower_var.ty),
      };
      env.insert(lower_var, var.clone());
      var
    }
  })
  .collect();
let ret_ty = lower_ty(&ir.type_of());
```

Closure conversion follows monomorphization, so we should no longer have any type parameters.
For each value parameter, we create a fresh variable and insert it into `env`. 
As part of creating the new variable we lower the type of our variable.
We also lower the type of our overall `ir` to produce the return type.

These are both accomplished by `lower_ty`, a method that turns a `lowering::Type` into our new `Type`.
Its implementation is straightforward:

```rs
fn lower_ty(ty: &lowering::Type) -> Type {
  match ty {
    lowering::Type::Int => 
      Type::Int,
    lowering::Type::Fun(arg, ret) => 
      Type::closure(
        lower_ty(arg), 
        lower_ty(ret)),
    lowering::Type::Var(_) 
    | lowering::Type::TyFun(_, _) => 
      panic!("ICE: Type function or variable appeared in closure conversion. This should've been handled by monomorphization."),
  }
}
```

`Fun` becomes `Closure` and `Int` becomes `Int`.
We shouldn't see any generics, so we panic if those show up.
Returning to `closure_convert`, we can begin actual conversion:

```rs
let mut conversion = ClosureConvert {
  var_supply,
  item_supply: Default::default(),
  items: Default::default(),
};
let body = conversion.convert(ir, env);
```

`ClosureConvert` holds the state we'll need while converting:

```rs
struct ClosureConvert {
  var_supply: VarSupply,
  item_supply: ItemSupply,
  items: BTreeMap<ItemId, Item>,
}
```

`ItemSupply`, like `VarSupply`, provides fresh `ItemId`s as needed.
`var_supply` we've seen before, you can find `VarSupply`'s implementation in the [full code](https://github.com/thunderseethe/making-a-language/tree/main/closure_convert/base) if you're curious.
`items` holds the item we generate for closures, it will eventually become `closure_items` in our output.
`ClosureConvert` exists to provide the `convert` method:

```rs
fn convert(
  &mut self, 
  ir: lowering::IR, 
  env: im::HashMap<lowering::Var, Var>
) -> IR {
  match ir {
    // ...
  }
}
```

`convert` is where we both convert our `lowering::IR` to our new IR, and convert our lambdas to closures.
It takes the form of match on our input `ir`.
We'll cruise through our first few cases.
They're boilerplate to translate from `lowering::IR` to `IR`:

```rs
lowering::IR::Int(i) => 
  IR::Int(i),
lowering::IR::Var(var) => 
  IR::Var(env[&var].clone()),
lowering::IR::TyFun(_, _) 
| lowering_base::IR::TyApp(_, _) => {
  panic!("ICE: Generics appeared after monomorphizing")
}
lowering::IR::Local(var, defn, body) => {
  let defn = self.convert(*defn, env.clone());
  let v = Var {
    id: self.var_supply.supply_for(var.id),
    ty: lower_ty(&var.ty),
  };
  let body = self.convert(*body, env.update(var, v.clone()));
  IR::local(v, defn, body)
}
```

`Local` has more code than our other cases, but ultimately it's converting each of its components and reconstructing a `local`.
`Fun` is where we actually start changing things:

```rs
lowering::IR::Fun(fun_var, body) => {
  let var = Var {
    id: self.var_supply.supply_for(fun_var.id),
    ty: lower_ty(&fun_var.ty),
  };
  self.make_closure(
    var.clone(),
    *body,
    env.update(fun_var, var))
}
```

We convert our bound parameter and delegate to our workhorse `make_closure`:

```rs
fn make_closure(
  &mut self,
  var: Var,
  body: lowering_base::IR,
  env: im::HashMap<lowering::Var, Var>,
) -> IR {
  todo!()
}
```
As is hopefully clear from the name, `make_closure` takes the pieces of a lambda and makes a closure from them.
Before we can produce a closure from our lambda, some analysis is required.
We need to:

* Determine the free variables of our body.
* Replace each use of a free variable with an `env` access.
* Create an item to house our converted body.

Completing these sub-quests will prime us for closures creation.
Determining the free variables of our body is easy enough, we just ask for them:

```rs
let ret = lower_ty(&body.type_of());
let mut body = self.convert(body, env);
let mut free_vars = body.free_vars();
free_vars.remove(&var);
```

Before we determine the free variables we convert our body to our new IR.
Converting our `body` consumes it, so before that we save our return type using `type_of`.
`free_vars` walks our term constructing the set of free variables, you can find its implementation in the [source code](https://github.com/thunderseethe/making-a-language/tree/main/closure_convert/base).
We'll trust it works as advertised here.
From the perspective of `body`, the variable bound by our lambda is free.
We happen to know it's immediately bound by aforementioned enclosing lambda, so we go ahead and remove it from the free variable set.

Next we need to migrate our free variables to environment accesses.
We begin by creating a variable for `env`:

```rs
let vars: Vec<Var> = 
  free_vars.iter()
           .cloned()
           .collect();
let closure_ty = 
  Type::closure(var.ty.clone(), ret.clone());
let env_var = Var {
  id: self.var_supply.supply(),
  ty: Type::closure_env(
    closure_ty.clone(),
    vars.iter()
        .map(|var| var.ty.clone())
        .collect(),
  ),
};
```

The type of our `env` is determined by the types of our free variables, and the type of our closure.
With `env`, we can construct a substitution to replace our free variables:

```rs
let subst = free_vars
  .into_iter()
  .enumerate()
  .map(|(i, var)| {
    let id = self.var_supply.supply();
    let new_var = Var {
      id,
      ty: var.ty.clone(),
    };
    body = IR::local(
      new_var.clone(),
      IR::access(IR::Var(env_var.clone()), i + 1),
      body.clone(),
    );
    (var, new_var)
  })
  .collect::<HashMap<_, _>>();

body.rename(&subst);
```

Each of our free variables becomes an `Access` of our env.
Surprisingly, those accesses are offset by one.
We need that offset because our `env` is also our closure.
Its first field is the reference to our top level function.
Our free variables only start appearing at index one and onwards.

The order of accesses is important. 
We need the index used to access `env` to sync up with the order of variables we use to construct our closure.
Fortunately, `free_vars` is a `BTreeSet` which keeps a consistent order for us.
While creating `subst`, we modify `body` wrapping it in locals during iteration.
These locals perform our env accesses once at the start of our body rather than multiple times throughout.

Rather than translating our closure body `x * y + a` into `x * env.y + env.a`, it becomes:

```rs
let t0 = env.y;
let t1 = env.a;
x * t0 + t1
```

In this contrived example the change seems negligible, maybe even a downgrade.
For most closures, however, we expect this change to increase sharing and reduce the number of heap accesses performed.
With the caveat, we may unnecessarily load a value we otherwise don't use.
For example, `if x { env.a } else { env.b }` becomes:

```rs
let t0 = env.a;
let t1 = env.b;
if x { t0 } else { t1 }
```

Whoops! All of a sudden we always load both our environment's fields rather than the specific one we need.
We're willing to make this tradeoff, for now.
Code like this is unlikely to appear in the wild.
We can come back over and use a more sophisticated heuristic if we make it far enough for this to be a problem.

Because of this approach, `subst` is a `HashMap<Var, Var>` rather than a `HashMap<Var, IR>`.
This allows us to use `rename` on body.
`rename` traverses our term and updates any variables that appear in `subst`.
An excerpt suffices to show its functionality:

```rs
fn rename(&mut self, subst: &HashMap<Var, Var>) {
  match self {
    IR::Var(var) => {
      if let Some(new_var) = subst.get(var) {
        *var = new_var.clone();
      }
    }
    // the rest of our cases are standard
  }
}
```

Our final task in `make_closure` is to create our item:

```rs
let params = vec![env_var, var];
let item = self.item_supply.supply();
self.items.insert(
  item,
  Item {
    params,
    ret_ty: ret,
    body,
  },
);
IR::Closure(closure_ty, item, vars)
```

Our item consists of our two parameters env and the variable provided by our lambda, the return type, and our refurbished lambda body.
We save this in `items` as one of our closures top level functions.
The resulting closure is constructed from our type, item, and free variables.

We take our knowledge of closure construction back to `convert` to handle our final case:

```rs
lowering::IR::App(fun, arg) => {
  let closure = self.convert(*fun, env.clone());
  let arg = self.convert(*arg, env);
  IR::apply(closure, arg)
}
```

Oh uh...huh.
I kind of expected more fanfare after the build up for `Fun`.
We kind of just recursively call `convert` and construct an `Apply`.

{{< accessory title="Why do we even need Apply?" >}}
`Apply` looks like it could just be `App`.
And that's true, we could avoid the rename.

The reason we rename it is to highlight its usage with closures.
Eventually we will have top level items, and when we do those can use `App` normally to pass arguments.
Then our distinction will becomes meaningful: `Apply` unpacks and calls a closure and `App` calls a top level item.
{{< / accessory >}}

Okay well maybe that's all our cases for `convert`, but surely there's still some work left in `closure_convert`:

```rs
fn closure_convert(ir: lowering::IR) -> ClosureConvertOutput {
  // ... all our previous code
  let body = conversion.convert(ir, env);
  ClosureConvertOutput {
    item: Item {
      params,
      ret_ty,
      body,
    },
    closure_items: conversion.items,
  }
}
```

Hm, not a lot going on here either.
Once we've converted `ir` we build our output and return it immediately.

I was expecting worse. 
Our biggest changes are really localized to `Fun`.
Now seems like a prime opportunity to talk about what we didn't do!
Our closure conversion makes some simplifying assumptions.
The largest of which is that each of our closures takes a single parameter.

When we convert multiple nested lambdas:

```rs
IR::fun(...,
  IR::fun(...,
    IR::fun(..., <body>)))
```

Each `Fun` get its own struct and top level function.
But that's a bit of waste, isn't it?
Our first two functions immediately return closures.
The action doesn't start until we reach `<body>`.

It'd be nice if we could produce one struct and one top level function for the "real work" of our function.
Our struct could have fields for our first two parameters, and only call our function once all three parameters were available.
Of course, this is how production closure conversion implementations handle chained lambdas.
It's hard to deny the benefits.
You pay for it in implementation complexity, however.

The details can be found in [Making a Fast Curry](https://simonmar.github.io/bib/papers/eval-apply.pdf), an accessible rundown on closures and their required runtime support.
Multi argument closures require arity tracking to determine when applications should save an argument vs calling a top level function.
This answer is obvious when all our closures have a single argument.
We're willing to lose out on the potential performance to make our lives easier, but it's worth knowing alternatives are out there.

## Working an Example

Let's walk through an example to see how our previous IR turns into our closure conversion IR.
Because our IR terms are going to be large, we'll show them using the pretty printing format.
The format omits types for brevity, but the types are still present in the underlying IR.
I'll use human legible names for this example, but feel free to imagine them as `v0`, `v1`, etc. as they would be in the real IR.
We'll start with the lowering IR:

```lisp
(fun [add]
  (let [
    (b 3)
    (f (fun [x] (add x 1)))
    (g (fun [a] (add (f a) (f b))))
    (id (fun [t] t))
  ] (id (g 2))))
```

We have three prime candidates for closure conversion (recall that our top level function is not converted): `f`, `g`, and `id`.
Our first lambda, `f`, contains one free variable `add`.
We create a fresh item to hold it's body and replace `add` by an access of the `env` parameter we create:

```lisp
(defn f_clos [env, x]
  (let 
    [(t0 env[1])]
    (apply (apply t0 x) 1)))
```

During item creation, we convert `f`'s body, turning our `App` nodes into the new `Apply` node.
We also let bound our env access and rename `add` to its new name `t0`.
The zeroth element of env is a reference to `item0` itself
`g` undergoes a similar transformation:

```lisp
(defn g_clos [env, a]
  (let [
    (t0 env[1])
    (t1 env[2])
    (t2 env[3])
  ] (apply (apply t0 (apply t1 a)) (apply t1 t2))))
```

This time we have more free variables to let bind.
`g` captures `add`, `f`, and `b`, becoming `t0`, `t1`, and `t2` respectively.
It really becomes clear with the shift to `apply` how often we're unpacking closures.
The final lambda, `id`, doesn't actually capture any variables, but still we closure convert it, when all you have is a hammer:

```lisp
(defn id_clos [env, t] t)
```

Our lack of captured variables keeps our `id` item short, but we still introduce an `env` parameter even though it's devoid of captures.
Putting it all together our final IR is a set of four items:

```lisp
(defn f_clos [env, x]
  (let 
    [(t0 env[1])]
    (apply (apply t0 x) 1)))

(defn g_clos [env, a]
  (let [
    (t0 env[1])
    (t1 env[2])
    (t2 env[3])
  ] (apply (apply t0 (apply t1 a)) (apply t1 t2))))

(defn id_clos [env, t] t)

(defn main [add] 
  (let [
    (b 3)
    (f (closure f_clos [add]))
    (g (closure g_clos [add, f, b]))
    (id (closure id_clos []))
  ] (apply id (apply g 2))))
```

We can see our three `fun`s have been replaced by closures.
Our closures reference the top level function we generated for them, and also track the variables captured explicitly.
We use this list of captures to construct the struct that will act as our closure.

Closure conversion removes one of the last remaining obstacles to code emission: variable capture.
Our IR now contains integers, "structs" (we don't have general structs but closures count), and top level functions.
All constructs that are close enough to the hardware, we can see how they map onto the machine.
As always you can find all the details in the [making a language repository](https://github.com/thunderseethe/making-a-language/tree/main/closure_convert/base).
Our next pass is an exciting one, [emitting code](/posts/emit-base).
