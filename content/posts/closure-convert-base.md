+++
title = "ClosureConvert[0].Base: Removing Functions From Our Language With Closure Conversion"
date = "2025-04-30T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "ClosureConvert"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Functions", "IR", "Compiler", "Runtime", "Closure", "Closure Conversion", "Lambda Lifting"]
description = "Converting our functions into closures for the Base IR."
draft = true
+++

* Intro
  * Explain our functions are lambdas
    * Explain lambdas?
  * Like polymorphism, no machine instructions for executing lambdas
  * Exemplify problem with variable capture when returning closures.
  * We have to convert them into a form we can execute

* Closure Conversion
  * Closure
    * Captures in scope variables.
      * Possibly explained with lambdas above.
    * A struct containing a field that points to our function body and a field for each of our captured variables
  * Convert each `Fun` node to a closure
    * We must also update our `App` to call closures rather than functions.
  * Our function body will be turned in to a top level item (that captures no environment).
    * In place of implicit capturing, it will take an explicit parameter to the env.
    * Each access of a captured variable in the body will be replaced by indexing into the env parameter.
  * Explain runtime semantics
    * Functions will now allocate a closure struct with the captured environment.
    * Applications will grab the function pointer out of a closure struct and pass the env and argument to the function.    
  * TODO: Do we need to explain more about closures here?

* Tangent: Figure out how to incorporate this organically.
  * Closure conversion is invasive.
  * Originally wanted to use a different techinque lambda lifting
  * Explain lambda lifting with an example.
  * Explain where lambda lifting fails (returning a lambda, partial applications).
  * Lambda lifting is not a wholistic solution, so we closure convert instead.

* Closure conversion requires a new IR.
  * Closures are a fundamentally different type from functions because they track their env.
  * A function type `Type::fun(Type::Int, Type::Int)` turns into one closure type per captured environment.
    * Part of why we have to update `App`.
  * Alongside this main change, our new IR introduces some other changes
    * Top level items
    * Structs to represent environment parameters.
    * Note: These are things we will add to our first IR in rows and items, but we need them in base for closure conversion and we need the new IR anyways.
  * TODO: IR code snippet.
  * Explain what stays the same
    * Var, but with a new type
    * Int
    * Local

* IR Type
  * With the changes to our IR we need a new type for our IR
  * TODO: IR type code snippet.
  * Explain Closure and ClosureEnv
    * Why not Struct for both?
    * Makes our lives easier to remember them as special types during code emission

* Main entry point
  * `closure_convert`
  * TODO: Signature code snippet
  * Takes in lowering::IR spits out `ClosureConvertOutput`.
  * TODO: ClosurecConvertOutput snippet
  * A collection of items produced by lifting function bodies to top level items and the final item representing our input IR.

* closure_convert impl
  * split funs, we don't want to turn top level functions into closures.
    * explain why
  * `var_supply` we've seen before, but now it converts lower::Var into Var
  * `env` tracks mapping from lowering::Var to Var.
  * Convert parameters and insert them in env
  * Lower ret_ty, used for convenience in the item we construct at the end.

* lower_ty
 * TODO: Code snippet
 * Int turns into I32
 * Fun turns into Closure
 * TyFun and Var shouldn't appear because this is post monomorphizing
 * Explain why nothing creates a ClosureEnv type? 

* Construct `ClosureConvert`
* TODO: ClosureConvert snippet
  * item_supply is like var_supply but for top level items
  * items holds the top level items we create for closures

* convert
  * TODO: Signature code snippet.
  * Takes in lowering::IR and env, spits out our new IR
  * Case by case 
    * TODO: Int snippet
    * Int turns into Int

    * TODO: Var snippet
    * Var turns into Var by look up in env
    * Should we talk about why we don't convert it each time?
    
    * TODO: Fun snippet
    * Simple on the surface
    * Convert to var and call make_closure

* make_closure
  * lower return type
  * convert body
  * determine free vars
  * remove our parameter from free vars.
  * create env_var from captured vars.
  * substitute body to replace captured vars with environment accesses.
  * construct an item for our closure body
  * return closure

* Back in `convert`
  * TODO: `App` snippet
  * Turn into an `apply`
  * Should we talk about distinction between apply and app?

  * TODO: `Local` snippet
  * Nothing noteworthy

  * TyFun and TyApp should not appear

* Clean up in convert

* Caveats
  * Closures are single parameter rather than multi parameter
    * Making of a fast curry
  * Cheating on typing
    * Proper typing requires existentials, instead we've just special cased the behavior

* Conclusion
  * Final step before code generation.
  * Closures are something we can see how to implement in a machine, unlike functions.
  * Closure conversion provides a wholistic solution, unlike lambda lifting, at the cost of being more complex and passing around allocated closures.

Our [previous pass](/posts/monomorph-base) stripped away polymorphism from our compilation path.
Today we've come to cut down an even closer confidant, functions.
This may come as a shock to some, functions are entwined deeply in our language.
I mean it's right there in the name, functional programming.
I don't know what al programming is, and I don't want to.

Without functions, only integers remain.
It's all ones and zeroes at the end of the day, but I'm not sure how to compute with just integers.
I'd at least need tape of some fashion.

Allow me to assuage your concerns.
We're removing functions, but we'll be replacing them with something new.
Before looking at what replaces functions, let's talk some about why we're replacing functions.

Unlike polymorphism, you will find instructions on a machine for functions.
Returning and passing arguments are common instructions you'll find among most machine code flavors, begging the question of why are we removing functions?
The dissonance arises from a distinction in definition of function.
When we talk about functions in our language, we really mean lambdas.

Lambdas are functions that capture their environment.
Our hardware functions lack this capability.
Rust can help us highlight the difference:

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

`foo` a hardware function requires no environment we pass it the requisite argument and receive our answer.
`bar` on the other hand captures the variables `a` and `y` in an environment that is made available every time we call `bar`.

The convenience of variable capture cannot be overstated.
A myriad of programming patterns can be boiled down to lambdas capturing other lambdas.
Lambda are worthwhile, but we need to convert them to a more compilable form: the closure.

A closure is a struct with a pointer to our function and a field for each captured value.
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
Our `env` parameter is the same type as our closure `Bar`.
`Bar` has one field per captured variable but also has a field `fun` which holds a reference to our generated top level function.

The translation from lambda to closure pervades application.
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

In the abstract, that's closure conversion.
We transmogrify every function and application we encounter, and we're done.
It's an invasive operation, so invasive in fact, that originally I planned to avoid it entirely.
My original plan for this pass was to use lambda lifting, an operation that fulfills a similar role but eschews closures.
Rather than creating a struct to hold our captured variables, lambda lifting lifts each function into a new top level definition and adds a new parameter for each free variable.
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
fn bar(a: usize, y: usize, x: usize) -> usize {
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

The whole point of our lambda is to save `a` until a later time at which point `b` is available.
A closure handles this just fine because it's a struct that can hold onto `a`.
But there's no valid top level function lambda lifting can provide here.
A top level function must take all its parameters at once which is incompatible with the delayed application we want to achieve in our example.

Lambda lifting, unlike closure conversion, is not a holistic solution.
Not to say it's not worthwhile, avoiding closures is admirable, to the point production compilers perform [selective lambda lifting](https://arxiv.org/abs/1910.11717).
But for our simple compiler here, we're going with closure conversion as a baseline since it handles all cases.
Our selection, however, comes at a cost.

TODO: Figure out where to put quick refresher

## New IR

We're going to need a new `IR` for closure conversion.
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
`Access` is how we pull our captured variables out of our `env` parameter.

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
Aside from that the rest of our `IR` remains the same.

Our closure node introduces two other data types we'll need for conversion: `Type` and `ItemId`.
`ItemId` is similar to `VarId`, but for top level functions rather than local variables.
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

We've replaced `Fun` types by `Closure`.
Our new type `ClosureEnv` is the type we'll give to the `env` parameter.
`Closure` and `ClosureEnv` are two types for the same value.
Any given closure receives a `Closure` type, used to check it's passed the right argument, but also receives a `ClosureEnv` type which is used when we pass the closure to its top level definition as `env`.

## Implementation

With that we can start on our implementation.
We take this one from the top starting with our entry point `closure_convert`:

```rs
fn closure_convert(
  ir: lowering::IR
) -> ClosureConvertOutput {
  todo!()
}
```

`ir` is the IR we've known till now.
We have to specify it's `lowering::IR` since closure conversion introduces its own `IR`.
Our output is a bundle of items:

```rs
struct ClosureConvertOutput {
  item: Item,
  closure_items: BTreeMap<ItemId, Item>,
}
```

`item` is the item produced by converting our input `ir`.
`closure_items` contains an item per lambda that was converted into a closure.
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

`split_funs` we've seen before in [simplification](/posts/simplify-base).
It collects contiguous functions nodes at the root of our `IR` and returns a list of their parameters and the body inside those functions nodes.
If our IR lacks functions at its root, `params` will be an empty list and body will be the IR itself.
We separate our top level parameters like this to avoid converting them to closures.

When we convert our IR to an item, we don't want its body to be a series of nested closures.
We only want to start converting functions to closures once we're inside the root function nodes.
After that we have some rote supporting structures:

* `var_supply` will provide any `Var`s we need throughout conversion
* `env` maps `lowering::Var`s to our new `Var`.

With whetted whistles, our next task is determining the parameters and return type of our end item:

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

`lower_ty` is straightforward:

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

We shouldn't see any generics, so we panic if those show up.
`Fun` becomes `Closure` and `Int` becomes `Int`.
Back in `closure_convert`, we can begin actual conversion:

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

`ItemSupply`, same as `VarSupply`, provides fresh `ItemId`s as needed.
`ClosureConvert` exposes the `convert` method:

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

`Local` has more code than our other cases, but ultimately it's converting each of it's subterms and reconstructing a `local`.
Unsurprisingly, `Fun` is where we actually start changing things:

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

Before we can produce a closure from our lambda's body, some analysis is required.
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
`free_vars` walks our term constructing the set of free variables, you can find its implementation in the [source code](TODO).
We'll trust it works as advertised here.
From the perspective of `body`, the variable bound by our lambda is free, so we remedy that after constructing our free variable set.

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

The order here is important, we need the `i + 1` used to access `env` to sync up with the order we use to construct our closure.
Fortunately, `free_vars` is a `BTreeSet` which takes cares of keeping a consistent order for us.
While creating `subst`, we modify `body` wrapping it in locals during iteration.
This serves to perform our env accesses once at the start of our body rather than multiple times throughout.

Rather than translating our closure body `x * y + a` into `x * env.y + env.a`, it becomes:

```rs
let t0 = env.y;
let t1 = env.a;
x * t0 + t1
```

In this contrived example the change seems negligible, maybe even a downgrade.
For most closures, however, we expect this change to increase sharing and reduce the number of heap accesses performed.
The downside is we may unnecessarily load a value we otherwise don't use.
For example, `if x { env.a } else { env.b }` becomes:

```rs
let t0 = env.a;
let t1 = env.b;
if x { t0 } else { t1 }
```

Whoops! All of a sudden we always load both our environment's fields rather than the specific one we need.
We're willing to make this tradeoff, however.
Code like this is also unlikely in the wild.
We can come back over and use a more sophisticated heuristic if we make it far enough for this to be a problem.

Because of this approach, `subst` is a `HashMap<Var, Var>` rather than a `HashMap<Var, IR>`.
This allows us to use `rename` on body.
`rename` traverses our term and updates any variables that appear in `subst`.
We'll only look at an excerpt here:

```rs
fn rename(&mut self, subst: &HashMap<Var, Var>) {
  match self {
    IR::Var(var) => {
      if let Some(new_var) = subst.get(var) {
        *var = new_var.clone();
      }
    }
    // the rest of cases are banal
  }
}
```

If a variable appears in `subst`, we swap it.
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
We add this to `items` as one of our closures top level functions.
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
Once we've converted `ir` we build our output and return it.

I was expecting worse, our changes were really localized to `Fun`.
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
Our first two functions immediately return lambdas.
The action doesn't start until we reach `<body>`.

It'd be nice if we could produce one struct and one top level function for the "real work" of our function.
Our struct could have fields for our first two parameters, and only call our function once all three parameters were available.
Of course, this is how production closure conversion implementations handle chained lambdas.
It's hard to deny the benefits.
You pay for it in implementation complexity, however.

Details can be found in [Making a Fast Curry](https://simonmar.github.io/bib/papers/eval-apply.pdf), an accessible rundown on closures and their required runtime support.
Multi argument closures require arity tracking to determine when applications should save an argument vs calling a top level function.
This answer is obvious when all our closures have a single argument.
We're willing to lose out on the potential performance to make our lives easier, for now.
But it's worth knowing what alternatives are out there.

Closure conversion removes one of the last remaining obstacles to code emission: variable capture.
Our IR now contains integers, "structs" (we don't have general structs but closures count here), and top level functions.
All constructs that are close enough to the hardware that we can see how they map onto the machine.
