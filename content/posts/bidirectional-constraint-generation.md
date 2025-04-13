+++
title = "Types[0].Base[1]: Bidirectional Constraint Generation"
date = "2023-06-24T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Type Inference"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Type Inference", "Bidirectional Typechecking", "Constraint Generation", "Polymorphism"]
description = "Generate Type Constraints with a Bidirectional Type System"
+++
[Last time](/posts/type-inference/), we laid out the AST and Type for the language we are building. We also got a bird's-eye view of our type inference algorithm: constraint generation, constraint solving, substitute our solved types. This time we're implementing the constraint generation portion of our type inference algorithm.

Our passes will need to share some state between each other. We introduce a `TypeInference` struct to hold this shared state and implement our passes as methods on that struct:
```rs
struct TypeInference  {
  unification_table: InPlaceUnificationTable<TypeVar>,
}
```
We'll talk more about `unification_table` when we talk about constraint solving. For now, it's enough to think of it as a mapping from type variables to type, and we'll use it in constraint generation to generate new type variables and keep track of them for later.

## Constraint Generation
We generate our set of constraints from contextual information in our AST.
To get this context we need to visit every node of our AST and collect constraints for that node.
Traditionally this is done with a bottom-up tree traversal (e.g. HM's Algorithm J).
We visit all of a node's children and then use the children's context to better infer the type of our node.
This approach is logically correct.
We always infer the correct type and type error when we should.
While correct, this approach doesn't make the most efficient use of information available.
For example, in an application node we know that the function child must have a function type.
Since types are only inferred bottom-up, we have to infer an arbitrary type for our function node and add a constraint that the inferred type must be a function.

In recent years, a new approach to type checking called Bidirectional Type Checking has arisen to solve this inefficiency.
With Bidirectional Type Checking we have two modes of type checking:

 * `infer` - works the same as the HM type systems we just described, so types are inferred bottom-up. 
 * `check` - works in the opposite direction, top-down. A type is passed into `check`, and we _check_ that our AST has the same type.

Our two modes will call each other mutually recursively to traverse the AST.
Now when we want to type check an application node, we have a new option.
We can construct a function type at the application node and check the function node against our constructed function type.
Making better use of top-down contextual info like this allows us to generate fewer type variables and produce better error messages.
Fewer type variables may not immediately appear as a benefit, but it makes the type checker faster.
Our constraint solving is in part bound by the number of type variables we have to solve.
It also makes debugging far easier. 
Each type variable acts as a point of indirection so fewer is better for debugging.
So while Bidirectional Type Checking doesn't allow us to infer "better" types, it does provide tangible benefits for a modest extension of purely bottom-up type inference.

Now that we know how we'll be traversing our AST to collect constraints, let's talk about what our constraints will actually look like.
For our first Minimum Viable Product (MVP) of type inference, `Constraint` will just be type equality:

```rs
enum Constraint {
  TypeEqual(NodeId, Type, Type)
}
```

We'll talk more about what it means for two types to be equal during constraint solving.
For now, it's sufficient to produce a set of type equalities as a result of our constraint generation.
We attach a `NodeId` to our `Constraint`, so we can remember where the constraint came from for error reporting.

Constraint generation will be implemented on our `TypeInference` struct with 3 methods:
```rs
impl TypeInference {
  fn fresh_ty_var(&mut self) 
    -> TypeVar { ... }

  fn infer(
    &mut self, 
    env: im::HashMap<Var, Type>, 
    ast: Ast<Var>
  ) -> (GenOut, Type) { ... }

  fn check(
    &mut self, 
    env: im::HashMap<Var, Type>, 
    ast: Ast<Var>, 
    ty: Type
  ) -> GenOut { .. }
}
```
`fresh_ty_var` is a helper method we're going to brush past for now.
(We'll have a lot to cover in constraint solving!) It uses our `unification_table` to produce a unique type variable every time we call it. 
Past that, we can see some parallels between our `infer` and `check` method that illustrate each mode.
`infer` takes an AST node and returns a type, whereas `check` takes both an AST node and a type as parameters.
This is because `infer` is working bottom-up and `check` is working top-down.

Let's take a second to look at `env` and `GenOut`.
Both `infer` and `check` take an `env` parameter.
This is used to track the type of AST variables in their current scope. 
`env` is implemented by an immutable `HashMap` from the [im](https://crates.io/crates/im) crate.
An immutable hashmap makes it easy to add a new variable when it comes into scope, and drop it when the variable leaves scope.
`infer` and `check` both also return a `GenOut`.
This is a pair of our set of constraints and our typed AST:
```rs
struct GenOut {
  // Set of constraints to be solved
  constraints: Vec<Constraint>,
  // Ast where all variables are annotated with their type
  typed_ast: Ast<TypedVar>,
}
```
One final thing to note, we have no way to return an error from `infer` or `check`. We could of course panic, but for the sake of our future selves, we'll return errors with `Result` where relevant. It just so happens it's not relevant for constraint generation. Our output is a set of constraints. It's perfectly valid for us to return a set of constraints that contradict each other. We discover the contradiction and produce a type error when we try to solve our constraints. That means there aren't error cases during constraint generation. Neat! 

### infer
With our setup out of the way, we can dive into our implementation of `infer` and `check`. We'll cover `infer` first. Because our AST begins untyped, we always call `infer` first in our type inference, so it is a natural starting point. `infer` is just a match on our input `ast`:
```rs
fn infer(
  &mut self, 
  env: im::HashMap<Var, Type>, 
  ast: Ast<Var>
) -> (GenOut, Type) {
  match ast {
    Ast::Int(id, i) => todo!(),
    Ast::Var(id, v) => todo!(),
    Ast::Fun(id, arg, body) => todo!(),
    Ast::App(id, fun, arg) => todo!(),
  }
}
```
We'll talk about each case individually, let's start with an easy one to get our feet wet:
```rs
Ast::Int(id, i) => (
  GenOut::new(
    vec![],
    Ast::Int(id, i)
  ), 
  Type::Int
),
```
When we see an integer literal, we know immediately that its type is `Int`. We don't need any constraints to be true for this to hold, so we return an empty `Vec`. One step up in complexity over integers is our variable case:
```rs
Ast::Var(id, v) => {
  let ty = &env[&v];
  (
    GenOut::new(
      vec![], 
      // Return a `TypedVar` instead of `Var`
      Ast::Var(id, TypedVar(v, ty.clone())
    ),
    ty.clone(),
  )
},
```
When we encounter a variable, we look up its type in our `env` and return its type. Our env lookup might fail though. What happens if we ask for a variable we don't have an entry for? That means we have an undefined variable, and we'll `panic!`. That's fine for our purposes; we expect to have done some form of name resolution prior to type inference. If we encounter an undefined variable, we should've already exited with an error during name resolution. Past that, our `Var` case looks very similar to our `Int` case. We have no constraints to generate and immediately return the type we look up. Next we take a look at our `Fun` case:
```rs
Ast::Fun(id, arg, body) => {
  // Create a type variable for our unknown type variable
  let arg_ty_var = self.fresh_ty_var();
  // Add our agrument to our environment with it's type
  let env = env.update(arg, Type::Var(arg_ty_var));
  // Check the body of our function with our extended environment
  let (body_out, body_ty) = self.infer(env, *body);
  (
    GenOut::new(
      // body constraints are propagated
      body_out.constraints,
      Ast::fun(
        id,
        // Our `Fun` holds a `TypedVar` now
        TypedVar(arg, Type::Var(arg_ty_var)),
        body_out.typed_ast,
      ),
    ),
    Type::fun(Type::Var(arg_ty_var), body_ty),
  )
}
```
`Fun` is where we actually start doing some nontrivial inference. We create a fresh type variable and record it as the type of `arg` in our `env`. With our fresh type variable in scope, we infer a type for `body`. We then use our inferred body type and generated argument type to construct a function type for our `Fun` node.  While `Fun` itself doesn't produce any constraints, it does pass on any constraints that `body` generated. Now that we know how to type a function, let's learn how to type a function application:
```rs
Ast::App(id, fun, arg) => {
  let (arg_out, arg_ty) = self.infer(env.clone(), *arg);

  let ret_ty = Type::Var(self.fresh_ty_var());
  let fun_ty = Type::fun(arg_ty, ret_ty.clone());

  // Because we inferred an argument type, we can
  // construct a function type to check against.
  let fun_out = self.check(env, *fun, fun_ty);

  (
    GenOut::new(
      // Pass on constraints from both child nodes
      arg_out
        .constraints
        .into_iter()
        .chain(fun_out.constraints.into_iter())
        .collect(),
      Ast::app(id, fun_out.typed_ast, arg_out.typed_ast),
    ),
    ret_ty,
  )
}
```
`App` is more nuanced than our previous cases. We infer the type of our `arg` and use that to construct a function type with a fresh type variable as our return type. We use this function type to check our `fun` node is a function type as well. Our final type for our `App` node is our fresh return type, and we combine the constraints from `fun` and `arg` to produce our final constraint set.

You may wonder why we've chosen to infer the type for `arg` instead of inferring a type for our `fun` node. This would be reasonable and would produce equally valid results. We've opted not to for a few key reasons. If we infer a type for our `fun` node, it is opaque. We know it has to be a function type, but all we have after inference is a `Type`. To coerce it into a function type we have to emit a constraint against a freshly constructed function type:
```rs
let (fun_out, infer_fun_ty) = self.infer(env.clone(), *fun);
let arg_ty = self.fresh_ty_var();
let ret_ty = self.fresh_ty_var();

let fun_ty = Type::fun(arg_ty.clone(), ret_ty.clone());
let fun_constr = Constraint::TypeEqual(id, infer_fun_ty, fun_ty);

let arg_out = self.check(env, *arg, arg_ty);
// ...
```
We have to create an extra type variable and an extra constraint compared to inferring a type for `arg` first. Not a huge deal, and in fact in more expressive type systems this tradeoff is worth inferring the function type first as it provides valuable metadata for checking the argument types. Our type system isn't in that category, though, so we take fewer constraints and fewer type variables every time. Choices like this crop up a lot where it's not clear when we should infer and when we should check our nodes. [Bidirectional Typing](https://arxiv.org/pdf/1908.05839.pdf) has an in-depth discussion of the tradeoffs and how to decide which approach to take.

### check
That covers all of our inference cases, completing our bottom-up traversal. Next let's talk about its sibling `check`. Unlike `infer`, `check` does not cover every AST case explicitly. Because we are checking our AST against a known type, we only match on cases we know will check and rely on a catch-all bucket case to handle everything else. We're still working case by case though, so at a high level our check looks very similar to `infer`:
```rs
fn check(
  &mut self, 
  ast: Ast<Var>, 
  ty: Type
) -> GenOut {
  match (ast, ty) {
    // ...
  }
}
```
Notice we match on both our AST and type at once, so we can select just the cases we care about. Let's look at our cases:
```rs
(Ast::Int(id, i), Type::Int) => 
  GenOut::new(
    vec![], 
    Ast::Int(id, i)
  ),
```
An integer literal trivially checks against the integer type. This case might appear superfluous; couldn't we just let it be caught by the bucket case? Of course, we could, but this explicit case allows us to avoid type variables and avoid constraints. Our other explicit check case is for `Fun`:
```rs
(Ast::Fun(id, arg, body), Type::Fun(arg_ty, ret_ty)) => {
  let env = env.update(arg, *arg_ty.clone());
  let body_out = self.check(env, *body, *ret_ty);
  GenOut {
    typed_ast: Ast::fun(id, TypeVar(arg, *arg_ty), body_out.typed_ast),
    ..body_out
  }
}
```
Our `Fun` case is also straightforward. We decompose our `Type::Fun` into it's argument and return type. Record our `arg` has `arg_ty` in our `env`, and then check that `body` has `ret_ty` in our updated `env`. It almost mirrors our `infer`'s `Fun` case, but instead of bubbling a type up, we're pushing a type down. Those are our only two explicit check cases. Everything else is handled by our bucket case:
```rs
(ast, expected_ty) => {
  let id = ast.id();
  let (mut out, actual_ty) = self.infer(ast);
  out.constraints
    .push(Constraint::TypeEqual(id, expected_ty, actual_ty));
  out
}
```
Finally, we have our bucket case. At first this might seem a little too easy. If we encounter an unknown pair, we just infer a type for our AST and add a constraint saying that type has to be equal to the type we're checking against. If we think about this, it makes some sense though. In the unlikely case that neither of our types are variables (`(Int, Fun)` or `(Fun, Int)`), we will produce a type error when we try to solve our constraint. In the case that one of our types is a variable, we've now recorded the contextual info necessary about that variable by adding a constraint. We can rely on constraint solving to propagate that info to wherever it's needed. 

This is the only place where we emit a constraint explicitly. Everywhere else we just propagate constraints from our children's recursive calls. The point where we switch from checking back to inference is the only point where we require a constraint to ensure our type line up. Our intuition for `infer` and `check` help guide us to that conclusion. This is in part the insight and the power of a bidirectional type system. It will only become more valuable as we extend our type system to handle more complicated types.

## Example
It's hard to see how our two functions fit together from just from their implementations. Let's walk through an example to see `infer` and `check` in action. Consider a contrived AST:
```rs
Ast::app(
  Ast::fun(
    Var(0),
    Ast::Var(Var(0))),
  Ast::Int(3)
)
```
This is the identity function applied to an integer. A simple example, but it uses all of our AST nodes and will give us some insight into how `check` lets us propagate more type information than `infer` alone. Our example will use some notation to let us introspect our environment and use human friendly names:
```
x, y, z represent variables

α, β, γ represent type variables

env will use a literal 
formatted as { <var0>: <type0>, <var1>: <type1>, ... } 
with {} being an empty environment.
```
Using our new notation we can shorten our AST example:
```rs
Ast::app(
  NodeId(0),
  Ast:fun(NodeId(1), x, Ast::Var(NodeId(2), x)),
  Ast::Int(NodeId(3), 3)
)
```
Okay, we start by calling `infer` on our root `App` node:
```rs
infer({}, Ast::App(...))
```
Our `App` case starts by inferring a type for our `arg`. Because our argument is `Ast::Int(3)` its inferred type is `Int`:
```rs
infer({}, Ast::Int(..., 3)) = Int
```
We use this inferred argument, and a fresh return type, to construct a function type that we check against our `App`'s `func`:
```rs
check(
  {}, 
  Ast::Fun(..., x, Ast::Var(..., x)), 
  Fun(Int, Var(α))
)
```
A function and a function type is one of our specific check cases (it doesn't fall into the bucket case). We destructure the function type to determine the type of our argument and body. This is where `check` shines. If we just had `infer` we would have to introduce a new type variable for `x` and add a constraint that `x`'s type variable must be `Int`. Instead, we can immediately determine `x`'s type must be `Int`. With our env updated to include `x` has type `Int`, we check body against our function type's return type:
```rs
check(
  { x: Int }, 
  Var(..., x), 
  Var(α)
)
```
This is not a check case we know how to handle, it falls into the bucket case. The bucket case infers a type for our body. This looks up `x`'s type in the environment and returns it:
```rs
infer({ x: Int }, Var(..., x)) = Int
```
We don't show it in the example, but this will also return a new AST where `x` is annotated with its type: `TypeVar(x, Int)`. We'll see how that gets used when we look at the final output of our example. A constraint is added that our checked type is equal to our inferred type:
```rs
Constraint::TypeEqual(
  NodeId(2),
  Var(α), 
  Int
)
```
Once we output that constraint we're done calling `infer` and `check`. We propagate our constraints up the call stack and construct our typed AST as we go. At the end of returning from all our recursive calls we have our constraint set, with just one constraint:
```rs
vec![Constraint::TypeEqual(..., Var(α), Int)]
```
and our typed AST:
```rs
Ast::app(
  ...,
  Ast::fun(
    ...,
    TypeVar(x, Int), 
    Ast::Var(..., TypedVar(x, Int))
  ),
  Ast::Int(..., 3)
)
```
The final overall type of our AST, returned from our first `infer` call, is `Var(α)`, remember our function's type is `Fun(Int, Var(α))`. This illustrates why we need the final substitution step after constraint solving. Only once we've solved our constraints do we know `α = Int`, and we can correctly determine our overall AST's type is `Int`.

With that we've finished generating our constraints. 
As output of constraint generation we produce three things: a set of constraints, a typed AST, and a Type for our whole AST. 
Our typed AST has a type associated to every variable (and from that we can recover the type of every node). 
However, a lot of these are still unknown type variables. 
We'll save that AST for now and revisit it once we've solved our set of constraints and have a solution for all our type variables. 
Naturally then, [next time](/posts/unification) we'll implement constraint solving.
Full source code can be found in the [companion repo](https://github.com/thunderseethe/making-a-language/tree/main/types/base)
