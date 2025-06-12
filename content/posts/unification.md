+++
title = "Tying up Type Inference"
date = "2023-07-01T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Type Inference"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Type Inference", "Bidirectional Typechecking", "Constraint Generation", "Polymorphism"]
description = "Solve Type Constraints via Unification"
+++

{{< accessory title="Making a Language" >}}
This post is part of the [making a language series](/series/making-a-language).
A series that teaches you how to implement a programming language using Rust.

This is the capstone of type inference we built towards with [constraint generation](/posts/bidirectional-constraint-generation).
{{</ accessory >}}

## Constraint Solving

[On last week's episode](/posts/bidirectional-constraint-generation) we generated a set of constraints with our bidirectional type system.
Now that we've got our set of constraints, we can attempt to solve them.
So what does it mean to solve a constraint? A constraint is something that has to be true about our types.
If we can prove whatever our constraint wants true, we have solved the constraint.
Since our `Constraint` datatype is just type equality, we can solve all our constraints by proving their types equal.
If we fail to prove two types equal, we have a type error, and we can bail out early.

Okay, that all sounds reasonable.
But begs the question, what does it mean for two types to be equal? For us, we determine two types are equal structurally.
We examine the tree of each type and if they are the same node and all their children are equal, we consider them equal.
This works great except for type variables.
It's not enough to just check that two variables are equal.
A type variable could stand for any type (even another type variable).
When we compare it to another type, we can freely assume the type variable stands for that type and make our two types equal, easy! This works great for any individual equality, but we can get into trouble if we do that haphazardly for our whole set.
We may assume for one equality that type variable `a` is `Type::Int` and for another equality assume it is `Type::Fun(Type::Var(b), Type::Var(c))`.
While a type variable can stand for any type, it can only stand for one type.
If our set of equalities needs a type variable to stand for two types, we have a type error.
To remedy this we need to remember what types we've assigned type variables as we solve.

An assignment of type variables to types is called a type substitution.
We build up a type substitution over the course of constraint solving, and at the end our substitution should have an entry for every type variable.
This process of solving a set of constraints by building up a substitution that makes them equal has a name: it's called [Unification](https://en.wikipedia.org/wiki/Unification_(computer_science)).

### Unification
Unification is the process of determining if a type substitution exists that can make our set of constraints true.
If no such substitution exists, as always, we have a type error.
Unification is fundamentally a search problem since we're trying out various types for our type variables until we find an assignment that works or discover an assignment cannot exist.
We won't, but we could imagine if we tried to bruteforce our solution it would take quite awhile.
Fortunately, once again, math has our back! Unification is a well studied subject and a couple of algorithms have been found that are able to unify types in near linear time.
We'll grab one of these algorithms off the shelf and be on our merry way to quick type inference.

#### Union-find
The secret to speeding up our unification lies in how we store our type substitution.
We'll use a [Union-Find](https://en.wikipedia.org/wiki/Disjoint-set_data_structure) data structure to build our type substitution.
There's a lot that can be said about the union-find, but we're going to skip most of it.
The union-find is only an important implementation detail of our unification algorithm.
Union-Find is an efficient way to store disjoint sets of things, for us, that will be sets of types.
It does this via two operations: union and find.
Each of our disjoint sets is represented by one of its elements.
Union merges two sets combining their representatives to form a new representative for the set.
Find will look up the set a type is a member of and return that set's representative.

It is not immediately apparent how these two operations will let us implement a type substitution.
To see how we implement our type substitution with union-find, let's revisit some code we brushed over in constraint generation: `unification_table` and `fresh_ty_var`:

```rs
struct TypeInference {
  unification_table: InPlaceUnificationTable<TypeVar>,
}

fn fresh_ty_var(&mut self) -> TypeVar {
  self.unification_table.new_key(None)
}
```

`unification_table` makes more sense now that we have some context.
It's our Union-Find data structure.
Whenever we call `fresh_ty_var`, it's creating a new empty set in our union-find and returning its representative.
We won't be creating our own Union-Find, as fun as that'd be.
The rustc team was nice enough to publish their Union-Find as the [ena](https://crates.io/crates/ena) crate.
We'll use that crate as our Union-Find implementation.

When we begin constraint solving, our union-find already has a single set for each type variable with that variable as the representative.
We can think of this as a type substitution where every variable maps to itself.
As we unify things, we'll call union to merge these sets.
Anytime we merge two sets all the type variables in that set now map to its new representative.
If we need to substitute a type variable during unification (spoiler we will), we can do that by calling find to look up its current representative.

#### Back to Unification

Now armed with the knowledge of how to maintain our type substitution in near linear time, let's start looking at how we unify types.
Our unification implementation begins with a function `unification`:

```rs
fn unification(
  &mut self, 
  constraints: Vec<Constraint>
) -> Result<(), TypeError> {
  for constr in constraints {
    match constr {
      Constraint::TypeEqual(left, right) => 
        self.unify_ty_ty(left, right)
            .map_err(|kind| TypeError { kind, node_id })?,
    }
  }
  Ok(())
}
```
Not a lot to see yet.
For each constraint we call `unify_ty_ty` to check the types are equal after unification.
If we encounter an error in any individual constraint, we bail early and don't try to solve any more constraints.

Cool, so what does `unify_ty_ty` look like?

```rs
fn unify_ty_ty(
  &mut self, 
  unnorm_left: Type, 
  unnorm_right: Type
) -> Result<(), TypeErrorKind> {
  let left = self.normalize_ty(unnorm_left);
  let right = self.normalize_ty(unnorm_right);
  match (left, right) {
    // ...
  }
}
```

Oh, it's gonna be a big match statement; neat, we've seen those before.
But what's that bit at the top there about `normalize_ty`? We're building up a type substitution as we unify, assigning types to type variables.
When we solve a type variable to a type, we don't want to re-solve that type variable everytime we encounter it in our constraints.
To avoid re-solving known type variables, whenever we unify a solved type variable, we first want to substitute the variable for its type.
This ensures that our type variable is only assigned one type and is equal to all other types our type variable could be assigned.
So before we unify a type, we first apply all possible substitutions to the type in `normalize_ty`:

```rs
fn normalize_ty(
  &mut self, 
  ty: Type
) -> Type {
  match ty {
    Type::Int => Type::Int,
    Type::Fun(arg, ret) => {
      let arg = self.normalize_ty(*arg);
      let ret = self.normalize_ty(*ret);
      Type::fun(arg, ret)
    }
    Type::Var(v) => match self.unification_table.probe_value(v) {
      Some(ty) => self.normalize_ty(ty),
      None => Type::Var(self.unification_table.find(v)),
    },
  }
}
```
When we normalize, we look up the representative for our type variable in our union-find and return a new type with all our instances of the type variable replaced by its representative. 
Note that when we replace our variable by its type from the union-find, we call `normalize_ty` on that type as well.
We might have solved type variables that are present in this type, so we normalize it to produce the most known type we can.
If our variable hasn't been solved to a type, we call `find` on it to return the root variable in the union-find.
This "solves" a type variable into another type variable when we've unified two variables together.

Great! Now that we've removed all the type variables we can with normalization, we can break down our unification cases:
```rs
(Type::Int, Type::Int) => Ok(()),
```
As is tradition by now, we'll start with our `Int` case. Since any two `Int` types are trivially equal, we can immediately return. Moving onto function types:
```rs
(Type::Fun(a_arg, a_ret), Type::Fun(b_arg, b_ret)) => {
  self.unify_ty_ty(*a_arg, *b_arg)?;
  self.unify_ty_ty(*a_ret, *b_ret)
}
```
Two function types are equal if their argument and return types are equal.  We check that by unifying their argument types and their return types. Up next is unifying type variables:
```rs
(Type::Var(a), Type::Var(b)) => {
  self.unification_table
    .unify_var_var(a, b)
    .map_err(|(l, r)| TypeErrorKind::TypeNotEqual(l, r))
}
```
Here's our first case where we add to our type substitution. Two type variables are equal if we can union their sets in our union-find. This might fail if both our variables already have types as representatives. In that case, we have to check both representative types are equal to each other. If they are not, we fail to union and return a type error. If the types are equal, or only one variable has a representative type, we union our sets, and all the type variables in each set now map to the new representative type. It doesn't matter which type becomes the representative, since they are equal, so we just pick one arbitrarily. Next let's look at unifying a variable with another type:
```rs
(Type::Var(v), ty) | (ty, Type::Var(v)) => {
  ty.occurs_check(v)
    .map_err(|ty| TypeErrorKind::InfiniteType(v, ty))?;
  self
    .unification_table
    .unify_var_value(v, Some(ty))
    .map_err(|(l, r)| TypeErrorKind::TypeNotEqual(l, r))
}
```
This case is where we actually assign a type to a type variable. When a type variable encounters a (non-variable) type, we unify the two making our concrete type the representative of that type variable's set. Moving forward whenever we normalize that type variable, or a type variable in it's set, we return this type.

Before any of that, we call `occurs_check` on our type possibly returning an `InfiniteType` error. `occurs_check` walks our type and makes sure it doesn't have any instance of the type variable we're about to unify it with. The reason we need to avoid this is it forms a cycle in our type substitution. This is a problem during normalization. When normalization substitutes a type, it first normalizes that type.  But that type contains our variable. So we look up our type again and normalize it again, looping infinitely. Infinite loops do not make for fast type inference, so before we can unify our variable and type we want to make sure we're not creating a cycle. Finally, our last case, a type error:
```rs
(left, right) => Err(TypeErrorKind::TypeNotEqual(left, right)),
```
Our final case covers all the combinations where our types don't line up (`(Int, Fun(...))`, etc.). This means our set of constraints has a contradiction. All we can do is return a type error and bail early.

With that we've successfully solved our set of constraints producing a type substitution. That tidies up our constraint solving phase. Last we have to tie everything together to form our type inference system.

## Tying the knot
After generating and solving constraints we have 3 things:

   * An inferred type for our whole AST
   * A typed AST where all our variables are annotated with their types
   * A type substitution to map all our type variables to their types

The final step in our inference is to use our type substitution to solve all of our inferred types from constraint generation.
We do this by walking our AST and normalizing each type we encounter using our type substitution.
The code for this is pretty rote, so we won't cover it in detail, but if you want to see it check out the [full source](https://github.com/thunderseethe/making-a-language/tree/main/types/base).

### Generalization
While we're walking our AST normalizing types, we'll come across a case I haven't touched on yet.
What do we do if our type substitution solves a type variable to itself, or another type variable?
Can such a thing even occur?
It absolutely can.
It turns out it's not even hard to construct such an example:

```rs
let x = Var(0);
let id = Ast::Fun(..., x, Ast::Var(..., x));
```

What type should we infer for our AST here?
We don't have enough contextual information to infer a type for `x`.
After constraint solving, we'll discover `x`'s type is solved to a type variable.

This is actually fine -- intentional even.
When we see this, it means the type for our whole AST is polymorphic and should contain a type variable.
The way we handle this is to generalize all our unbound type variables at the end of type inference.
A type paired with it's unbound type variables is called a [type scheme](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Polytypes).
So the type scheme we'd infer for our example AST `id` is:

```rs
TypeScheme {
  unbound: vec![TypeVar(0)],
  ty: Type::Fun(Type::Var(TypeVar(0)), Type::Var(TypeVar(0)))
}
```
We have one unbound type variable, and our function takes that type variable and returns it.
Anyone that wants to call our function will have to provide us a type for our type variable, and only then will we know the type of our function.

## Conclusion
Now that we've put all our pieces in place we can pretty succinctly glue them together to perform type inference in our coalescing `type_infer` function:

```rs
fn type_infer(ast: Ast<Var>) -> Result<(Ast<TypedVar>, TypeScheme), TypeError> {
  let mut ctx = TypeInference {
    unification_table: InPlaceUnificationTable::default(),
  };

  // Constraint generation
  let (out, ty) = ctx.infer(im::HashMap::default(), ast);

  // Constraint solving
  ctx.unification(out.constraints)?;

  // Apply our substition to our inferred types
  let (mut unbound, ty) = ctx.substitute(ty);
  let (unbound_ast, typed_ast) = ctx.substitute_ast(out.typed_ast);
  unbound.extend(unbound_ast);

  // Return our typed ast and it's type scheme
  Ok((typed_ast, TypeScheme { unbound, ty }))
}
```
Our function even divides cleanly into our phases.
We generate a set of constraints, solve them into a substitution, and apply our substitution.
The full source code is available as a GitHub repo you can check out and play around with [here](https://github.com/thunderseethe/making-a-language/tree/main/types/base).

We've done it!
We can infer types for our simple language now.
There's a ton we can do to make this more performant or infer more powerful types, but all of that can wait for another day.
This article is already woefully long for how much we've had to skim over the details.
We've created a great MVP that we can start playing around with, and in a [future article](/posts/row-types) we can start extending it with the fancy features we'd like our language to support.
Best of all we didn't get stuck endlessly fiddling with our parser.
Type driven design has allowed us to escape the orbital decay of parser bikeshedding.
