+++
title = "Part 4: TypeChecking Top Level Functions"
date = "2024-07-05T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Type Inference"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Type Inference", "Bidirectional Typechecking", "Unification", "Constraint Solving", "Skolemization", "Rigid Type Variables", "Type Checking", "Top Level Items", "Top Level Definitions"]
description = "Adding support for annotated top level items to our type checker"
+++


[Last season](/posts/row-types), boy how time flies, we added support for algebraic datatypes using row types.
With **both** product and sum types, our type inference must be near completion.
It's a little lacking in scope though.
We don't have a way to call top-level functions.

Worse still, we don't have top level functions at all.
A top level function (we'll call them items for brevity from here on out) is a name associated with an instance of our `AST`.
The `AST` acts as the item's body, and the name is how other `AST`s can refer to that item.
Almost every programming language has some concept of defining a top level function (but not ours!).
`fn` in Rust, `function` in Javascript, `def` in Python, and the list goes on.

Taking stock of our type inference, it's not clear where we would add items.
`type_infer` takes an `AST`, and if we look at all our `AST` nodes, none of them define or refer to a top level function.
This might stoke some objection from the crowd.
We have variables, this is a functional programming language, just put top level functions in variables.
Define the items and call them the same way we do local functions in variables, simple.
This works; we can structure our programs as binding all top level functions as variables of some expression. 
Usually, calling a `main` function:

```hs
let 
  -- one top level item
  f = ...
  -- another top level item
  g = ...
  -- main is also a top level item
  main = f (g 3)
in main
```

However, we won't be organizing our programs that way.

Take a knee gang.
Much like your little league soccer coach after you lose the finals, we're about to get ideological.
Top level functions _could_ be variables, but should they?
When thinking about compiler architecture, a lot of concerns revolve around how work can be split up.
Anytime work can be split into chunks that's an opportunity to cache the work and to parallelize it.
Items provide natural breaking points in our code to split up work.

For example, our compiler might split items up into groups and compile each group in parallel.
A compiler could store the type of an item between compilations, and if the item body hasn't changed the next time we compile it, just reuse the stored type.
If we combine local variables and items, we lose the ability to split up work efficiently.

The distinction provided by items has another use in our language.
Local variables always have a type, but items have a type scheme.
We could allow local variables to have a type scheme.
This feature is called let generalization.
We won't be doing that, for reasons that are well detailed  in [Let Should not be Generalised](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf).
To summarize, let generalization does not come up often in practice, and we can simplify things considerably if we only generalise items.

Ideology isn't the only principle driving our decision.
On the pragmatic side, we want to compile local variables and items differently.
Local variables turn into accesses to a register or a stack slot.
Items, on the other hand, are calls to another section of code.

Another decision falls out of our goal of dividing work up along item boundaries.
Item definitions are checked instead of inferred.
This means all our item definitions have to have a user-supplied type annotation.
We could infer the types of item definitions, but our abstinence affords us some important properties.

Type inference of top level definitions causes [action at a distance](https://en.wikipedia.org/wiki/Action_at_a_distance_(computer_programming)).
If we change the body of an item, causing it to have a new inferred type, that change can cause a seemingly unrelated type error in a totally separate part of my codebase.
The error isn't even guaranteed to be on a call to my changed item.
Because of this downside, languages that support top level type inference, such as Haskell, recommend annotating items.
We avoid this [action at a distance](https://en.wikipedia.org/wiki/Action_at_a_distance_(computer_programming)) entirely by requiring the annotation.

This upside is large enough to warrant annotations on its own.
But the benefits don't stop there.
From a readability perspective, annotations on all top level definitions is great documentation.
It also helps on the implementation side in our type checker.
If all items have annotations, whenever we encounter an item call we know what type it has immediately.
This means all we have to do in our type checker is look it up and proceed.

This means we can check any two items in parallel.
Because all items have their type readily available, there's never a data dependency between them.
In a world where we allow inferring item's types, whenever we encounter an item call we'd have to go off and figure out its type.
This could be arbitrary work; the item we're calling might call another item and so on (don't even get me started on recursive items).
We can solve this by inferring the type of any items we call ahead of time and caching their type.
Problem solved, but we've introduced a new problem: how do we know what items are called to cache their type?
Now we have to do a pass over our AST to collect all the items called before we do another pass to actually type check the AST.

Don't get me wrong, none of these are insurmountable.
We have clear evidence in Haskell and OCaml these issues can be surmounted.
But as we add support for items in this new language, it's worth taking a step back and evaluating the tradeoffs.
Personally, top level inference isn't worth the effort over requiring signatures on items.

Scrape the dirt off your knee, that's enough ideology for one post.
Let's get started on the actual work.
The things we'll need to support items are:

  * A new entry point `type_check` to check our item definitions
  * `ItemId` a new identifier for our items (since they aren't local variables)
  * A new AST variant `Item` that references an item so we can call it
  * Update our inference engine to handle `Item`

Oh hey, we don't need to touch our constraints or unification logic to support items.

# New entry point `type_check` 

Our item definitions will be checked against a user provided annotation.
This annotation gives us a type scheme, and we just have to check our item's body actually matches that type scheme.
Given we already have a `check` function for our bidirectional type system, that sounds easy.
Let's take a pass at a trivial `type_check`:

```rs
fn type_check(
  ast: Ast<Var>, 
  signature: TypeScheme
) -> Result<Ast<TypedVar>, TypeError> {
  let mut ctx = TypeInference::default();

  // We start with `check` instead of `infer`.
  let mut out = ctx.check(
    im::HashMap::default(), 
    ast, 
    signature.ty);

  // Add any evidence in our type annotation to be used during solving.
  out
    .constraints
    .extend(signature
      .evidence.iter()
      .map(|ev| match ev {
        Evidence::RowEquation { left, right, goal } => 
          Constraint::RowCombine(RowCombination {
            left: left.clone(),
            right: right.clone(),
            goal: goal.clone(),
          }),
      }));

  ctx.unification(out.constraints)?;

  // We still need to substitute, but only our ast.
  let subst_ast = ctx.substitute_ast(out.typed_ast);

  // And we're done
  Ok(subst_ast.value)
}
```

We unpack our type scheme into its type and evidence.
Our `AST` is checked against the type to generate a set of constraints.
We add any evidence from our type scheme to the constraints, so they can be used during solving.
Checking doesn't have to worry about producing a type, but we still need to solve constraints to generate a substitution.
Even without a final type, our typed `AST` still need to be substituted.
Let's try this out on a simple example to get a feel for checking items.

Given an item body that takes a function and applies it to an argument:
```rs
let f = Var(0);
let x = Var(1);
let body = Ast::fun(f, Ast::fun(x, Ast::app(Ast::Var(f), Ast::Var(x))));
```
We'll check it against signature:
```rs
let a = TypeVar(0);
let signature = TypeScheme {
  unbound_tys: set![a],
  unbound_rows: set![],
  evidence: vec![],
  ty: Type::fun(
    Type::fun(Type::Var(a), Type::Var(a)),
    Type::fun(Type::Var(a), Type::Var(a))
  )
};
```
We can tell it worked because we get back the typed `AST` we expect and not a type error:
```rs
assert_eq!(
  type_check(ast, signature),
  Ok(Ast::fun(
    TypedVar(f, Type::fun(Type::Var(a), Type::Var(a))), 
    Ast::fun(
      TypedVar(x, Type::Var(a)),
      //...we don't care about this part
    )))
);
```
Bam! Just like that, we're checking items. 
Since we're good engineers, let's modify our signature to be wrong and ensure we get a type error.
We'll reuse our item body but give it an incorrect signature:
```rs
let a = TypeVar(0);
let signature = TypeScheme {
  unbound_tys: set![a],
  unbound_rows: set![],
  evidence: vec![],
  ty: Type::fun(
    Type::fun(Type::Var(a), Type::Var(a)),
    Type::fun(Type::Int, Type::Int)
  )
};
```
Now our function `f` has type `a -> a` but our argument `x` has type `Int`.
This should fail with a type error complaining `Int` isn't type `a`:
```rs
assert_eq!(
  type_check(ast, signature),
  Ok(Ast::fun(
    TypedVar(f, Type::fun(Type::Int, Type::Int)), 
    Ast::fun(
      TypedVar(x, Type::Int),
      _..._
    )))
);
```
Uhhhh... huh. That's not good.
Our type checker has worked a little too well here.
It's erroneously "solved" our type variable `a` to type `Int`.
Normally during inference that's exactly what we want.
In this situation however, we've been told explicitly by our annotation that our function **has** to be `a -> a`. 
We're not allowed to solve it.

## An important distinction in our type variables

Before adding `type_check`, we only supported type inference.
The only way type inference could introduce a type (or row) variable is by creating a fresh one during `infer` or `check`.
Now with support for checking, we have a new way to introduce a type (or row) variable.
A user can provide a variable as part of an annotation.

These variables look the same as variables introduced during constraint generation, but they behave more like base types like `Int`.
They can't be solved, and they only equal themselves.
Checking type annotations has alerted us to a distinction in our variables we've been able to ignore till now.
Fortunately, we are not the first to encounter this issue.
What we're running up against is the difference between unification variables and "type" variables.

Unification variables are what we've used up till now.
We create them during constraint generation, solve them during unificaiton, and (until now) generalise them at the end.
We love unification variables.
Their new compatriot, type/row variables, are only introduced by annotations.
Because they've been provided by an annotation, we can't solve them.
This insolvability of our new variables leads to the two variables often being called rigid and flexible.
Unification variables are flexible because they can be solved. 
Type variables are rigid because they cannot be solved.

## Bifurcating our variables and cleaning up the mess

With our two new variables come the penance for our sins, we have a lot of refactoring to do.
A lot of this is rote, so I won't cover the details. You can find the [gory details in the repo](https://github.com/thunderseethe/making-a-language/tree/main/types/items).
Our laundry list of refactors:

1. First we rename our old `TypeVar` to `TypeUniVar`.
1. Rename our `Var` case to `Unifier`
1. Do this for `RowVar` (renamed `RowUniVar`) and `Row::Open` (renamed `Row::Unifier`)

With these 3 done our type checker is exactly the same, but now we call everything unification variables instead of type variables.
It might have only been 3 bullet points to you, but that was a couple of late nights for me.
I need a breather.
While I have you here, let's talk about why we're renaming things.

Instead of doing all this renaming, we could leave our old `TypeVar` as is and just introduce our new variable kind with a new name.
The goal behind our renaming is to make unification variables purely an implementation detail of the type checker.
All the unification variables we'll need are introduced during constraint generation and removed during substitution.
Long term this is nice for the entirety of our compiler.

Unification variables are common place within the type checker but never exist outside it.
Previously, the rest of our compiler and our type checker dealt in the sole variable kind.
Now that there's two, it's worth considering how they will be used.
The rest of our compiler deals in row variables and type variables.
Our other compiler phases have no concern for unification variables. 
It's none of their business how types get solved.
Unification variables are an implementation detail of type checking, and so we rename them to reflect that purpose.

## Adding rigid variables

We continue by introducing the new kind of variable.
First, create our new variable kinds named `TypeVar` and `RowVar`:

```rs
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub struct TypeVar(pub u32);

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub struct RowVar(pub u32);
```

Next, add "new" variants named `Type::Var` and `Row::Open`.

```rs
enum Type {
  /// A rigid type variable
  Var(TypeVar),
  // ...the rest of our cases.
}

enum Row {
  /// A rigid row variable
  Open(RowVar),
  // ... the rest of our cases.
}
```

Finally, we modify unification to handle our new variants.
Because these variants can't be solved, they behave similarly to `Type::Int`. 
They are only equal with themselves:

```rs
fn unify_ty_ty(...) -> Result<(), TypeError> {
  // ...normalize and match on our types
    // Our new match case for the rigid variables.
    (Type::Var(a), Type::Var(b)) if a == b => Ok(()),
  // ...the rest of our unify function is unchanged.
}
```

We do the same for rows in `unify_row_row`:

```rs
fn unify_row_row(...) -> Result<(), TypeError> {
  // ..normalize and match on our rows
    // Our new match case for the rigid variables.
    (Row::Open(left), Row::Open(right)) if left == right => Ok(()),
    // ...The rest of our unification cases
    // Update our RowsNotEqual case to include the new possibility 
    // that two `Row::Open`'s aren't equal.
    (left, right) => Err(TypeError::RowsNotEqual((left, right))),
  }
}
```

Previously, `RowsNotEqual` could only occur for `ClosedRow`s.
Now that we separate unification variables and row variables, two row variables could be not equal.
Easy fix, we update `RowsNotEqual` to allow for arbitrary `Row`s instead of `ClosedRow`s:

```rs
enum TypeError {
  // This used to be (ClosedRow, ClosedRow).
  RowsNotEqual((Row, Row)),
  // ...
}
```

With that our `Type` now has two variants for our two new variables: `Type::Unifier` and `Type::Var`.
We're almost done refactoring. 
We just have one more area to fix up: substitution.
At the end of our type inference, substitution solves any leftover unification variables and converts our `Type` to a `TypeScheme`.

Before our rigid/flexible variable distinction, substituting variables was obvious.
If they were in our substitution, replace them with their corresponding type.
Otherwise, leave them as is.
It's still mostly that, but we want to change how we handle unsolved unification variables. 
Instead of leaving them as is, we're going to replace them with fresh type variables (or row variables).
This is how we'll ensure unification variables don't escape the type checker.

Making this change is relatively straightforward.
We add two new members to our `TypeInference` struct: 

```rs
struct TypeInference {
  //... stuff we've seen before 
  subst_unifiers_to_tyvars: HashMap<TypeUniVar, TypeVar>,
  next_tyvar: u32,
  subst_unifiers_to_rowvars: HashMap<RowUniVar, RowVar>,
  next_rowvar: u32,
}
```

We'll add some helper methods to convert unification variables into their corresponding variable:

```rs
impl TypeInference {
  fn tyvar_for_unifier(&mut self, var: TypeUniVar) -> TypeVar {
    *self.subst_unifiers_to_tyvars.entry(var)
      .or_insert_with(|| {
        let next = self.next_tyvar;
        self.next_tyvar += 1;
        TypeVar(next)
      })
  }

  fn rowvar_for_unifier(&mut self, var: RowUniVar) -> RowVar {
    *self.subst_unifiers_to_rowvars.entry(var)
      .or_insert_with(|| {
        let next = self.next_rowvar;
        self.next_rowvar += 1;
        RowVar(next)
      })
  }
}
```

These helpers check if we've already converted a unification variable and return the generated variable if we have. 
If we haven't, they generate a new variable using `next_tyvar`/`next_rowvar` and save that as the variable for this unification variable.
With these in hand, modifying our `substitute_*` suite of methods is straightforward.
We'll take a look at how we update `substitute_ty`, and we can extrapolate from that what we do for the others:

```rs
fn substitute_ty(&mut self, ty: Type) -> SubstOut<Type> {
  match ty {
    Type::Var(v) => SubstOut::new(Type::Var(v)),
    Type::Unifier(v) => {
      let root = self.unification_table.find(v);
      match self.unification_table.probe_value(root) {
        Some(ty) => self.substitute_ty(ty),
        None => {
          let tyvar = self.tyvar_for_unifier(root);
          SubstOut::new(Type::Var(tyvar)).with_unbound_ty(tyvar)
        }
      }
    }
    // the rest of our cases stay the same...
  }
}
```

It looks mostly the same.
Except when we have an unsolved unifier we call `tyvar_for_unifier` to convert it and then return a `Type::Var` instead of a `Type::Unifier`.

# Item type inference

Phew, that's enough refactoring.
That'll show me for having a fundamental misunderstandings about type checking.
With that out of the way we can finally add some new functionality.

Items are fundamentally different from our existing variables.
Variables explicitly represent local variables.
They must be bound by a `fun` node somewhere in our `AST`.
Whereas items must not be local, they cannot be bound by a `fun` node in our `AST`.
Accordingly, we'll give items their own identifier:

```rs
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub struct ItemId(pub usize);
```

`ItemId` looks exactly the same as `Var`, but because it's a different type we can't get them confused with each other.
Since `ItemId` is a new type, it will need a new AST variant `Item`:

```rs
#[derive(Debug, PartialEq, Eq)]
pub enum Ast<V> {
  // our previous cases... 
  // A reference to a top level definition
  Item(Option<ItemWrapper>, ItemId),
}
```

Alongside the `ItemId` itself we store an optional `ItemWrapper`:

```rs
struct ItemWrapper {
  types: Vec<Type>,
  rows: Vec<Row>,
  evidence: Vec<Evidence>
}
```

Our wrapper will start out `None`.
We'll see how it's used during type checking.
We introduce a new operation, instantiation, and our wrapper will save all the types we produce from instantiating an item.
At the end of type checking we'll substitute `ItemWrapper`s (like the rest of our `Ast`) solving their variables to types and rows.

A natural question arises from here. 
If items aren't bound by `fun` nodes, how do we know what items are available, and (more importantly) how do we know their types?

## Sourcing our items

Now these are important questions, and were we writing a parser and a name resolver we'd be on the hook to answer them.
Lucky for us though we're writing a type checker, so we can play hooky.
By the time we're at the type checker, name resolution would've determined the in scope items for us.
On top of that, we know their types because all our items are annotated.
We'll capture this faux knowledge in an `ItemSource` struct:

```rs
#[derive(Default)]
struct ItemSource {  
  types: HashMap<ItemId, TypeScheme>,
}
```

`ItemSource` has an entry for each `ItemId`.
Each entry has the annotated type of its `ItemId`.
`TypeInference` will store an instance of `ItemSource`:

```rs
struct TypeInference {
  // ...stuff we've seen before
  // ...the var converter fields we just added
  item_source: ItemSource,
}
```

This `ItemSource` will have an entry for each in-scope `ItemId`.
We don't have to worry about encountering an `ItemId` that isn't in-scope. 
That's handled by name resolution.
We'll add new `type_infer` and `type_check` methods that take an `ItemSource` as input.
Ostensibly this `ItemSource` will be produced by name resolution and passed to our type checker, but that's none of our concern:

```rs
fn type_infer_with_items(
  item_source: ItemSource,
  ast: Ast<Var>,
) -> Result<(Ast<TypedVar>, TypeScheme), TypeError> {
    let mut ctx = TypeInference {
        item_source,
        //... normal defaults
    };

    // ... everything else stays the same
}

fn type_check_with_items(
  item_source: ItemSource,
  ast: Ast<Var>,
  signature: TypeScheme,
) -> Result<Ast<TypedVar>, TypeError> {
    let mut ctx = TypeInference {
        item_source,
        //... normal defaults
    };

    // ... everything else stays the same
}
```

## Inferring `Item` calls

With the type schemes for all our items available, we can delve into inferring a type for our new `Item` node:

```rs
fn infer(&mut self, env: im::HashMap<Var, Type>, ast: Ast<Var>) -> (InferOut, Type) {
  match ast {
    // ...
    Item(item_id) => {
      let ty_scheme = self.item_source.type_of_item(item_id);

      // Create fresh unifiers for each type and row variable in our type scheme.
      let mut wrapper_tyvars = vec![];
      let tyvar_to_unifiers = ty_scheme
        .unbound_tys
        .iter()
        .map(|ty_var| {
          let unifier = self.fresh_ty_var();
          wrapper_tyvars.push(Type::Unifier(unifier));
          (*ty_var, unifier)
        })
        .collect::<HashMap<_, _>>();
      let mut wrapper_rowvars = vec![];
      let rowvar_to_unifiers = ty_scheme
        .unbound_rows
        .iter()
        .map(|row_var| {
          let unifier = self.fresh_row_var();
          wrapper_rowvars.push(Row::Unifier(unifier));
          (*row_var, unifier)
        })
        .collect::<HashMap<_, _>>();

      // Instantiate our scheme mapping it's variables to the fresh unifiers we just generated.
      // After this we'll have a list of constraints and a type that only reference the fresh
      // unfiers.
      let (constraints, ty) =
        Instantiate::new(&tyvar_to_unifiers, &rowvar_to_unifiers).type_scheme(ty_scheme);
      let wrapper = ItemWrapper {
        types: wrapper_tyvars,
        rows: wrapper_rowvars,
        evidence: constraints
          .clone()
          .into_iter()
          .filter_map(|c| match c {
            Constraint::RowCombine(row_combo) => Some(Evidence::RowEquation {
              left: row_combo.left,
              right: row_combo.right,
              goal: row_combo.goal,
            }),
            _ => None,
          })
          .collect(),
      };
      (
        InferOut::new(
          constraints, 
          Ast::Item(
            Some(wrapper), 
            item_id)), 
        ty
      )
    }
    // ...
  }
}
```

Let's walk through this code one section at a time.
First off, what is `type_of_item(item_id)`?

```rs
impl ItemSource {
  fn type_of_item(&self, item_id: ItemId) -> TypeScheme {
    self.types[&item_id].clone()
  }
}
```

Huh... I don't know what I was expecting.
All the same nothing to write home about, what's going on with all this `tyvar_to_unifiers` and `rowvar_to_unifiers` business?

These are part of instantiation.
Instantiation is the compliment operation to generalization.
When we substitute a type at the end of inference, we track all the free variables in that type and wrap them up in a type scheme.
As of this article, we also convert unification variables into type variables en route.

Instantiation does the opposite: 

* Start with a type schemes
* Create fresh unifiers for all of its bound variable
* Replace each instance of a variable in our evidence and type by its fresh unifier
* Turn all evidence into its corresponding constraints

Once we do all of that, we have a list of constraints and a type for our item using all fresh unifiers.
Unlike variables, where each instance of a variable has the same type, each instance of an item has a unique type.
This is by design. 
Each reference to an item is unique and should have a unique type (unlike variables which are implicitly referencing the same thing).
So even if we didn't introduce this unification variable/type variable split, we'd still have to instantiate our item's type schemes so each referenced used fresh unification variables.

We'll peek at instantiation, but we won't cover the details because they are rote (similar to generalization).
If you're interested, you can find them [in the repo](https://github.com/thunderseethe/making-a-language/tree/main/types/items).

```rs
struct Instantiate<'a> {
  tyvar_to_unifiers: &'a HashMap<TypeVar, TypeUniVar>,
  rowvar_to_unifiers: &'a HashMap<RowVar, RowUniVar>,
}
impl<'a> Instantiate<'a> {
  fn type_scheme(&self, ty_scheme: TypeScheme) -> (Vec<Constraint>, Type) {
    let constraints = ty_scheme
      .evidence
      .into_iter()
      .map(|ev| self.evidence(ev))
      .collect();
    let ty = self.ty(ty_scheme.ty);
    (constraints, ty)
  }

  fn evidence(&self, ev: Evidence) -> Constraint { ... }

  fn row(&self, row: Row) -> Row { ... }

  fn ty(&self, ty: Type) -> Type { ... }
}
```

`type_scheme` is our top level entry point.
It's basically a big fold over our type scheme. 
We have a function for each kind of thing we encounter (`Evidence`, `Row`, and `Type`) that takes in an instance and returns a new instance but with all the variables replaced by unification variables.
Once we've folded everything we return the list of constraints and type we created.

After we instantiate our type scheme into a type, we're done. 
We return the instantiated constraints and types as the `InferOut` for our item.
That polishes off everything we needed to support items.
We added support for checking against user annotations, dealt with the consequences of that, added support for in scope items to `TypeInference`, and finally inferred calls to items with our new `Item`.
A modest extension to our type inference engine but an important one.
