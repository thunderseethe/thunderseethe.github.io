+++
title = "Lowering[2].Items: Lowering Top Level Items"
date = "2025-03-04T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Lowering"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Lowering", "Elaboration", "IR", "System F", "Item", "Instantiation", "Generalization", "Recursion"]
description = "Lowering top level items into our IR. A more elaborate affair than anticipated."
+++

[Last time](/posts/lowering-rows-intro) saw us endeavoring upon a trilogy to bring rows crashing down from their high-flying AST nodes into the realities of our lowly IR.
Our goals this time are less highfalutin. 
We're going to be lowering items.
I can't tell you how relieved I am to see this titled ".Items", not ".Items[0]".

Items represent top level functions in our language.
In our [previous work](/types/check-top-level-items/), which type checked items, we learned items differ from local variables in the types they're allowed to have.
Our local variable's types must not introduce any type variables, whereas items are allowed to introduce type variables unrepentantly.
This is because items have type schemes, whereas local variables have to make do with monotypes.

Lowering removes our type scheme/type distinction, but leaves an important difference.
Because items had type schemes, IR item's types will have type functions.
We've seen this at the end of `lower` where we wrap the IR for an item in type functions.
This gives us a new ability local variables lack.
An item can be instantiated at multiple types.

It also burdens us with a new responsibility.
When we call an item we have to figure out the right types to pass for that particular call.
Because items support multiple instantiations, it's not immediately obvious what type an item call should have, unlike local variables.

Thankfully, type checking has done a lot of the heavy lifting for us.
Type checking used unification to infer what types are passed to each of our item calls.
All we have to do is use the work we saved from type checking to apply the types to our lowered item calls.

{{< accessory title="Quick Refresher" >}}

In [types/items](/types/check-top-level-items/), we only needed one new AST node:

```rs
enum Ast<V> {
  // ...our other nodes
  Item(Option<ItemWrapper>, ItemId),
}
```

It introduces two new types: 

 * `ItemId` 
 * `ItemWrapper`.

`ItemId` is unsurprising:

```rs
struct ItemId(usize);
```

It serves a similar role to `ast::Var`.
We can imagine some name resolution pass, we haven't written yet, assigns a unique `ItemId` to each `Item` it encounters.

`ItemWrapper` is more involved:

```rs
struct ItemWrapper {
  types: Vec<Type>,
  rows: Vec<Row>,
  evidence: Vec<Evidence>
}
```

The role of `ItemWrapper` is to save the result of instantiation for each item call.
Our `Item` nodes begin unannotated.
When we instantiate an item, we set its `Option<ItemWrapper>` to `Some` with all the variables and evidence introduced.

Items require new data outside the AST itself.
We also create `ItemSource`.
This is a type that holds the type schemes of all the items that are in scope.
Again we can imagine this is the product of a name resolution pass, we're definitely for sure going to write any day now:

```rs
struct ItemSource {
  types: HashMap<ItemId, TypeScheme>,
}
```

Putting together `ItemSource` is made trivial by our requirement that items be annotated.
We don't have to do any processing to determine our items type schemes.
We can just collect each `ItemId` and `TypeScheme` from our source code and assemble them into `ItemSource`.

{{</ accessory >}}

## Setting off on our journey

We start our journey in our old friend the `lower` function:

```rs
fn lower_with_items(
  item_source: ast::ItemSource,
  ast: Ast<TypedVar>,
  scheme: ast::TypeScheme,
) -> (IR, Type) {
  // ...some code
}
```

Uh...huh. 
That's not how I remember our old friend.

Ah, here we are.
I didn't scroll far enough:

```rs
fn lower(
  ast: Ast<TypedVar>, 
  scheme: ast::TypeScheme
) -> (IR, Type) {
  lower_with_items(
    ast::ItemSource::default(), 
    ast, 
    scheme)
}
```

Okay that's not how I remember `lower` looking either.
This happened once already when type checking items.
Our `lower` function works on a single item at a time.
But to support calling other items we need information outside the single item `lower` processes.

A full compiler would have parsed those other items from a source file and have them at hand.
That's not us (yet!), but we can fake it by providing the required item metadata in an `ItemSource`.
We used this trick to get the `TypeScheme`s of our items during type checking.
We'll do it again here to get the `Type`s of our IR items.

For convenience, we provide `lower`, and it passes a default instance for `item_source`.
Most compilation will have items, but this is helpful for testing.
Unlike type checking, where our `ItemSource` had to be conjured from the void, lowering can start from an `ast::ItemSource`.
`lower_with_items` makes this apparent by taking an `ast::ItemSource` instead of our new IR `ItemSource`.

## Establishing our new `lower` function

We find ourselves in unfamiliar territory with all the renaming and parameters adding going on.
A cursory look into `lower_with_items` convinces us things are more familiar than they appear:

```rs {hl_lines=[27,28]}
fn lower_with_items(
  item_source: ast::ItemSource,
  ast: Ast<TypedVar>,
  scheme: ast::TypeScheme,
) -> (IR, Type) {
  let ev = scheme.evidence.clone();
  let (ir_ty, kinds, lower_ty) = lower_ty_scheme(scheme);

  let mut supply = VarSupply::default();
  let mut ev_to_var: HashMap<ast::Evidence, Var> = HashMap::default();
  let params = ev
    .into_iter()
    .map(|ev| {
      let ty = lower_ty.lower_ev_ty(ev.clone());
      let param = supply.supply();
      let var = Var::new(param, ty);
      ev_to_var.insert(ev, var.clone());
      var
    })
    .collect::<Vec<_>>();

  let mut lower_ast = LowerAst {
    supply,
    types: lower_ty,
    ev_to_var,
    solved: vec![],
    item_source: lower_item_source(item_source),
    item_supply: ItemSupply::default(),
  };

  let ir = lower_ast.lower_ast(ast);
  let solved_ir = lower_ast
    .solved
    .into_iter()
    .fold(ir, |ir, (var, solved)| IR::app(IR::fun(var, ir), solved));
  let param_ir = params
    .into_iter()
    .rfold(solved_ir, |ir, var| IR::fun(var, ir));
  let bound_ir = kinds
    .into_iter()
    .fold(param_ir, |ir, kind| IR::ty_fun(kind, ir));
  (bound_ir, ir_ty)
}
```
The highlighted lines are the only ones requiring our attention.
But I'm comforted to see our reliable `lower` is still found under a new alias.

## Sourcing Our Items

Our first new line introduces a function `lower_item_source`.
Peeking at its definition we find:

```rs
fn lower_item_source(
  items: ast::ItemSource
) -> ItemSource {
  todo!()
}
```

A function responsible for ferrying us from `ast::ItemSource` to `ItemSource`.
`ast::ItemSource` maps `ast::ItemId` to `TypeScheme`s.
Our new `ItemSource` serves a similar role:

```rs
struct ItemSource {
  items: HashMap<ast::ItemId, Type>,
}
```

In lowering, `TypeScheme`s become `Type`s.
So it makes sense that our `ItemSource` maps `ast::ItemId`s to `Type`s.
Accordingly, the body of `lower_item_source` is straightforward:

```rs
fn lower_item_source(
  items: ast::ItemSource
) -> ItemSource {
  ItemSource {
    items: items
      .types
      .into_iter()
      .map(|(item_id, ty_scheme)| {
        let (ir_ty, _, _) = lower_ty_scheme(ty_scheme);
        (item_id, ir_ty)
      })
      .collect(),
  }
}
```

We iterate over the type schemes of our items.
`lower_ty_scheme` turns each scheme into an IR type.
These are collected into our new `ItemSource` completing our implementation.

Let's take a sidebar to discuss how we've lowered `ItemSource` here.
We pass an `ast::ItemSource` into our lower function.
This creates a lot of duplicated work.
`lower_with_items` is called once per item we're lowering.
Each of these calls lowers the same `ast::ItemSource` (we won't worry that items might be in different scopes for now).

For expository purposes, I've included item source lowering, so we can see how it works.
But our hypothetical compiler would have some glue code that handles this.
The code would lower our `ast::ItemSource` in one location and pass it to each call of `lower_with_items`.

## Supplying Our Items

That's all there is to see in `lower_with_items`.
Our `item_source` is embedded as field within `LowerAst`.
We move on to our other new `LowerAst` field: `item_supply`.

`item_supply` is, shockingly, an instance of `ItemSupply`.
Mirroring `VarSupply`, an `ItemSupply` converts `ast::ItemId` into our new IR id `ItemId`.
Similar to how we don't reuse `ast::Var` in lowering, we don't reuse `ast::ItemId`.
Opting instead to introduce a new IR specific ID:

```rs
struct ItemId(u32);
```

The similarities persist into the rationale.
We'll want to generate items that only exist in the IR.
Rather than give those IR-only items an `ast::ItemId`, its easier to break the correspondence now. 
We can remember a mapping from `ast::ItemId` to `ItemId` if we need it.
This makes it simple to tell what items are generated and what items are sourced from an `Ast` (and correspondingly from user written code).

## Lowering Our Items

With the introduction of our two new fields, we can begin actual lowering.
Look no further than `lower_ast` to meet your lowering needs:

```rs
impl LowerAst {
  fn lower_ast(
    &mut self, 
    ast: Ast<TypedVar>
  ) -> IR {
    match ast {
      // our lower cases...
    }
  }
}
```

We have a single new case to add to our `match`:

```rs
Ast::Item(wrapper, item_id) => {
  todo!()
}
```

Before we can lower `Ast::Item`, we need something to lower into.
IR needs a new `Item` case our lowering can target:

```rs
enum IR {
  //...our previous cases
  Item(Type, ItemId),
}
```

It's reasonable to question why `Item` contains a `Type`.
Didn't we just put all our `Item`'s types in `ItemSource`?
You raise a great point, and if performance was a higher priority, we'd be doing precisely that.
Our priority, however, is simplicity.

Caching a `Type` in `Item` means `type_of` doesn't require an `ItemSource` to construct types.
Having the `Type` at hand is also helpful for all sorts of traversals we'll perform on `IR`.
We could of course thread an `ItemSource` through each traversal.
But this is added complexity on every traversal interface we're going to write.
Embedding the type side steps the problem entirely.

Our `IR` qualifies for lowering now, back in `lower_ast`:

```rs
Ast::Item(wrapper, item_id) => { 
  let ty = self.item_source.lookup_item(item_id);
  todo!()
}
```

We're immediately impeded by an in-known implementation.
Off to `lookup_item`:

```rs
impl ItemSource {
  fn lookup_item(
    &self, 
    item: ast::ItemId
  ) -> Type {
    self.items[&item].clone()
  }
}
```

Thankfully, there isn't much to see here.
Given an `ast::ItemId` it returns the item's `Type`.
Type checking ensures any `ItemId` we see here will be present in `self.items`.

Quick, we've got to make some headway before another impeding implementation strikes:

```rs
Ast::Item(wrapper, item_id) => { 
  let ty = self.item_source.lookup_item(item_id);
  let item_ir = IR::Item(ty, self.item_supply.supply_for(item_id));
  let wrapper = wrapper.expect("ICE: Item lacks expected wrapper");
  todo!()
}
```

Gah, `supply_for` got us, but at least our speed bought us an extra line.

We construct our `IR::Item` using the `ty` we just looked up.
Construction makes use of `supply_for` to convert our `ast::ItemId` into `ItemId`.
`supply_for` is like `VarSupply::supply_for`, but for items.
Its implementation is uninteresting, and can be found in the [full code](https://github.com/thunderseethe/making-a-language/tree/main/lowering/items).

## Ordering Our Applications

Assembling our `IR::Item` is only the first step in our lowering.
Recall that upon lowering an item body we wrap it in type functions and functions for our type variables and evidence.
Correspondingly, when we lower an item call we have to apply extra parameters for those extra functions.

These parameters didn't exist in the type checker, but we saved enough information to figure out what they are.
This is why `Ast::Item` holds a reference to `ItemWrapper`.
This struct remembers how we instantiated our item.

The information from instantiation is precisely what we need to add our parameters to our item.
Generalization holds the information to add our type functions to item.
It makes sense that it's compliment, instantiation, would hold the information to apply the added type functions.
`ItemWrapper` holds all the answers, but we have to be careful to apply them in the right order.

When we pass our parameters they need to line up in the same order as our functions.
We wrap our IR using the order: 

  * Evidence Parameters
  * Row Variables
  * Type Variables

We need to pass our parameters in the reverse order:

  * Type Variables
  * Row Variables
  * Evidence Parameters

Reversing the order lines up our parameters to their functions.
A quick example shows us why this is the case.
Consider a term `ir` with type:

```rs
Type::ty_fun(Kind::Type,
  Type::ty_fun(Kind::Row,
    Type::fun(<ev_type>, ...)
```

If we try to apply our parameters in the same order:

```rs
IR::ty_app(
  IR::ty_app(
    IR::app(ir, <ev_term>), 
    Row::Open(...)), 
  Type::Var(...))
```

We quickly see our problem.
We're applying `<ev_term>` to a type function.
Even worse we're type applying a `Type::Var(...)` to a function.

Reversing nicely resolves our issue, so that's the order we'll use for `ItemWrapper`.
Armed with our order, we start by applying our types:

```rs
let ty_ir = wrapper.types.into_iter().fold(item_ir, |ir, ty| {
  IR::ty_app(ir, TyApp::Ty(self.types.lower_ty(ty)))
});
```

We iterate over each type and wrap our original `ir` in a `ty_app` node.
Rows are applied the same way:

```rs
let row_ir = wrapper.rows.into_iter().fold(ty_ir, |ir, row| {
  IR::ty_app(ir, TyApp::Row(self.types.lower_row_ty(row)))
});
```

Finally, we apply our evidence parameters as normal applications because they are values:

```rs
wrapper.evidence.into_iter().fold(row_ir, |ir, ev| {
  let param = self.lookup_ev(ev);
  IR::app(ir, IR::Var(param))
})
```

This completes our item case and will turn `Ast::Item(...)` into:

```rs
IR::app(
  IR::ty_app(
    IR::ty_app(
      IR::Item(...),
      <type>),
    <row>),
  <evidence)
```

We don't have to worry about passing the rest of our parameters to our item.
Those were already represented in the AST as `Ast::App` nodes.
Lowering will take care of them for us.

I have to reveal a bit of a ruse.
This is actually the first time we've constructed a `TyApp` node in lowering our AST.
We've had `TyApp` from the very start, but we didn't need it till now.
My apologies for being deceiving you this long.

You have to understand though.
I couldn't just introduce a lone type function with no way to use it.
What's peanut butter without jelly?
Eggs without bacon?
Beans without toast?
Our type functions would be naked and alone.

The reason we haven't employed type applications till now lies in the duality between instantiation and generalization.
We only introduce type functions for our type schemes, and we only introduce type schemes when we generalize an item.
Subsequently, we only consume type functions when we instantiate items (ignoring evidence terms which have a teensy use of both).

That's all the changes we need to make to lower items into our IR.
A modest but important extension to our IR.
It's not obvious looking at the changes here.
We've introduced a new feature we lacked prior: recursion.

All the items we want to reference live in the `ItemSource`.
But that can include the very item we're lowering.
`Var` doesn't share in this possibility.
`Var`s cannot be recursive, so the only way for us to be recursive (at the moment) is to call an `Item`.

Recursion presents no problems for lowering.
Our items are annotated, so we can populate `ItemSource` without processing any item definitions.
Further down the compiler, however, we must be mindful that items introduce recursive possibilities.
Lest we end up waiting on our compiler passes forever.
