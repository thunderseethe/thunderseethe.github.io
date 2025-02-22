+++
title = "Part 7: Lowering Top Level Items"
date = "2025-02-10T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Lowering"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Lowering", "Elaboration", "IR", "System F", "DeBruijn Index"]
description = "Lowering top level items into our IR. A more elaborate affair than anticipated."
draft = true
+++


* Intro

* Single new node `Item`.
  * Intro `ItemId`.
  * Explain why we don't reuse `ast::ItemId`.

* Changes to `lower`.
  * New `lower_with_items`.
    * Draw parallel to `type_check_with_items`.
  * `ItemSource` is back but now lower.
    * Intro code to lower `ast::ItemSource` into `ItemSource`.

TODO: Mention how to handle recursion somewhere?

* `LowerAst` now holds our `ItemSource`.

* Directly into `lower_ast`.
  * New case `Item`:
    * Split this up and talk about it.
```rs
Ast::Item(wrapper, item_id) => {
  let ty = self.item_source.lookup_item(item_id);
  let item_ir = IR::Item(ty, self.item_supply.supply_for(item_id));
  let wrapper = wrapper.expect("ICE: Item lacks expected wrapper");
  let ty_ir = wrapper.types.into_iter().fold(item_ir, |ir, ty| {
    IR::ty_app(ir, TyApp::Ty(self.types.lower_ty(ty)))
  });
  let row_ir = wrapper.rows.into_iter().fold(ty_ir, |ir, row| {
    IR::ty_app(ir, TyApp::Row(self.types.lower_row_ty(row)))
  });
  wrapper.evidence.into_iter().fold(row_ir, |ir, ev| {
    let param = self.lookup_ev(ev);
    IR::app(ir, IR::Var(param))
  })
}
```
  * Reveal ruse about `ty_app`.
    * We've never actually constructed a `TyApp`.
  
  * As we recall, when we lower a top level item we wrap it in function parameters and type function parameters.
    * Accordingly, when we call an item we have a responsibility to pass the parameters lowering added.
    * The normal parameters that appeared in the `Ast` are already passed.
    * We're off the hook for those.
 
  * `ItemWrapper` knows what parameters to pass.
    * Remember from type checking, `ItemWrapper` saves the work we did instantiating `ast::Item`.
    * Now it has exactly what we need to call `IR::Item`.


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

Okay that's not how I remember `lower` looking before.
We saw this once already when type checking items.
Our `lower` function works on a single item at a time.
But to support calling other items we need some metadata about them outside the single item we're processing.

A full compiler would have parsed those other items from a source file and have them at hand.
That's not us (yet!), but we can fake it by providing the required item metadata in an `ItemSource`.
We used this trick to get the `TypeScheme`s of our items during typechecking.
We'll do it again here to get the `Type`s of our IR items.

For convenience, we provide `lower`, and it passes a default instance for `item_source`.
Unlike type checking, where our `ItemSource` had to be conjured from scratch, lowering can start from an `ast::ItemSource` rather than scratch.
We reuse `ast::ItemSource` in our new `lower_with_items` function.
Outside this change `lower_with_items` is a rename of our previous `lower` function:

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
The highlighted lines are the only ones we need to pay attention to.
I just needed to assure you the implementation hadn't changed out from under us.

Peeking in `lower_item_source` we find:

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
Our new IR `ItemSource` is a collection of those `ItemId`s and `Type`s:

```rs
struct ItemSource {
  items: HashMap<ast::ItemId, Type>,
}
```

Let's take a sidebar to discuss how we've lowered `ItemSource` here.
We've passed an unlowered `ast::ItemSource` into our lower function.
This creates a lot of duplicated work.
`lower_with_items` is called once per item we're lowering.
Each time it lowers the same `ast::ItemSource`.

For expository purposes I've included the lowering, so we can see what it looks like.
But, much like a parser, our hypothetical compiler would have some glue code that handles it.
The code would lower our `ast::ItemSource` in one location and pass it to each call to `lower_with_items`.

That's all we had to see in `lower_with_items`.
Our `item_source` is embedded within `LowerAst`.
Our other new field in `LowerAst` is `item_supply`.

`item_supply` is an instance of `ItemSupply`.
Mirroring `VarSupply`, an `ItemSupply` converts `ast::ItemId` into our new id `ItemId`.
Similar to how don't reuse `ast::Var` in lowering, we don't reuse `ast::ItemId`.
Opting instead to introduce a new IR specific ID:

```rs
struct ItemId(u32);
```

The rationale preserves the similarity.
We'll want to generate items that only exist in the IR.
In preparation for that, we introduce the new `ItemId`.
Our next stop is `lower_ast`:

```rs
fn lower_ast(
  &mut self, 
  ast: Ast<TypedVar>
) -> IR {
  match ast {
    // our lower cases...
  }
}
```

We have one new case to add:

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
You're absolutely right we did.
And if performance was a higher priority, we'd be doing precisely that.
Our priority, however, is simplicity.

Caching a `Type` in `Item` means `type_of` doesn't require a parameter.
Having the `Type` at hand is also helpful for traversing the tree.
It's not always easy to thread extra context through a tree traversal.
This side steps that problem entirely.

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

Gah, `supply_for` got us.
But at least our speed bought us an extra line.

Using the `ty` we looked up we can construct our `IR::Item`.
Construction makes use of `supply_for` to convert our `ast::ItemId` into `ItemId`.
`supply_for` is like `VarSupply::supply_for`, but for items.

Assembling our `IR::Item` is only the first step in our lowering.
Recall that upon lowering an item we wrap it in type functions and functions for our type variables and evidence.
Correspondingly, when we lower an item call we have to add extra parameters for the extra functions.

These parameters didn't exist in the type checker, but we did save enough information from the type checker to figure out what they are.
`Ast::Item` holds a reference to `ItemWrapper`.
This struct remembers how we instantiated our item, and was substituted at the end of type checking.

The information from instantiation is precisely what we need to add our parameters to our item.
Similar to how generalization holds the information to add our type functions to item.
All the information we need is present in `ItemWrapper`, but we have to be careful to apply it in the right order.

We have all the info we need.
Some care is required in applying it.
When we pass our parameters they need to line up in the same order as our functions.
The order we wrap our ir in functions is: 

  * Evidence Parameters
  * Row Variables
  * Type Variables

We'll pass our parameters in the reverse order:

  * Type Variables
  * Row Variables
  * Evidence Parameters

Reversing the order lines up our parameters to their functions.
If we have a term `ir` with type:

```rs
Type::ty_fun(Kind::Type,
  Type::ty_fun(Kind::Row,
    Type::fun(<ev_type>, ...)
```

and we try to apply our parameters in the same order:

```rs
IR::ty_app(
  IR::ty_app(
    IR::app(ir, <ev_term>), 
    Row::Open(...)), 
  Type::Var(...))
```

We quickly see our problem.
We're applying `<ev_term>` to a type function.
Even worse we're type applying a `Type::Var(...)` to a type function.

Armed with our order, we start by applying our types:

```rs
let ty_ir = wrapper.types.into_iter().fold(item_ir, |ir, ty| {
  IR::ty_app(ir, TyApp::Ty(self.types.lower_ty(ty)))
});
```

We iterate over each type and wrap our original `ir` in a `ty_app` node with the type.
Rows are applied the same way:

```rs
let row_ir = wrapper.rows.into_iter().fold(ty_ir, |ir, row| {
  IR::ty_app(ir, TyApp::Row(self.types.lower_row_ty(row)))
});
```

Finally, we apply our evidence parameters as normal applications:

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
This is actually the first time we've constructed a `TyApp` node in lowering.
We've had them from the very start, but we didn't need them till now.
My apologies for being duplicitous.

You have to understand though.
I couldn't just introduce a lone type function with no way to use it.
What's peanut butter without jelly?
Our type function would be naked and alone.

The reason we haven't employed type applications till now lies in the duality between instantiation and generalization.
We only introduce type functions for our type schemes, and we only introduce type schemes when we generalize an item.
Subsequently, we only consume type functions when we instantiate items.

That's all the changes we need to make to lower items into our IR.
A modest but important extension to our IR.
It's not obvious looking at the changes here.
We've introduced a new feature we lacked prior: recursion.

All the items we want to reference live in the `ItemSource`.
But that can include the very item we're lowering.
`Var` doesn't share in this possibility.
`Var`s cannot be recursive, so the only way for us to be recursive (at the moment) is to use an `Item`.

Recursion presents no problems for lowering.
Our items are annotated, so we can populate `ItemSource` without processing any item definitions.
Further down the compiler, however, we'll have to be mindful that items can introduce recursion.
Lest we end up waiting on our compiler passes for quite a while.
