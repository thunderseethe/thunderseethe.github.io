+++
title = "WIP Desugar"
date = "2025-11-12T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Desugar"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Parsing", "Recursive Descent", "LL(1)", "Lexing", "Concrete Syntax Tree", "Error Recovery"]
description = "Desugaring our base CST into our base AST"
+++

Previously, we, begrudgingly, [parsed a syntax into a CST](/posts/parser-base).
With that tarpit deftly dodged, we can proceed to our next task, named desugaring: mapping our CST onto our AST.
Our CST leaves us with a lot of cruft, such as `|` or `=`.
This stuff was important for telling head from tail in our initial source file, and we'll want to have it around when we're reporting diagnostics, but the rest of the compiler doesn't really care about such mundane affairs.
Desugaring helps us strip away all the syntax and focus in on what's important, lightening the cognitive load for following compiler passes.

It's worth asking then, do we really need to?
Can't the later passes just deal with the excess syntax?
We've come to expect that's the tee up for why we can't do that, but actually you kinda just...can.
In fact, that's exactly what Swift does.

They parse the CST, and their "AST" is just the subset of fields on the CST that are semantically relevant.
It's a perfectly valid strategy.
If you're not writing a compiler to explain each pass in isolation, I might even _recommend_ it.
We won't employ that strategy for two reasons:

* I (and I hope you) find it helpful to have an explicit separate AST
* Our CST includes syntax with no parallel in our AST

We allow for let expressions in syntax, but let expressions are nowhere to be found in our AST.
This reasons turns out be a common motivator for splitting CST and AST.
If you look at `rust-analyzer`, they employ a similar strategy to Swift.
[They expose](https://github.com/rust-lang/rust-analyzer/blob/master/crates/syntax/src/ast/generated/nodes.rs) a bunch of helpers on the CST for the semantically interesting stuff.
Despite that, they still produce a separate tree called [HIR](https://github.com/rust-lang/rust-analyzer/blob/master/crates/hir-def/src/hir.rs#L191).

Much like us, Rust desugars away a lot of it's surface syntax.
Accomodating this transformation requires producing a new tree.
It's not enough to provide methods for the semantically interesting stuff.
We need to fundamentally change the structure of our tree.

As we change our tree, we need to remember where we came from.
It's important that we're able to map our AST back onto our CST.
This will matter not only for error reporting where the CST tells us where an error occurs, but also for semantic queries.
If we want to go to definition, we'll need to determine the AST node our cursor is pointing at and then use that to determine where it's definition is.

To that end, desugaring is going to produce our new AST _and_ a mapping from AST nodes to CST nodes.
Desugaring, like parsing, is also going to be resilient.
We produce a list of errors alongside our AST and mapping.

TODO: Explain rowan distinction between syntax node and token
TODO: Explain tag maybe?
TODO: Explain rowan traversal API (maybe after desugar because it introduces it lightly)

* Shared State
  * `Desugar`
    * `node_id` tracks the next available node id for our AST
    * `ast_to_cst` tracks the CST that produced a given AST node.
      * Why is this `ast_to_cst` and not `cst_to_ast`
      * Invariant: A sync node only appears once as a value
    * `errors` tracks desugaring errors
      * We're resilient, so we always produce something
    * `root` the root of our CST
      * Why do we hang onto this?

* `desugar`
  * top level entry point for desugaring
  * construct `Desugar`
  * call `desugar_program`
  * what's `SyntaxNode`
    * a part of `rowan`
    * `GreenNode` provides an immutable view of our CST suitable for storage
    * It is not suitable for traversal.
    * `rowan` provides `SyntaxNode` to traverse our CST.
    * These are the red nodes in the red-green tree.

    * `SyntaxNode` uses mutation internally to speed up traversal
    * Under the hood `SyntaxNode` is a pointer, so it is not convenient to store.
    * This leads us to a common pattern we'll see throughout desugaring.
    * We'll work with `SyntaxNode` directly while we're traversing to produce our AST.

    * When we want to store a `SyntaxNode`, we'll turn it into a `SyntaxNodePtr`.
    * A `SyntaxNodePtr` is the span of our node and it's kind.
    * It acts as an index into our GreenNode.
    * By storing a `GreenNode` and a `SyntaxNodePtr` we can create our `SyntaxNode` as needed but we don't have to worry about the lifetimes around storing a pointer. 

* Once we have a `SyntaxNode`, we pass it to `desugar_program`
* Recall from parsing, our program is just an expression.
* We see that's still the case for `desugar_program`

* TODO: Code snippet

* `first_child_by_kind` is one of our traversal helpers only available on `SyntaxNode`.
* We use it to find the first `Expr` node in our CST.
* Failing to find an expression, we assume our program is invalid and return a `hole` for our program.

* `hole` constructs an `Hole` AST node and maps it to our CST node:

* TODO: `hole` definition

* Recall `Hole` is part of our resilience strategy.
* Rather than failing at the first invalid AST, we treat it as a Hole and try to recover as much of our ASt as possible.
* Hard to see the value in that when our whole program is hole, but I promise it'll be handy later.

* Whenever we create a hole we attach an error to let us know what went awry.
* Down the road we can use these to report diagnostics to the user.

## desugar_error

* Back in `desugar_program`, when we do find our `Expr` we pass it along to `desugar_expr`:
* The first thing we do in `desugar_expr` is check we are looking at an expression.

* TODO: `desugar_expr` expression

* We just checked for a `Expr` node in `desugar_program`.
* Why are we checking again?
* We'll call `desugar_expr` all over the place, and we won't always know it's an expression ahead of time.
* We check to make sure that's the case in `desugar_expr`.
* If it's not, we call `error_hole`.
* `error_hole` is like `hole`, but it does emit an error.
* We don't assume parsing will have emitted an error this time, so we emit an error ourselves.
  * What's the rationale for why this gets an error, but the other does not?
  * When can this arise in practice?

* Empowered by the knowledge we're looking at an expression, we being the process of desugaring let bindings.
* Recall an expression is any number of let bindings followed by an application:

* Expr
  * Let
    * ...
  * Let
    * ...
  * App
    * ...

* In a perfect world, we'd consume this layout and be on our merry way.
* Alas, we have to contend that the expression we're looking at isn't actually valid.
* We maintain a list of bindings and walk the children `SyntaxNode`s of our expr:

* TODO: Walk expr children code snippet.

* `children` only walks over syntax nodes, not tokens, so we don't have to worry about child being a `Whitespace` or other minutia.
* If our child is a `Let`, we try to desugar a let binding and at it to our list of bindings.
* Our let bindings might be an error.
* In which case we treat the entire child as a hole, emit an error, and call `build_locals` using our hole.

* We're ignoring the rest of our children when this happens.
* Once we encounter an erroneous binding, we assume the rest of our expression is invalid as well.
* Otherwise, our error would have been localized within the let binding rather than the entire binding itself.
* There's no point in translating the remainder of our expression as an error.
* It won't provide better analysis in later passes, so we save ourselves the effort.

* If all goes well, eventually we'll hit something that isn't a `Let`.
* In that case, we assume it's the body of our expression and pass it to `desugar_app`.
* Whatever comes out of that, hole or otherwise, we pass to `build_locals`.

* On our happy path, our for loop returns with a call to `build_locals`.
* If we exit our for loop, something has gone wrong.
* Either our expr has no children, or it only had let bindings and nobody.
* We'll try to attach our error to the last node of expr, and fallback to the entire expression if it's empty.

## desugar_let

* `desugar_let`, much like `let_` from our parser, does not desugar a full let expression
* It only handles the binding portion: `let <var> = <expr>;`.
* Because of that we don't produce an `Ast` out of `desugar_let`, we can't.
* Instead, we produce a pair consisting of our identifier, and it's definition, relying on `desugar_expr` to turn those into a full `Ast`.

* We'll extract our pair from the children of our `Let` node:

TODO: `desugar_let` code snippet
TODO: Explain the expected structure of our tree, mayhbe with a syntax diagram?

* We assume our let binding only has two nodes (recall the distinction between nodes and tokens).
* The first is a `LetBinder`, which holds the variable our let binds.
* In parsing we parse an identifier and then wrap it up as a `LetBinder` making it easier to find now.
* We unwrap our `LetBinder` node to reveal it's underlying `Identifier` token and get its text.
* If our binding is missing we error out
  * TODO: Note that we could handle missing identifiers but do not.

* We don't check our next child for a specific syntax kind.
* It's the definition of our let binding, which could be any expression.
* We pass it to `desugar_expr` and use whatever it gives us.
* Failing to find a next child at all, we produce a hole and let the user know they're missing an expression.

## desugar_app

Recall from our parsing an application that we parse one or more atoms as a single "application".
Our `Ast` node for application, however, only supports a function and an argument.
Repeated application is represented by nesting applications within each other.
Fortunately, our CST already encodes the tree structure we need to construct such an AST.
While we parse a flat list of atoms, we construct a nested tree out of them:

* App
  * App
    * ...
    * <atom>
  * <atom>

We get to lean on that in desugaring and don't have to do it ourselves.
Another special case for application is when it contains a single atom.
In this case, we don't want an application at all and want to treat that atom as our overall expression.
We handle that by checking if the root of our application is an `App`:
TODO: Fit in our `desugar_app` signature somewhere

```rs
let Syntax::App = app.kind() else {
  return self.desugar_atom(app);
};
```

If it's not, this is our single atom case.
If it is, we move on to construct an application:

```rs
let fun = match app.first_child() {
  Some(fun) => self.desugar_app(fun),
  None => self.hole(&app, DesugarError::ApplicationMissingFun)
};
```

We assume our first child is another application.
This works because if it's a single atom, such as `f`, `desugar_app` will return it as is.
If it's actually an application this handles our nesting accordingly.
After that we grab our second child:

```rs
let arg = match app.last_child() {
  Some(arg) => self.desugar_atom(arg),
  None => self.hole(&app, DesugarError::ApplicationMissingArg),
};
```

The second child uses `desugar_atom` instead of `desugar_app`.
An application that appears in the argument position must be wrapped in parentheses and will be an atom.
Finally, we construct our actual `Ast` node:

```rs
let id = self.next_id();
self.insert_node(id, SyntaxNodePtr::new(&app));
Ast::app(id, fun, arg)
```

`next_id` is a helper that produces a unique `NodeId` for our new `Ast` node.
We use that ID to map our fresh AST node back to the CST node that inspired it.
Returning our fresh `App` node, we continue on our way.

## desugar_atom

In a lot of ways `desugar_atom` mirrors parsing an atom.
Where parsing looks at the next token, `desugar_atom` looks at the next `Syntax`.
Where parsing has a case for each kind of atom, `desugar_atom` has a case.
Our structure matches that:

```rs
fn desugar_atom(
  &mut self, 
  atom: SyntaxNode<Lang>
) -> Ast<String> {
  match atom.kind() {
    Syntax::Var => ...,
    Syntax::IntegerExpr => ...,
    Syntax::Fun => ...,
    Syntax::ParenthesizedExpr => ...,
    _ => self.hole(&atom, DesugarError::UnexpectedAtom(atom.kind())),
  }
}
```

Where parsing breaks on an unknown token, we return a hole and an error on an unknown atom.
Our first case, `Var`, grabs the first identifier token and constructs an `Ast` node with it:

```rs
let Some(var) = atom.first_child_or_token_by_kind(&|kind| kind == Syntax::Identifier)
else {
  return self.hole(&atom, DesugarError::VarMissingIdentifier);
};
let id = self.next_id();
self.insert_node(id, SyntaxNodePtr::new(&atom));
Ast::Var(id, var.to_string())
```

We see a lot of similarities for our `Int` case:

```rs
let Some(int) = atom.first_child_or_token_by_kind(&|kind| kind == Syntax::Int) else {
  return self.hole(&atom, DesugarError::IntegerExprMissingInt);
};

let id = self.next_id();
let val = match int.to_string().parse() {
  Ok(int) => int,
  Err(err) => return self.hole(&atom, DesugarError::InvalidInt(err)),
};
self.insert_node(id, SyntaxNodePtr::new(&atom));
Ast::Int(id, val)
```

Except, we may fail to parse an integer from our string, returning a hole.
Our `ParenthesizedExpr` case simply unwraps our expr:

```rs
match atom.first_child_by_kind(&|kind| kind == Syntax::Expr) {
  Some(expr) => self.desugar_expr(expr),
  None => self.hole(&atom, DesugarError::MissingSyntax(Syntax::Expr)),
}
```

Finally, our function case defers to our helper `desugar_fun`:

```rs
Syntax::Fun => self.desugar_fun(atom),
```

TODO: Transition

## desugar_fun

Our funciton syntax is `| <identifier> | <expr>`.
During desugaring, we're only interested in the semantically relevant syntax.
For function that's the identifier and expression, much like let bindings.
We start by grabbing our identifier.

```rs
let Some(var) = fun
  .first_child_by_kind(&|kind| kind == Syntax::FunBinder)
  .and_then(|node| node.first_child_or_token_by_kind(&|kind| kind == Syntax::Identifier))
else {
  return self.hole(&fun, DesugarError::MissingSyntax(Syntax::FunBinder));
};
```

Because we wrap our `Identifier` up as a `FunBinder` in our parser, we can traverse directly to it now.
From there we grab the identifier, or our function is invalid and we return a hole.
Next, we get the body of our function:

```rs
let body = match fun.first_child_by_kind(&|kind| kind == Syntax::Expr) {
  Some(expr) => self.desugar_expr(expr),
  None => self.hole(&fun, DesugarError::FunMissingExpr),
};
```

We trust the parser to ensure that the only `Expr` we'll see is our function body.
Unlike our identifier, if we fail to find an expression we still return a function but with a hole for the body.

TODO: Note the possibility of optional identifiers.

Finally, we wrap our two pieces up as a new `Ast` node:

```rs
let id = self.next_id();
self.insert_node(id, SyntaxNodePtr::new(&fun));
Ast::fun(id, var.to_string(), body)
```

TODO: Transition

## build_locals

`build_locals` is, in a sesnse, where all the magic happens.
Our other helpers turn one syntatic construct into one AST construct.
Here we turn our let bindings into their corresponding AST nodes.    
Plural because we're breaking our 1:1 mapping.

```rs
fn build_locals(
  &mut self,
  binds: Vec<(String, Ast<String>, SyntaxNode<Lang>)>,
  body: Ast<String>
) -> Ast<String> {
  binds.into_iter().rfold(body, |body, (var, arg, child)| {
    let app_id = self.next_id();
    let fun_id = self.next_id();
    if let Some(let_binder) = child.first_child_by_kind(&|kind| kind == Syntax::LetBinder) {
      self.insert_node(fun_id, SyntaxNodePtr::new(&let_binder));
    }
    self.insert_node(app_id, SyntaxNodePtr::new(&child));
    Ast::app(app_id, Ast::fun(fun_id, var, body), arg)
  })
}
```

We accomplish a secondary task whilst desugaring lets, nesting let bindings correctly.
`body` is the body expression of our innermost let binding, which will be the last element of `binds`.
We walk binds backwards starting with `body`, constructing new let expressions out of the previous until we reach our first bindings.
The first binding is our outermost let expression and should include all our other bindings within it's body.

Once we have our nesting in under, the work of producing `Ast` nodes is straight forward.
Given a let expression `let <var> = <arg>; <body>` we turn it into a function immediately applied to an argument `Ast::app(.., Ast::fun(..., var, body), arg)`.
This transformation works because the identifier bound by our let is only in scope for the body of the expression.
The same way function parameters are only in scope for the their function bodies.

Care must be taken in how me map our new `Ast` nodes back onto our CST.
We might, and by that I mean I did and we can learn from my mistake, naively map both our function and application to the `child`.
This presents a problem for our full compiler.

It's helpful to have each CST node map to a single `Ast` node.
If both our function and application share a CST node, whenever we ask for the AST node at a given position in the file, we'll have to pick between our two nodes.
A surmountable hurdle, but a needless complication to are already daunting task.
We'll happily save our future self the trouble.

You've convinced me.
One CST node maps to one AST node.
But we, sometimes, map both our AST nodes to CST nodes.
Why not just map `app_id` to `child` and be done with it?

We can do this and then anytime we want the function we'll unpack our `App` node to get at it.
It turns out that whenever we're operating on a `LetBinder`, we always want the function and never the application.
We can save ourselves some more work by connecting the two directly without risking our 1:1 mapping.

## Let's see let in action

Let's get a feel for our desugaring by working through an example.
We'll start with the syntax:

```rs
let one = |s||z| s z;
let add = |m||n||s||z| m s (n s z);
add one one
```

All my [church](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals) heads sound off in chat.
I don't even know why we're planning to add more features.
This is a perfectly good way to do addition, if you ask me.
That syntax gets parsed into a CST, that we'll only show the highlights of:

* Program
  * Expr
    * Let
      * LetBinder "one"
      * Expr ...
    * Let
      * LetBinding "add"
      * Expr ...
    * App
      * App
        * Var "add"
        * Var "one"
      * Var "one"

From that CST, we're going to desugar into the `Ast`.
We'll omit `NodeId`s and the body of our functions for brevity.
We just want to get a sense for how our let expressions transform:

```rs
Ast::App(
  Ast::Fun("one", 
    Ast::App(
      Ast::Fun("add",
        Ast::App(
          Ast::App(Ast::Var("add"), Ast::Var("one")),
          Ast::Var("one")
        )
      ),
      Ast::Fun("m", Ast::Fun("n", Ast::Fun("s", Ast::Fun("z", ...))))
    )
  ),
  Ast::Fun("s", Ast::Fun("z", ...))
)
```

Phew, writing that out by hand really makes me appreciate we got the computer to do it for us.
Now of course, let is syntax sugar because we could reach the same `Ast` by writing:

```rs
(|one| 
  (|add| add one one) 
  (|m||n||s||z| m s (n s z))
) (|s||z| s z)
```

I'll leave it to you to verify, but this turns into the same `Ast`.
Again, I know which one I'd rather write.
But I think it's insightful to see that we don't _need_ let expressions.

{{< notice note >}}

This is only the case in our language because of choices we made around the type system.
In haskell, as the most notable example, let bindings allow for different typings than function parameters, making this transformation invalid.
We don't have any special treatmeant of let expressions, so we can desugar them.

{{< /notice >}}

One thing is still bugging me about the desugar example.
Our `Ast` uses `String` for varaibles, but during type inference we use `Var`.
We're going to need one more pass before we can tie the knot and pass our `Ast` to type inference: name resolution.
