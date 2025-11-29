+++
title = "Desugarging the Relationship Between Concrete and Abstract Syntax"
date = "2025-11-12T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Desugar"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Parsing", "Recursive Descent", "LL(1)", "Lexing", "Concrete Syntax Tree", "Error Recovery"]
description = "Converting our CST into our AST and desugarging let expressions"
+++

Previously, we, begrudgingly, [parsed some syntax into a Concrete Syntax Tree (CST)](/posts/parser-base).
With that tarpit deftly dodged, we can proceed to our next pass desugaring.
Desugaring removes syntax sugar and maps our CST onto our Abstract Syntax Tree (AST).
Our CST leaves us with a lot of cruft, such as `|` or `=`.
This stuff was important for telling head from tail in our initial source file, and we'll want to have it around when we're reporting diagnostics, but the rest of the compiler doesn't really care about such mundane affairs.
Desugaring helps us strip away all the syntax and focus in on what's important, lightening the cognitive load for following compiler passes.

But... do we really gotta?
It seems like a pain.
Can't the later passes just deal with the excess syntax?
We've come to expect that's the tee up for why we can't do that, but actually you kinda just...can.
In fact, that's exactly what Swift does.

They parse the CST, and their "AST" is just the subset of fields on the CST that are semantically relevant.
It's a perfectly valid strategy.
I might even _recommend_ it.
That is of course, if you didn't write every following pass of the compiler already using an explicit AST already.
But who would do that, haha.
Don't worry, we have a real reason to use a separate AST as well.

We find let expressions in our syntax, but they are nowhere to be found in our AST.
Syntax sugar turns out to be a common motivator for splitting CST and AST.
If you look at `rust-analyzer`, they employ a similar strategy to Swift.
[They expose](https://github.com/rust-lang/rust-analyzer/blob/master/crates/syntax/src/ast/generated/nodes.rs) a bunch of helpers on the CST for the semantically interesting stuff.
Despite that, they still produce a separate tree called [HIR](https://github.com/rust-lang/rust-analyzer/blob/master/crates/hir-def/src/hir.rs#L191).

Rust desugars away a lot of its surface syntax, as well.
Accommodating this transformation requires producing a new tree.
It's not enough to provide methods for the semantically interesting stuff.
We need to fundamentally change the structure of our tree.

As we change our tree, we need to remember where we came from.
It's important that we're able to map our AST back onto our CST.
This will matter not only for error reporting, but also for queries.
If we want to go to definition, we'll need to determine the AST node our cursor is pointing at and then use that to determine where it's definition is.

Desugaring produce our new AST _and_ a mapping from AST nodes to CST nodes.
Desugaring, like parsing, is also going to be resilient.
We produce a list of errors alongside our AST and mapping.

For the most part, desugaring is straightforward.
We walk our CST, taking the interesting bits out as we go and stashing them in the new AST we're constructing.
Conveniently, our syntax nodes map directly onto our AST nodes.
Almost as if we designed them that way.

Let expressions are an exception, requiring some more care.
They can't be mapped directly onto our AST.
We have to represent them with a tree of nodes and map them back onto our CST.

## Traipsing Through Our CST

During parsing, we were concerned with building up our CST, giving little consideration to how we consume our CST.
That changes in desugaring.
We are now concerned not only with how we traverse our CST, but how we store our CST.
Recall one of our outputs is a mapping between CST and AST.
Producing such a mapping requires we have a way to reference a particular CST node.

Our CST is provided by [`rowan`](https://crates.io/crates/rowan), and we happen to be in luck.
`rowan` provides not only a CST, but a way to traverse it.
Traversal is performed by [`SyntaxNode`](https://docs.rs/rowan/0.16.1/rowan/api/struct.SyntaxNode.html).
A type we did not encounter at all during parsing.

We can construct a `SyntaxNode` from our parsed `GreenNode`, providing us with a [suite](https://docs.rs/rowan/0.16.1/rowan/api/struct.SyntaxNode.html#method.first_child_by_kind) of new methods.
The most common method we'll use is `first_child_by_kind`.
`first_child_by_kind` takes a `Fn(Syntax) -> bool` and returns the first node that returns true for our function
Our predicate allows us to pick nodes of a particular kind (`SyntaxKind::LetBinder`, `Sytnax::Expr`, etc.) out of our tree.

Notably, `first_child_by_kind` only returns a syntax node.
It cannot return a token, aka a leaf node in our CST.
This is not an oversight on `rowan`s part but a conscious design decision.
If we want to find tokens, we can use `first_child_or_token_by_kind`.

When wading through a `SyntaxNode`'s children we only care about the nodes.
The tokens will be syntactic noise such as `Whitespace` or `Equal`, which are irrelevant to producing our AST.
`rowan` knows this and lets us skip right to the action.

This is why we wrapped notable `Identifier` tokens in nodes during parsing.
`FunBinder` and `LetBinder` always wrap a single `Identifier` (give or take some `Whitespace`) but let us find that identifier via `first_child_by_kind`.

## Setting Up For Success
 
Like our other passes, desugar is a set of recursive methods walking a tree.
With all the trees we're traversing, we'll have covered a whole forest by the time we're done with this compiler.
Also like our other passes, we share state between those recursive methods in a `Desugar` struct:

```rs
struct Desugar {
  node_id: u32,
  root: GreenNode,
  ast_to_cst: HashMap<NodeId, SyntaxNodeHandle>,
  errors: HashMap<SyntaxNodePtr<Lang>, DesugarError>,
}
```

Desugar is where we're on the hook to uniquely identify our `Ast` nodes.
`node_id` tracks the next available ID as we construct AST nodes.
Like `VarSupply` from deeper in our compiler, we increment this counter every time we create a node.

`root` is the root of our CST from parsing.
This is the same CST desugar is in the middle of traversing.
A `GreenNode` is cheap to clone, so we don't need to worry about holding a copy here.

`ast_to_cst` maps our `Ast` nodes back onto the CST nodes that spawned them.
This mapping will be central to error reporting, taking errors on our AST nodes and turning them into spans in our source file.
We might be surprised to see it stores something called a `SyntaxNodeHandle`, rather than a `SyntaxNode`.

A `SyntaxNode` is a pointer under the hood.
This is good for performance, but not great for long term storage.
Instead of trying to figure out how to store a pointer safely, we store a reference to the root of our tree and an index that will let us return to the position in our tree our `SyntaxNode` pointed at.

{{< notice note >}}
As the name might imply, what we're describing here is the [handle pattern](https://floooh.github.io/2018/06/17/handles-vs-pointers.html).
{{</ notice >}}

We can see this if we pop open `SyntaxNodeHandle`:

```rs
struct SyntaxNodeHandle {
  root: GreenNode,
  ptr: SyntaxNodePtr<Lang>,
}
```

`SyntaxNodePtr` comes from `rowan`.
Despite the name, a look at its definition reveals an absence of pointers:

```rs
struct SyntaxNodePtr<L: Language> {
  // This is Syntax for us
  kind: L::Kind,
  // This is the span in our source text
  range: TextRange,
}
```

From the span and kind of our node, we can find it within `root` and produce a `SyntaxNode` when we need one.
We work with `SyntaxNode` while traversing because it's fast, but once we want to store a node we convert it to `SyntaxNodeHandle`.
When we want to traverse again, we convert our handle back into a `SyntaxNode` and pick up where we left off.

`error` also needs to store a `SyntaxNode` to point at where errors occurred.
We're less concerned with restarting traversal for our errors, so it suffices to store a `SyntaxNodePtr`.

## Taking the Icing Off the Cake

With our state squared away, we can take a look at our entry point `desugar`:

```rs
fn desugar(root: GreenNode) -> DesugarOut {
  todo!()
}
```

We take in a `GreenNode` from parsing, and produce a `DesugarOut`:

```rs
struct DesugarOut {
  ast: Ast<String>,
  ast_to_cst: HashMap<NodeId, SyntaxNodeHandle>,
  errors: HashMap<SyntaxNodePtr<Lang>, DesugarError>,
}
```

`DesugarOut` holds the three things we produce from desugaring.
Due to our resilience, we always produce all of our outputs in some shape.
From there, our body is brief:

```rs
fn desugar(root: GreenNode) -> DesugarOut {
  let mut desugar = Desugar::new(root.clone());
  let ast = desugar.desugar_program(SyntaxNode::new_root(root));
  DesugarOut {
    ast,
    ast_to_cst: desugar.ast_to_cst,
    errors: desugar.errors,
  }
}
```

We construct `Desugar`, desugar our program's AST, and then assemble our outputs.

## From the Top With Programs

Recall from parsing, our program is just an expression.
We see that's still the case for `desugar_program`:

```rs
fn desugar_program(&mut self, cst: SyntaxNode<Lang>) -> Ast<String> {
  let Some(expr) = cst.first_child_by_kind(&|kind| kind == Syntax::Expr) else {
    // Assume parser has emitted an error for the missing node and just return a Hole here.
    return self.hole(&cst, DesugarError::MissingSyntax(Syntax::Expr));
  };

  self.desugar_expr(expr)
}
```

We find the first `Expr` node in our CST.
There should only ever be at most one, so the first is always correct.
Failing to find an expression, we assume our program is invalid and return a `hole` for our program.
`hole` constructs an `Hole` AST node and maps it to our CST node:

```rs
fn hole(&mut self, node: &SyntaxNode<Lang>, kind: DesugarError) -> Ast<String> {
  let ptr = SyntaxNodePtr::new(node);
  self.errors.insert(ptr, kind);

  let id = self.next_id();
  self.insert_node(id, ptr);
  Ast::Hole(id, "_".to_string())
}
```

`Hole` is part of our resilience strategy, previously seen in [type inference](/posts/types-base/#holy-ast-batman).
Rather than failing at the first invalid AST, we treat it as a Hole and try to recover as much of our AST as possible.
Hard to see the value in that when our whole program is hole, but I promise it'll be handy later.

Whenever we create a hole we attach an error to let us know what went awry.
Down the road we can use these to report diagnostics to the user.

## Expressive Desugaring

When we find our `Expr`, we pass it along to `desugar_expr`:
The first thing we do in `desugar_expr` is check we are looking at an expression.

```rs
fn desugar_expr(&mut self, expr: SyntaxNode<Lang>) -> Ast<String> {
  if expr.kind() != Syntax::Expr {
    return self.hole(&expr, DesugarError::MissingSyntax(Syntax::Expr));
  }

  todo!()
}
```

We just checked for a `Expr` node in `desugar_program`.
Why are we checking again?
We'll call `desugar_expr` all over the place, and we won't always know it's an expression ahead of time.
We check to make sure that's the case in `desugar_expr`.
If it's not, we treat our entire expression as a `hole`.

Empowered by the knowledge we're looking at an expression, we being the process of desugaring let bindings.
Recall our expression syntax is any number of let bindings followed by an application:

* Expr
  * Let
    * ...
  * Let
    * ...
  * App
    * ...

In a perfect world, we'd consume this layout and be on our merry way.
Alas, we have to contend that the expression we're looking at isn't actually valid.
We maintain a list of bindings and walk the children `SyntaxNode`s of our expression:

```rs
let mut binds = vec![];
// The only tokens that appear in Lets are whitespace that we are happy to skip here.
for child in expr.children() {
  match child.kind() {
    Syntax::Let => match self.desugar_let(child.clone()) {
      Ok((var, arg)) => binds.push((var, arg, child)),
      Err(error) => {
        let hole = self.hole(&child, error);
        return self.build_locals(binds, hole);
      }
    },
    _ => {
      let body = self.desugar_app(child);
      return self.build_locals(binds, body);
    }
  }
}
```

`children` only walks over syntax nodes, not tokens, so we don't have to worry about child being a `Whitespace` or other minutia.
If our child is a `Let`, we try to desugar a let binding and add it to our list of bindings.
Our let bindings might be an error.
In which case we treat the entire child as a hole, emit an error, and call `build_locals` using our hole.

We're ignoring the rest of our children when this happens.
Once we encounter an erroneous binding, we assume the rest of our expression is invalid as well.
Otherwise, our error would have been localized within the let binding rather than the entire binding itself.
There's no point in translating the remainder of our expression as an error.
It won't provide better analysis in later passes, so we save ourselves the effort.

If all goes well, eventually we'll hit something that isn't a `Let`.
In that case, we assume it's the body of our expression and pass it to `desugar_app`.
Whatever comes out of that, hole or otherwise, we pass to `build_locals`.

On our happy path, our for loop returns with a call to `build_locals`.
If we exit our for loop, something has gone wrong.
Either our expression has no children, or it only had let bindings and no body.
We'll try to attach our error to the last node of expression, and fallback to the entire expression if it's empty.

## desugar_let

`desugar_let`, much like `let_` from our parser, does not desugar a full let expression
It only handles the binding portion: `let <var> = <expr>;`.
Which becomes the relatively flat CST (omitting whitespace which may or may not be present):

* Let
  * LetKw
  * LetBinder
    * ...
  * Equal
  * Expr
    * ...
  * Semicolon

Because of that we don't produce an `Ast` out of `desugar_let`, we can't.
Instead, we produce a pair consisting of our identifier, and it's definition, relying on `desugar_expr` to turn those into a full `Ast`.
We'll extract our pair from the children of our `Let` node:

```rs
fn desugar_let(
  &mut self, 
  bind: SyntaxNode<Lang>
) -> Result<(String, Ast<String>), DesugarError> {
  let mut children = bind.children();

  let Some(var) = children.next() else {
    return Err(DesugarError::LetMissingBinding);
  };

  let Some(binder) = (var.kind() == Syntax::LetBinder)
    .then_some(())
    .and_then(|_| var.first_child_or_token_by_kind(&|kind| kind == Syntax::Identifier))
  else {
    return Err(DesugarError::LetMissingBinding);
  };

  let ast = match children.next() {
    Some(expr) => self.desugar_expr(expr),
    None => self.hole(&bind, DesugarError::LetMissingExpr),
  };

  Ok((binder.to_string(), ast))
}
```

We assume our let binding only has two nodes (recall the distinction between nodes and tokens).
The first is a `LetBinder`, which holds the variable our let binds.
In parsing we parse an identifier and then wrap it up as a `LetBinder` making it easier to find now.
We unwrap our `LetBinder` node to reveal it's underlying `Identifier` token and get its text.
If our binding is missing, we error out

We don't check our next child for a specific syntax kind.
It's the definition of our let binding, which could be any expression.
We pass it to `desugar_expr` and use whatever it gives us.
Failing to find a next child at all, we produce a hole and let the user know they're missing an expression.

Next we move onto desugaring appl- You know what actually...

## You get the gist

We've got a taste for desugaring.
I trust you can extrapolate from what we've seen so far.
For each piece of syntax we:

1. Traverse to the interesting node in our CST
2. Extract it's text
3. Put it in our AST

When we fail to do any of those steps, we replace the AST we're constructing with a `Hole`, attempting to replace the smallest AST we can with a hole.
We'd rather replace the definition of a let with a hole than the entire let.
Whenever we create an AST node, we give it a unique ID and a pat on the head, map it back to our CST and send it on its way.
If you want to see it in glorious high resolution (depending on your monitor) detail, check out the [full code](TODO).
Instead of rehashing the same concepts we've covered above, let's move on to the next interesting bit: desugaring let expressions.

## build_locals

`build_locals` is, in a sense, where all the magic happens.
Our other helpers turn one syntactic construct into one AST construct.
Here, however, we turn our let expressions into multiple AST nodes.    
With the loss of our 1:1 mapping, we have to answer the question: How do we map multiple AST nodes back onto one CST node.
From this first quandary, more questions arise.

A let expression turns into a function nested within an application.
Whenever we write `let x = 1; incr x`, that's the same as writing `(|x| incr x) 1` and we'll represent let expressions as an `Ast::Fun` and `Ast::App`.
But, what's the span of our `Ast::Fun`?
Our tree transformation is destructive.
We've lost some information.

There isn't a contiguous span in our source that represents our function.
If we encompass all the elements of our function, as in `let ∣x = 1; incr x∣`, we also include parts of our application.
Our application faces a similar conundrum, but it's easier for us to handwave it away by saying our application span is our full let expression's span.

In lieu of picking the perfect span for our function, let's take a step back and consider why we need a span for our function.
Foremost, our span serves as a location for diagnostics.
After that, our span also serves to identify our AST node for any user interactions.
For example, if we want to get the type of our function parameter, we'll use the span to figure out which function to get the type of.

Our function doesn't actually need a span for that many diagnostics in practice.
If an error occurs in our function body, our function body is an expression that maps to its own span independent of our function.
A similar deal for any interactions occurring in the function body.
We don't have to worry about it.

In that case, we don't need a span for our _entire_ function.
If we can give a span to our function parameter, our function body will take care of itself.
Our narrowed task is much simpler to satisfy: `let ∣x∣ = 1; incr x`.
Just like that; We've assigned a span to our function parameter.
If we look at the implementation, we'll see that's exactly what we do:

```rs
fn build_locals(
  &mut self,
  binds: Vec<(String, Ast<String>, SyntaxNode<Lang>)>,
  body: Ast<String>
) -> Ast<String> {
  binds.into_iter().rfold(body, |body, (var, arg, child)| {
    let app_id = self.next_id();
    let fun_id = self.next_id();
    if let Some(let_binder) =
      child.first_child_by_kind(&|kind| kind == Syntax::LetBinder) {
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
The first binding is our outermost let expression and should include all our other bindings within its body.

Once we have our nesting in under, the work of producing `Ast` nodes is straight forward.
Given a let expression `let <var> = <arg>; <body>` we turn it into a function immediately applied to an argument `Ast::app(.., Ast::fun(..., var, body), arg)`.
This transformation works because the identifier bound by our let is only in scope for the body of the expression.
The same way function parameters are only in scope for their function bodies.

## Let's see let in action

Let's get a feel for our desugaring by working through an example.
We'll start with the syntax:

```rs
let one = |s||z| s z;
let add = |m||n||s||z| m s (n s z);
add one one
```

All my [church](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals) heads sound off in chat.
This is a perfectly good way to do addition, if you ask me.
I don't even know why we're planning to add more features.
That syntax gets parsed into a CST, that we'll only show the highlights of:

* Program
  * Expr
    * Let
      * LetBinder "one"
      * Expr ...
    * Let
      * LetBinder "add"
      * Expr ...
    * App
      * App
        * Var "add"
        * Var "one"
      * Var "one"

From that CST, we're going to desugar into the `Ast`.
We'll omit the body of our let definitions for brevity.
We just want to get a sense for how our let expressions transform:

```rs
use Ast::*;
App(
  NodeId(25),
  Fun(
    NodeId(26),
    "one", 
    App(
      NodeId(23),
      Fun(
        NodeId(24),
        "add",
        App(
          NodeId(22),
          App(
            NodeId(20),
            Var(NodeId(18), "add"), 
            Var(NodeId(19), "one")),
          Var(NodeId(21), "one")
        )
      ),
      Fun(NodeId(17), "m", Fun(NodeId(16), "n", Fun(NodeId(15), "s", Fun(NodeId(14), "z", ...))))
    )
  ),
  Fun(NodeId(4), "s",
    Fun(NodeId(3), "z", ...))
)
```

Phew, writing that out by hand really makes me appreciate all the work the computer does for us.
Now, because `let` is syntax sugar, we could also reach the same `Ast` by writing:

```rs
(|one| 
  (|add| add one one) 
  (|m||n||s||z| m s (n s z))
) (|s||z| s z)
```

I'll leave it to you to verify, but this turns into the same `Ast`.
I know which syntax I'd rather write.
But I think it's insightful to see that we don't _need_ let expressions.

{{< notice note >}}

This is only the case in our language because of choices we made around the type system.
In Haskell, let bindings allow for different typings than function parameters, making this transformation invalid.
We don't have any special treatment of let expressions, so we can desugar them.

{{< /notice >}}

One thing is still bugging me about the desugar example.
Our desugared `Ast` uses `String` for variables, but during type inference we use `Var`.
We're going to need one more pass before we complete the arch and pass our `Ast` to type inference: name resolution.
