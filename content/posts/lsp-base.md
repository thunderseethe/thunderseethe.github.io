+++
title = "Making an LSP for great good"
date = "2025-12-29T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "LSP"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Language Server Protocol", "Query Based", "Incremental Computation"]
description = "Implementing an LSP from our query-based compiler passes"
+++

* Intro
  * Summit of compilation
  * We're going to make an LSP (and more importantly a query based compiler).
  * Clarify we'll use LSP as shorthand for a server that implements the protocol.
  * The LSP is a vehicle for motivating our choice of compiler architecture (query-based).
    * There are other good reasons to be query based, but an LSP really highlights the benefits

* Compilation.
  * Batch compilation example.
  * Queries invert the traditional model.
  * Rather than Lex->Parse->Desugar we ask "What would the desugaring of this file look like and behind the scenes that query depends on parsing (which depends on lexing)."
  * To understand why, let's talk about what LSP is.

* What is an LSP
  * We're going to need at least a rudimentary understanding of LSP.
  * Solves an M:N problem for editors and language
    * Without a protocol each of M editors needs an IDE implementation for N languages
    * The protocol allows for each language to implement their share of the protocol and it will work with any editor that implements their share of the protocol.
  * Response/Request format (contrast with batch model)
    * The difference really comes alive here.
    * Unlike the unswerving batch compiler, our query compiler must be quick on its feet.
    * Responding to whatever the request asks of us and only what it asks of us.
    * We might even receive a requst in the midst of responding to a request.
  * Give some example queries?
  * Link to a more in depth explanation?

* Expectations
  * Implement LSP
    * Focus on language and guts.
    * Not on the server (farming out to a server library).
    * Discuss requests and responses, but not how we marshal JSON or shuffle bytes.
  * Playground code
    * Our LSP includes code that is used by the playground.
    * Won't be explaining it explicitly, but feel free to peruse it and the playground code itself.
  * Won't cover all of LSP.
    * LSP has a ton of surface area.
    * Link to the specification.
    * Implement a subset of requests that give a sense of how it's done.
      * Cover querying
      * Cover editing
      * Hopefully can extrapolate to the remainder
  * Our end result will be an LSP, but that's mostly just a vehicle for talking about query-based incremental compilation.
    * All the techniques we discuss today apply outside of LSP.
    * They're simply well exemplified by LSP due to its interactive nature.

* TODO: Transition from expectations to starting with the query engine.

* Query Based Engine
  * Query
    * Query takes inputs and produces an input
    * Sounds like a friend of ours, the function
    * Queries are like functions except:
      * Output is cached
        * Requires our query to be side-effect free.
      * Query is only re-run when an input changes
    * Incremental reuse allows us to avoid re-doing work that hasn't changed

  * Caveats on our implementation.
    * No consideration for persistence.
    * No cycle detection (infite loops are a problem).
      * In theory our queries should form a DAG.
      * In practice, they don't.o
      * Cycle detection helps us debug such bugs without waiting forever.
    * Boilerplate.
      * Use salsa in practice or macro magic.
    * TODO: Where to put this.

  * Before we can write our compiler queries, we need a query engine to run them.

  * Query Engine Responsibilities
    * Update query result when inputs change ("run the queries" so to speak)
    * Store query results
    * Track query dependencies

  * By virtue of running our queries, our query engine decides when to run queries and much more importantly when not to run a query.
  * The efficacy of the query engine lies in its deftness at determining a query does not need to be run, saving us time and effort.
  * We'll need an algorithm to determine when a query must be re-run.
  * Rustc has just such an algorithm: the red-green algorithm.

  * Red-green algorithm. (TODO: https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation.html)
    * Every query has a color, either red or green, associated with it.
      * Red - stale
      * Green - fresh (cached value can be reused)
    * Before running a query, we check it's inputs.
    * If they're all green, we can reuse our cached result.
    * When one of our inputs is red, we re-run our query and produce an update result.
    * Critically, running our query doesn't necessarily mark it red.
    * If our result equals the cached value, we leave our query marked green.

    * In practice, this prevents superfluous changes in source code from cascading into recompilations.
    * Imagine we wrap an expression in parenthesis, a no-op in terms of behavior.
    * After desugaring, we'll see our AST hasn't changed and stop updating queries, allowing us to reuse the cached value for our most of compiler passes.

  * `QueryKey`
    * Each query has an associated key which denotes a particular query and it's arguments.
      * Big dumb enum for us, but a "real" system would do something less brittle: trait, macro, etc.
      * Used for storing query results.
      * Used for tracking query dependencies.
      * Talk about tradeoffs
        * Fingerprinting is important for persistence.

    * As key might imply, we'll use this as the key of lots of hashmaps
      * Our color map uses this key to track the color of keys.
      * We key our cached query results by `QueryKey`.
      * Dependencies are tracked by `QueryKey`.

  * `QueryContext` runs our queries
    * All of our queries are methods on `QueryContext`
    * TODO: Code snippet
    * `db` stores our query results
    * `dep_graph` tracks our query dependencies
    * TODO: Note arc and async-ness of our server?

  * Store query results
    * Database holds the results of all our queries.
    * Field per query
    * Keyed by `QueryKey`
      * Explain `QueryKey`
    * TODO: Explain usage of `DashMap`?
    * Revision
      * Rust's red-green assumes a batch compilation model (one shot compilation).
      * Our LSP is a long-lived process that will compile our code multiple times.
      * Revision forces queries to re-run on new input.
      * Otherwise, a query can end up marked green incorrectly when it should have been marked red by changed input.
        * TODO: Possibly explain interaction with try_mark_green and dependencies?
        * NOTE: This doesn't mean we re-run everything every revision update, just that we check.
        * NOTE-NOTE: Revision is used by salsa
    * Colors
      * TODO

  * Track query dependencies.
    * Dependencies tracked dynamically in a graph.
      * Note why static dependencies won't work and are easier to mess up.
      * Include anecdote about trying this and it failing.
    * When a query calls another query we write it down in our graph.
    * TODO: Code snippet

  * `query` is our central pillar.
    * Ties together `Database` and `DepGraph`.
    * Runs query, tracks dependencies, updates results.
    * Implements red-green algorithm (unless I've botched it of course).

  * `query` walkthrough
    * `update_value`
      * If we determine our query is stale, re-runs the query, handling any state updates, and returns the value.
      * IMPORTANT: When we run producer we use a new `QueryContext` with an updated parent. This is crucial for constructing our dependency graph.
    * Check we have a revision for our query.
      * If not, we haven't run this query yet and we `update_value`.
    * Upon a stale query revision, we return early with `update_value`.
      * This won't necessarily re-run the query but forces a check that it isn't stale.
    * Last we try to mark our query green.
      * If we succeed, we can reuse our cached value
      * An incremental win!
      * Otherwise, we need to re-run our query with `update_value`.
    
  * `try_mark_green` walkthrough
    * Tries to mark each dependency green
    * If it can, this node is green
    * If it can't, this node is red
    * TODO: code snippets
 
* Compiler Passes
  * TODO: Figure out where to handle `ContentOf`. It's a special case because it's an input query.
  * Each pass is formulated as a query, requiring three things:
    * An entry in `QueryKey`.
    * A field in `Database`.
    * A function calling `query`.

  * We'll walk through what that looks like for our parser query, `cst_of`, to get the idea.
  * `cst_of` depends on one query `content_of`, providing the content of our file, and produces our Concrete Syntax Tree (CST) and any errors as output.
  * Add an entry to `QueryKey`.
  * LSP associates each "file" (may not be a literal file on disk) with a URI.
  * We reuse that as a convenient way to identify CSTs in our query key:
  * TODO: Code snippet for query key.
  * NOTE: We'll only ever have one URI right now. We don't support importing other files.

  * Database gets a new field `cst_query`.
  * It contains our two parsing outputs: `Cst` and `PellucidError`.
  * TODO: Should we explain `DashMap` here?

  * We put these pieces together in our `cst_of` function.
    * TODO: Code snippet signature missing producer.
    * Our function is a thin wrapper around `query` that specifies the cache in database and constructs our `QueryKey`.
    * TODO: Code snippet showing producer
    * NOTE: Our "irrefutable" pattern match on key is an unfortunate consequence of using our enum rather than a more sophisticated key type.
    * We call `content_of` (another query) to get our source text, pass that to our vanilla `parse` from (TODO: Link blog post), and return our outputs.
    * All our work in `query` has made it trivial to wrap `parse` up as a query, which is good because we're gonna need _a lot_ of queries.
    * NOTE: We map our specific `ParseError` into the more generic `PellucidError`

  * TODO: Consider using `types_of` to show how error accumulation behavior works.
    * TODO: Consider updating how error behavior works.
  * Let's take a look at another query `simple_ir_of` to cement our understanding.
  * We'll be faster this time since we get the gist.
  * `simple_ir_of` returns the optimized Intermediate Representation (IR) from our simplification pass.
  * TODO: Code snippet.
  * We see the same patterns:
    * Thin wrapper around `query`.
    * Calls input query `ir_of` to get our unoptimized IR.
    * Returns our output IR.
  * Our output is wrapped in an Option here.
    * Talk about resilience change.

  * The queries for our other compiler passes can be found in the [full source code](TODO).
  * They follow a similar pattern. 
    * `wasm_of` breaks the mold because it has to work around our lack of top level items.
  * We're going to move on to cover the queries we want to build atop our passes.
  * We'll start with a critical one: Diagnostics.

* Diagnostics
  * List of errors produced for the current source text during compilation.
  * All errors come from parsing, desugaring, name resolution, and type inference.
    * After that we assume things are right and crash rather than produce errors.
  * Because each pass accumulates all errors up to that point, we only have to check `types_of` to get all our errors.
  * TODO: Code snippet for types_of
  * We walk that error and construct a diagnostic for each kind of error.

  * A diagnostic is an LSP concept. 
  * It supports a lot of error reporting, but for our purposes we'll make do with `new_simple` which takes a span in source text and a message.
  * Our structure for creating a diagnostic stays consistent for each error kind, but the details shift slightly:
    * Get a span for our error
    * Format our error kind as a human readable string.

  * For parsing, we have spans ambiently available.
  * But we need to translate them into a format LSP understands.
  * `newlines.lsp_range_for` handles this for us by converting our byte based span into a line/column range expected by LSP.
    * TODO: Introduce newlines somewhere before this.
  * Once we have a range, our message for parsing is just the list of expected syntax.

  * Desugar also has immediate access to a span via the CST.
  * We convert that span and then match on desugar kind to print our message:
    * TODO: Code snippet
  * This is a big match, but it is not doing a lot.
  * Each case gets a human readable string and we're done.

  * Name resolution only has access to the AST.
  * To get a span we're going to need our AST to CST map from desugaring.
  * We can retrieve this by calling `desugar_of`, since we've just called `types_of` we can be confident this will be a cache hit.
  * We only have one kind of error: an undefined variable.
  * TODO: Code snippet assembling our diagnostic

  * Types employs the same span-producing tactic as name resolution.
  * We then convert our type error into a string.
  * Our type errors contain types, duh, which are not easily readable by default.
  * We have pretty printer that handles this for us.
  * It will print type variables as fancy greek letters and handle adding parens to disambiguate nested types.

  * With all that out of the way all that remains is to return our list of diagnostics.

* Queries under our belt; Time to take a look at how it all gets put together into an LSP.
* TODO: Explain LSP at a high level maybe with a diagram?
* tower-lsp handles all our server stuff.
* We don't have to worry about http requests, json marshalling, etc.
* tower-lsp provides a trait that represents the Language Server Protocol

* We implement the methods in the trait that our LSP supports, and tower-lsp takes care of turning that into a server for us.
* Our old friend the shared struct, but now it's a server!
* TODO: Code snippet PellucidLsp
* Our shared struct holds reference counted pointers to our database and dependency graph.
  * We'll be cloning these pointers around as we query, but this ensures they live for the lifetime of the server, and provide easy access when we need them.
* It also holds client, a tower-lsp type that represents the client connected to our server.
  * Lets us send messages to the client, rather than just respond to client requests


* We begin by implementing some methods that handle logistics
  * `initialize` is called by the client upon starting the LSP
    * server responds with a message that specifies what functionality it supports.
    * For example, our server supporst diagnostics, so we set the `diagnostics_provider`, broadcasting that we can provide diagnostics if the clients supports it.
    * Allows clients to only ask for functionality that the server can provide.
    * Also allows configuring some more general settings such as how to update content when text changes occur.

  * `did_open` notifies the server that the client has opened a new file (again not necessarily a literal file)
    * For us, that means we want to the file's content in our database
    * Once we've done this we check for any diagnostics and publish them to the client.
      * Explain `root_query_context`.

  * `did_change` notifies the server that file contents have been changed
    * This can be a delta or the full updated contents of the file.
    * We apply the changes to our current content and then update the database with our new content.
    * We also publish update diagnostics based on our change.

* With the logistics out of the way we can move on to requests that implement features: `diagnostic`.
* `diagnostic` is like `publish_diagnostics` but the client can call it to retrieve any diagnostics on demand (pull vs push).
* It supports reporting partial diagnostics, allowing for sophisticated servers to stop partway through generating diagnostics if it's taking too long.
* For our simple server, we'll always produce a full report.
* If our diagnostics are empty, we report them as unchanged so the client knows it doesn't need to do anything.
* TODO: Code snippets.
* Our logic could be fancier here.
* We could try to detect when our non-empty list of diangostics is the same as the last one we returned.

* Move on to more interactive queries
* Namesake features of LSPs (and IDEs): goto_definition, references, rename, hover, and completion.
* They all share two core queries we'll cover first: `syntax_node_starting_at` and `ast_node_of`.

* `syntax_node_starting_at` and `ast_node_of`
  * Link to [The Heart of a Language Server](https://rust-analyzer.github.io//blog/2023/12/26/the-heart-of-a-language-server.html)
  * Together they take us from a position in a file to the AST node corresponding to that position.
  * `syntax_node_starting_at`
    * Traverses the CST looking for the token at the given byte (translated from line/column).
    * Note resilience maybe?
    * Returns handle into  the CST
  * `ast_node_of`
    * Looks up a CST handle and returns it's typed AST node
    * Get our ast mapping from desugaring.
      * This works because we know one CST maps to one AST node in this map
    * Use `NodeId` of our desugared AST node to get the ast node of our typed AST tree.

* TODO: Figure out the order of remaining queries

* goto_definition
  * Explain what it does
  * Calls `definition_of` query.
  * `definition_of`:
    * Uses 1-2 combo `syntax_node_starting_at` into `ast_node_of` to get the ast node under our cursor.
    * If it's not a variable, it doesn't have a definition. We bail.
    * Walk up our AST looking for the node that defines our variable.
    * Map that node back to its CST node.
    * Grab the range of the CST node.
    * NOTE: The pattern Span -> CST -> AST -> Do the work -> AST -> CST -> Span. We're going to see this pattern a lot.

* references
  * Explain what it does
  * Calls `reference_at` query.
  * `reference_at`:
    * Same combo to get the ast node 
    * `var_refernce`
      * Helper that walks our ast grabbing every instance of our var
    * Walk AST vars
      * Convert them back to a syntax node
      * Special case for functions, we select the funbinder rather than the whole function
        * We don't have to do this for lets because desugaring mapped those nodes to the letbinder directly.
      * Get the range of our syntax node
    * Return list of references

* rename
  * Explain what it does
  * Reuses `reference_at` query
  * Instead of returning the list of locs, we first turn it into a list of edits
  * Client is responsible for applying the edits
    * This will in turn trigger a did_change, publish_diagnostics, etc.

* hover
  * Explain what it does
  * `hover_of` query
  * `syntax_node_starting_at` but not yet `ast_node_of`
  * We first check our node is a valid hover target
    * If it's a funbinder, we want to use the syntax node for the function itself
  * Get our AST node so we can get its type
  * We also get a range from our CST
  * We pretty print the type and return it along our range as our hover
    * NOTE: Should we talk about the other possible hover options here?

* completion
  * Explain what it does
    * Not just scope of our program, but scope at a cursor
    * Example of where this matters.
  * `scope_at` query
    * `zip_ast` combines our AST to produce a `AST<(String, TypedVar)>`
      * Talk about how `Hole` is handled?
  * Walk parents of our AST node
    * In reverse, so that the nearest enclosing binding is last and shadows any others
    * Collect bindings from `Fun` and grab their name and pretty printed type
  * Return our collection

* playground specific stuff
  * execute_command
    * used by playground
    * worth noting?
