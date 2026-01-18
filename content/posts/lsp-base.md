+++
title = "Making an LSP for great good"
date = "2025-12-29T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "LSP"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Language Server Protocol", "Query Based", "Incremental Computation"]
description = "Implementing an LSP from our query-based compiler passes"
+++

We've talked a big game about resilience this and interactive that.
With [name resolution](/posts/nameres-base) done, it's finally time to put our money where our mouth is.
All our compiler passes are assembled.
It's time to tie them together into a compiler, but not just any compiler.
Sure, we _could_ string together a batch compiler if we really had to:

```rs
fn batch_compiler(source: &str) -> Vec<u8> {
  let (cst, _) = parser_base::parse(&content);
  let desugar = desugar_base::desugar(cst);
  let nameres = name_resolution_base::name_resolution(desugar.ast);
  let types = types_base::type_infer(nameres.ast);
  let lower = lowering_base::lower(types.ast, types.scheme);
  let simple_ir = simplify_base::simplify(lower.ir);
  let monomorph = monomorph_base::trivial_monomorph(ir);
  let closures = closure_convert_base::closure_convert(ir);
  
  let mut defns = closures.closure_items;
  let main_defn = ItemId(
    closures
      .closure_items
      .last_key_value()
      .map(|(key, _)| key.0 + 1)
      .unwrap_or(0),
  );
  defns.insert(main_defn, closures.item);

  emit_base::emit_wasm(defns.into_iter().collect())
}
```

{{< notice tip >}}
Please do not ask me about error reporting.
I'm trying to make a point here.
{{</ notice >}}

But our aim is much loftier than that.
We're going to implement a language server for the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) (LSP).
You can actually see it right now in the interactive [playground](TODO) showing off its features.

For the uninitiated, LSP was invented to solve an [M:N problem](https://matklad.github.io/2022/04/25/why-lsp.html) in the IDE space.
There are multiple editors (Vim, VSCode, Helix, etc.) and there are multiple languages (in fact we're adding to that problem right now).
We want great IDE features in each of our editors, but implementing them per language per editor is a combinatorial explosion of work.
LSP saves us by introducing a unifying middleman.
Each language implements a server that speaks the protocol, and each editor implements a client that speaks the protocol.
Now any language with a server works with any editor with a client.

{{< notice note >}}
Technically what we're implementing is a language server for LSP, but we'll refer to it as an LSP throughout this article.
But we're not making a protocol.
{{</ notice >}}

Our server will provide classic IDE features like semantic information on hover, autocomplete, goto definitions, the works.
Clients will connect to our server and send us requests.
Our server performs just enough semantic analysis to answer the request.

While our end result will be an LSP, that is only a means to an end.
Our actual goal is to create an incremental query-based compiler.
An LSP just happens to particularly perfectly present the advantages of such a compiler.
Plus it's really cool to be able to see the compiler chug as we interact with the code.

A query based compiler is almost the exact inverse of our example batch compiler above.
Rather than one big pipeline shuffling data along, compilation is structured as a series of questions (queries if you will) that get answered.
Queries can depend on each other.
We'll have our final query "what does the WebAssembly for this source code look like?".
En route to answering that query, our compiler will ask a bunch of subqueries:

  * What does the IR for this source look like?
  * What do the types for this source look like?
  * What does the CST for this source look like?

And a whole bunch of other queries.
The advantage of this model, over a more traditional pipeline, is its ability to cache queries, allowing for incremental computation.
Once we answer the query "What does the CST for this source look like?" we can save that result and reuse it to answer anybody else who asks.
Even better, when we change our source code we only have to recompute queries that depend on that change.
If a query depends on an unchanged part of the code, we can just keep using the cached value.

This is especially pertinent for an LSP.
Requests need to be answered as fast as possible, computing our answers incrementally lets us do the least amount of work and respond quickly.
Or we might receive any number of requests without the source code changing.
Imagine a user jumping around their codebase by hitting the goto definition key multiple times.
We can reuse our cached query results across those jumps and only recompute things when our source code actually changes.

We understand what we're going to do now, implement a query-based compiler and a language server.
Let's talk about what we're not going to do.
Our focus will be on the compiler and how it fulfills requests for our LSP.
But building a server still involves a lot of server-y stuff: HTTP headers, JSON marshaling, IO, etc.
We won't be touching on any of that at all.
We use the [`tower-lsp`](https://crates.io/crates/tower-lsp) crate, and it handles all that for us.

We won't even cover all the protocol part of LSP.
LSP is a gigantic [spec](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/).
I'm not confident any server implements every request it specifies.
We will implement a meaningful but small subset of it, giving us a taste for how to implement IDE features.

## Queries

Our sights are set.
Summiting compilation requires an LSP implementation.  
Before we can implement our LSP, we need some compiler queries.  
Before we can write some compiler queries, we need something that executes queries.  
Before we can execute queries, we need to know what a query is.

A query takes in some inputs, runs some code, and produces an output, which sounds indistinguishable from functions.
What sets a query apart is caching its results, and only updating that cache when one of it's inputs change (and sometimes not even then).
As a side effect of this caching, our queries must be side-effect free (i.e. pure functions).
We only want to run a query when we're confident it will produce a different value from the previous run.
In an ideal world, most of our queries are cached most of the time.

Queries depend on each other and form a graph, much like a call graph for normal functions.
But this graph must be a Directed Acyclic Graph (DAG).
It can't contain cycles.
If a query could end up as it's own input, we'd suddenly have a very hard time telling when it's input hadn't changed.

Most of our queries follow the same logic:

* Check my inputs
* If they haven't changed, use my cached value
* If they have changed, run my body and cache the value

But our queries form a DAG, so some of them must have no inputs.
These independent queries are called input queries.
Because all our queries are pure and only depend on other queries they're the only way we input new information into the graph.
Rather than deriving their value from some logic, we provide setters for input queries, allowing for the outside world to update their value freely.
Whenever an input is set to a new value, that notifies us we need to update any queries that depend on that input.

## Query Engine

Something has got to be responsible for wrangling our query graph and its inputs.
If queries could do that themselves, any old function could do this by default and this article would write itself.
In a kinder world, perhaps.
Here in our world, however, that role rests on the query engine.

The query engine manages all the state we need behind the scenes to execute queries.
It has three responsibilities:

* Execute queries
* Cache query results
* Track query dependencies

Executing queries sounds straightforward on its face, but as we'll see, it's complicated by our other two responsibilities.
As we execute queries, our engine tracks each query's dependencies.
Once we've executed the query to produce a result, our engine keeps that around in a cache for future queries.
On the happy path, our engine determines our query don't need to execute and returns the cached value immediately.

Our query engine does a lot for us, but there's still more we might want from a query engine we won't be handling.
Any production query engine wants to persist query caches to disk, so they can be reused between runs of our program.
A whole realm of complexity resides behind that feature that we won't touch on at all.
Look to the [rustc docs](https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation-in-detail.html#the-real-world-how-persistence-makes-everything-complicated) for some insight into what that entails.

Our engine won't handle garbage collection.
Often a cached query result becomes unnecessary. 
It was cached for input that no longer exists or otherwise has been supplanted by future query runs.
Evicting these entries is important for a long lived process like an LSP, but it adds quite a bit of complexity to the engine and I don't know how to do it.
We're making do without today.
If you know more than I, please let me know.

Our engine won't handle detecting cycles between queries.
In theory, our queries form a DAG, lacking cycles.
In practice, they don't.
Cycle detection helps debug accidental cycles in a finite amount of time, but we'll press on without.

Finally, our engine will have quite a bit of boilerplate.
If we're doing it right, this will make it really explicit how the query engine operates, helping with learning.
In a real implementation a lot of what we'll do can be wrapped up with macro magic to save everyone a lot of time.
Heck, if your heart isn't set on building a query engine from scratch, consider an off the shelf solution such as [salsa](https://crates.io/crates/salsa).

## Red-Green Algorithm

The efficacy of our query engine lies in its deftness at determining when a query does not need to be run.
We'll need an algorithm to determine when a query must be run.
We borrow a tried and true algorithm from Rust: the [red-green algorithm](https://salsa-rs.github.io/salsa/reference/algorithm.html).

At a high level, the red-green algorithm associates each query with a color, red, or green.
When a query is green we're safe to reuse it's cached value.
When a query is red, we need to reexecute it and update it's cache.

{{< notice note >}}
We say query here, but we really mean a query with a particular set of arguments.
We'll have a query `cst_of(Uri)` that takes in a file URI and outputs a parse tree.
That query's cache will have an entry per file URI we parse, and each entry can be red or green
{{</ notice >}}

All our queries begin red.
Upon executing them, we mark them green.
When a query's input changes, we mark it red again.
Before our engine executes any query, it checks the colors of that query's dependencies.
If all of them are green, we can eschew our run and use our query's cached value.

When any dependency is red, our engine executes our query.
This does not necessarily mark our query red, however.
We first compare our new value with the cached value.
If our execution produced an equal value, we leave our query green.
Only when they are different do we mark our query red.

This is more important in practice than it might seem.
Imagine a scenario where we make a trivial change in our source text, changing whitespace, adding a comment, etc.
We have to rerun our parsing query.
Upon desugaring, however, we'd produce the same AST, marking our desugaring query green.
Any query that depends on desugaring no longer needs to update.
We've saved ourself from executing the remainder of our passes.

With our exposition out of the way, we can begin looking at some actual code.
`QueryContext` represents our query engine and holds everything it needs:

```rs
struct QueryContext {
  parent: Option<QueryKey>,
  db: Arc<Database>,
  dep_graph: Arc<DepGraph>,
}
```

Each query lives as a method on `QueryContenxt`.
As you've probably picked up, we're very interested in tracking queries.
Tracking requires identification.
We create a new type `QueryKey` to identify our queries throughout our engine.
It's a big dumb enum with one entry per query we support:

```rs
enum QueryKey { 
  ContentOf(Uri),
  CstOf(Uri),
  NewlinesOf(Uri),
  // ...the rest of our queries
}
```

Each variant represents a query and holds the arguments of that query.
We can see `content_of`, `cst_of`, and `newlines_of` all take a single `Uri` argument.
This is because they all work on one file represented by the path to that file as a `Uri`.
Once we get to our LSP features, we'll see some more granular queries that take, for example, a specific location within a file.

{{< notice note >}}
Our enum is straightforward but has some downsides (mostly around boilerplate).
It gets the job done and makes it easy to see what's going on.
You can solve these issues by using a fancier solution, probably involving a trait.
See rustc's [`QueryConfig`](https://doc.rust-lang.org/stable/nightly-rustc/rustc_query_system/query/trait.QueryConfig.html) for an example.
{{</ notice >}}

We'll see `QueryKey` all over as the key of hashmaps or the vertices of our dependency graph.
It's also the `parent` field of our `QueryContext`, where it tracks the parent query of the query we're currently executing, if one exists.
`parent` tracks our query dependencies during execution.

Our next field, `db`, is our database.
It contains a cache for each query:

```rs
struct Database { 
  colors: ColorMap,
  revision: AtomicUsize,
  // Query caches 
  content_input: DashMap<QueryKey, String>,
  cst_query: DashMap<QueryKey, (Cst, Vec<PellucidError>)>,
  newlines_query: DashMap<QueryKey, Newlines>,
}
```

Each of our cache fields is a `DashMap` from the [`dashmap`](https://crates.io/crates/dashmap) crate.
A dashmap is a concurrent hashmap.
We need it because we're building a server, so we handle requests asynchronously.
Dashmaps can be thought of as `RwLock<HashMap<K, V>>`.
Their implementation differs for performance, but it's a helpful mental model.

We store two important pieces of information: our color map and revision.
The color map stores the color of each query, as seen on the red-green algorithm.
The revision is a counter that gets updated whenever we receive new input.
It forces us to check green queries that ran on a previous input and make sure they're still up to date.

Our final field `dep_graph` tracks query dependencies.
Whenever one query executes another query to produce its results, we'll write that down in our dependency graph:

```rs
struct DepGraph {
  graph: RwLock<DiGraph<QueryKey, ()>>,
  indices: DashMap<QueryKey, NodeIndex>,
}
```

It offloads the bulk of storing logic to the [`petgraph`](https://docs.rs/petgraph/0.8.3/petgraph/) crate via `DiGraph` (a DAG).
It's critical we have _a_ dependency graph for knowing what dependencies a query has.
The particulars of _how_ that dependency graph knows dependencies are less critical to our goals today.
Feel free to dig into [the code](TODO) and see how it's done.
We'll suffice knowing our graph provides two operations:

```rs
impl DepGraph {
  fn add_dependency(
    &self,
    from: QueryKey,
    to: QueryKey
  );

  fn dependencies(
    &self,
    key: &QueryKey
  ) -> Option<Vec<QueryKey>>;
}
```

We can add a dependency between queries, and we can ask for all the dependencies of a query.

A single function employs all our query context: `query`.
This function implements our red-green algorithm and handles all three of our query engine's responsibilities.
It's a lot to take in, but once we understand `query`, implementing our queries on top of it consists of writing normal functions (with a little bit of boilerplate).
We'll start slow with the signature:

```rs
impl QueryContext {
  fn query<V: PartialEq + Clone>(
    &self,
    key: QueryKey,
    cache: &DashMap<QueryKey, V>,
    producer: impl FnOnce(&Self, &QueryKey) -> V,
  ) -> V {
    todo!("Implement me please")
  }
}
```

We can glean a surprising amount from our signature.
Whatever goes on inside, we end up returning a `V`.
As a generic type, `query` doesn't know how to produce a `V` itself.
It only has two sources of `V` available: `cache` and `producer`.

`cache` will be a field of `Database`.
In a fancier implementation, we might use a `Query` trait of some kind to automagically figure out the database field for each query.
Our humble engine will just pass it manually, speaking of boilerplate.

`producer` houses the logic of our query.
It's what we'll run if we can't reuse our cached value and need to produce a new one.
We pass it our `QueryContext`, so that it itself can call queries.
We pass it `QueryKey`, so that it has access to our query arguments.

From there, `query`'s implementation begins with defining a helper `update_value`:

```rs
let revision = self.db.revision.load(Ordering::SeqCst);
let update_value = |key: QueryKey| {
  todo!("we'll see what this in a sec")
};
```

At multiple points throughout `query` we might determine we need to update our queries value.
`update_value` shares that logic in one place:
Updating a query value comprises three tasks.
First, we add an edge to our dependency graph:

```rs
if let Some(parent) = &self.parent {
  self.dep_graph.add_dependency(parent.clone(), key.clone());
}
```

Whenever we execute a query, but only when we execute the query, we record it in the dependency graph.
We trust that if we're using a cached value the query dependencies have already been recorded and have not changed.
Second, we produce our new value:

```rs
let value = producer(
  &QueryContext {
    parent: Some(key.clone()),
    db: self.db.clone(),
    dep_graph: self.dep_graph.clone(),
  },
  &key,
);
```

The most critical bit here is that we update the parent of `QueryContext` when we run `producer`.
Any children queries executed will see `key` (our current query) as their parent and record that in our dependency graph.
Third, we check that our new value is different from our cached value:

```rs
let old = cache.insert(key.clone(), value.clone());
if old.is_some_and(|old| old == value) {
  self.db.colors.mark_green(key, revision);
} else {
  self.db.colors.mark_red(key, revision);
}
value
```

If our value hasn't changed, we mark our query green, saving us work!
`update_value` finishes by returning the new value.

Back in `query`, we continue by trying whatever we can to not call `update_value`:

```rs
let update_value = |key: QueryKey| { /* ... */ };
let Some((_, rev)) = self.db.colors.get(&key) else {
  return update_value(key);
};
// Our query is outdated
if rev < revision {
  return update_value(key);
}
```

First we check the revision of our query.
If our color map lacks an entry for our query entirely, this is our first time seeing this query and we have to run it.
If we have an entry, but it's revision is stale, we also have to run our query.

After checking the revision, we move on to checking the color:

```rs
let color = self.try_mark_green(key.clone());
match color {
  Color::Green => cache
    .get(&key)
    .unwrap_or_else(|| {
      panic!(
        "Green query {:?} missing value in cache\n{:?}",
        key, self.db.colors
      )
    })
    .value()
    .clone(),
  Color::Red => update_value(key),
}
```

We try to mark our query green.
Upon succeeding, we get to use our cached value.
Otherwise, we're forced to run our query.

`try_mark_green` checks each of our queries dependencies.
When all our dependencies are green, we mark our query green for free.
If we encounter a red at any point, our query is red:

```rs
fn try_mark_green(&self, key: QueryKey) -> Color {
  let revision = self.db.revision.load(Ordering::SeqCst);
  // If we have no dependencies in the graph, assume we need to run the query.
  let Some(deps) =  self.dep_graph.dependencies(&key) else {
    return Color::Red;
  };
  for dep in deps {
    match self.db.colors.get(&key) {
      Some((Color::Green, rev)) if revision == rev => continue,
      Some((Color::Red, _)) => return Color::Red,
      _ => todo!("a single case"),
    }
  }
  // If we marked all dependencies green, mark this node green.
  self.db.colors.mark_green(key, revision);
  Color::Green
}
```

Our main logic is looping over the dependencies recorded in our graph.
There's a case that requires more care, noted by our `todo!`.
Our dependent queries could be absent from our color map, or have a stale revision.
In that case, we recursively try to mark that dependency green:

```rs
if self.try_mark_green(dep.clone()) != Color::Green {
  self.run_query(dep);
  // Because we just ran the query we can be sure the revision is up to date.
  match self.db.colors.get(&key) {
    Some((Color::Green, _)) => continue,
    Some((Color::Red, _)) => return Color::Red,
    None => unreachable!("we just ran the query"),
  }
}
```

Upon succeeding, we're done our dependency is green.
Failing that, we still try to mark our query green by just running it and seeing what happens.
Running the query handles the case where we need to produce a new value, but that value is equal to our cached value.
`run_query` is a helper that matches on `QueryKey` and dispatches the appropriate function call for our query, throwing away the result.

That's everything `query` does.
Our query engine may be rudimentary but it is complete.
All the core concepts are on display and we can see how they combine.
It will execute queries incrementally and, once we write some queries, compile code.

 
## Compiler Passes

I loved learning about queries, but it didn't have a lot to do with compilers.
Our queries could compute anything: UI, Game State, an ETL pipeline.
That's great don't get me wrong, but we're here to build a bloody compiler.
It's time to talk about the queries we'll need to compile code.

At minimum, we'll need a query for each pass of our compiler (i.e. parsing, type inference, closure conversion, etc.).
Each pass query takes a similar shape.
Every pass contains a top level function as it's entry point (almost like it was designed for querying).
We wrap that function in a call to `query` that performs any previous passes' queries we need.

We'll walk through what that looks like for our parser query, `cst_of`, to get the idea.
Our query produces the parse tree for an entire file, identified by its file URI.
First we add an entry to `QueryKey`:

```rs
enum QueryKey {
  // ...
  CstOf(Uri),
  // ...
}
```

Database gets a field `cst_query` to hold our cached values:

```rs
struct Database {
  // ...
  cst_query: DashMap<QueryKey, (Cst, Vec<PellucidError>)>,
  // ...
}
```

It stores our two query outputs: the CST and any errors.
`PellucidError` is a big enum that holds a variant for each pass that might produce an error.
Named such because our language is named Pellucid (I probably should have mentioned that by now.).
With those two changes, we're ready to implement our query `cst_of`:

```rs
impl QueryContext {
  fn cst_of(
    &self, 
    uri: Uri
  ) -> (Cst, Vec<PellucidError>) { 
    self.query(
      QueryKey::CstOf(uri), 
      &self.db.cst_query, 
      |this, key| {
        todo!("Implement me")
      })
  }
}
```

We can see our database field and query key at work here, and that pesky boilerplate we've been talking about.
The actual logic of `cst_of` lives in the closure we pass for `producer`:

```rs
let QueryKey::CstOf(uri) = key else {
  unreachable!("cst")
};
let content = this.content_of(uri.clone());
let (root, errors) = parser_base::parse(&content);
(
  root,
  errors.into_iter().map(PellucidError::Parser).collect(),
)
```

For all the work we put into building our query engine, this is pleasingly short and mostly devoid of query concerns.
Our sole query-ism comes right at the start.
We unpack our query key to get our arguments back.
This can't fail.
We saw three lines prior that we always pass `QueryKey::CstOf`.
More boilerplate due to our enum.
A more sophisticated solution would prevent this, but we'll manage, _somehow_.

The remainder of our logic is pure.
We ask for the content of the file we want to parse with `content_of`.
Under the hood, this is another query and will track that `cst_of` depends on `content_of`, but we're free to ignore that detail here.
We pass our input to `parse` and then return its results as our queries results.
Our parser works in a specific `ParseError` type, so we wrap that up in a `PellucidError::Parser` variant.

We follow that pattern for all of our passes.
The specific function may change but the formula persists.
Create a query key, add a database field, and wrap our function in a `query` call.

Let's look at one more, but faster, to make sure we get the gist.
We'll jump deeper into the compiler and look at `simple_ir_of`, which optimizes the Intermediate Representation (IR) produced by lowering.
We create variant `QueryKey::SimpleIrOf(Uri)` and add field `simple_ir_query: DashMap<QueryKey, Option<lowering_base::IR>>` to our database.
`simple_ir_of` is then a quick call to `query`:

```rs
fn simple_ir_of(
  &self, 
  uri: Uri
) -> Option<lowering_base::IR> {
  self.query(
    QueryKey::SimpleIrOf(uri),
    &self.db.simple_ir_query,
    |this, key| {
      let QueryKey::SimpleIrOf(uri) = key else {
        unreachable!()
      };
      let lower = this.ir_of(uri.clone())?;
      let simple_ir = simplify_base::simplify(lower.ir);

      Some(simple_ir)
    })
}
```

We can see the same pattern applies here:

  * Get our inputs from `ir_of` query
  * Call our passes' top level function
  * Return the results

At this stage of the compiler, resilience is gone.
Any error we encounter is an internal compiler error.
Hard crashing, however, does a great server experience make.
We'd like our queries to be callable on invalid input, even if the underlying passes are not.
We bridge that gap by wrapping the results of our query in `Option`.
If we have errors in the frontend of the compiler, we simply don't call the underlying passes after type inference (our last resilient pass).

The queries for our other compiler passes can be found in the [full source code](TODO).
`wasm_of` breaks the mold a bit to work around our lack of top level items, but otherwise they all follow this pattern.

## Our Sole Input Query

With one exception, our sole input query, `content_of`.
As seen in `cst_of`, it provides the source text for a file URI.
By virtue of being an input query, it gets a `set_content` method:

```rs
fn set_content(&self, uri: Uri, content: String) {
  let key = QueryKey::ContentOf(uri);
  self.content_input.insert(key.clone(), content);
  let old_revision = self.revision.fetch_add(1, Ordering::SeqCst);
  self.colors.mark_red(key, old_revision + 1);
}
```

We mark our content red and incrementing our revision, informing our engine to update any queries that depend on content.
Our other half, `content_of`, returns the set content for a URI:

```rs
fn content_of(
  &self, 
  uri: Uri
) -> String {
  let key =
    QueryKey::ContentOf(uri.clone());
  if let Some(parent) = &self.parent {
    self.dep_graph
        .add_dependency(
          parent.clone(),
          key.clone());
  }
  self.db.colors.mark_green(
    key,
    self.db
        .revision
        .load(Ordering::SeqCst));
  self
    .db
    .content_input
    .get(&QueryKey::ContentOf(uri))
    .map(|r| r.value().clone())
    .expect("Uri was queried with unset value")
}
```

`content_of` does not call out to `query`.
It doesn't have dependencies and will never need to update its cache.
We still want to track dependencies for any parent query, if applicable.
But we always mark our content query green and get its value directly from cache.

## Blossoming LSP Implementation

Our compiler passes gave us a taste of writing queries.
We're going to move on to write queries that do more IDE-like functionality and actually implement our LSP.
Starting with a hallmark of interaction: Diagnostics.
A staple of IDEs.
Diagnostics are the little red squiggles that appear in our editor when we type the wrong code.

Each of our frontend passes produces a list of errors.
Those errors, however, are not yet diagnostics.
For us, diagnostics refers specifically to the LSP concept of a [diagnostic](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic).
An LSP diagnostic is a span in a source file and a human legible message.
`tower-lsp` provides just such a type for us: [`Diagnostic`](https://docs.rs/lsp-types/0.94.1/lsp_types/struct.Diagnostic.html#method.new_simple).
If we hammer our error into that shape, we can bundle them up in a response and `tower-lsp` takes care of the rest.

We create a new query `diagnostics_of` that collects all our errors and turns them into diagnostics:

```rs
fn diagnostics_of(&self, uri: Uri) -> Vec<Diagnostic> {
  self.query(
    QueryKey::DiagnosticsOf(uri),
    &self.db.diagnostics_query,
    |this, key| {
      let QueryKey::DiagnosticsOf(uri) = key else {
          unreachable!()
      };
      let newlines = this.newlines_of(uri.clone());
      this
        .errors(uri.clone())
        .map(|err| match err {
          todo!("a truly gargantuan match")
        })
        .collect()
    })
}
```

It is common we'll want to get all our errors, so we have a helper for it `errors`:

```rs
fn errors(
  &self, 
  uri: Uri
) -> impl Iterator<Item = PellucidError> {
  let types =
    self.types_of(uri.clone());
  let nameres =
    self.nameresolve_of(uri.clone());
  let desugar =
    self.desugar_of(uri.clone());
  let (_, parse_errors) =
    self.cst_of(uri.clone());
  types
    .errors
    .into_iter()
    .chain(nameres.errors)
    .chain(desugar.errors)
    .chain(parse_errors)
}
```

We map overall our errors, and convert them into diagnostics.
This is done as a gigantic match expression, where each case turns produces a human readable string and a span from our error.
We won't cover every case.
They get rote very fast.
A look at how we handle type errors will give us the big ideas:

```rs
PellucidError::Types(types) => {
  let desugar = this.desugar_of(uri.clone());
  let range = newlines
    .lsp_range_for(desugar.ast_to_cst[&types.node].ptr.text_range().into())
    .expect("error span outside range");

  Diagnostic::new_simple(
    range,
    todo!("match on `types.mark` to create a string")
  )
}
```

Our type errors are reported in terms of `NodeId`.
A meaningless concept to our client.
We use `ast_to_cst` from desugaring to turn our `NodeId` into its CST node.
Our CST node has a byte range, LSP works in terms of line/column ranges.

We'll need to convert our byte range to a line and column range.
`newlines_of` helps us do just that.
It processes our source text and finds all the newlines, building a mapping between bytes and line/columns.
Once built, we just have to ask it for the line/columns of our byte range via `lsp_range_for`.

With range at hand, we construct our diagnostic with `new_simple`.
LSP diagnostics support _a lot_ of features.
We, however, do not.
`new_simple` is a convenience constructor that passes reasonable defaults for all the fields of `Diagnostic`.
Leaving us with the task of providing a range and a string message.
We produce a message by matching on `types.mark`:

```rs
match types.mark {
  TypeError::AppExpectedFun {
    inferred_ty,
    expected_fun_ty,
  } => format!(
    "types: Expected this to be a function {} but it has type {}",
    prettyprint_ty(&expected_fun_ty),
    prettyprint_ty(&inferred_ty)
  ),
  // Our other cases...
}
```

We provide a human readable message for our `AppExpectedFun` and pretty print our types.
Pretty printing handles adding parenthesis, replacing numeric IDs with fancy Greek letters, etc.

{{< notice note >}}
For informational purposes, we tag our diagnostic "types:".
This is just so it's easy to see where diagnostics come from in the playground.
A real implementation doesn't have to do this (but it can if you like it!).
{{</ notice >}}

Our other cases are similar, so we'll save ourselves some time.
With our query covered we need a server to run it on request.

## We Need a Language Server

Something has to tell our query engine when to run our snazzy `diagnostics` query.
For us that's our language server.
There is a lot that goes into a server we won't cover, leaving it behinds closed doors in the `tower` ecosystem.
But you've stuck with me this far.
You deserve to know how our server is implemented.

It cannot be overstated how much `tower-lsp` is doing for us here.
All we have to do on our end is provide an implementation of their `LanguageServer` trait.
We aren't even going implement all of it.
To implement a trait, we'll need a type:

```rs
struct PellucidLsp {
  client Client,
  database: Arc<Database>,
  dep_graph: Arc<DepGraph>,
}
```

`PellucidLsp` represents our server.
Anything it stores lives and dies with our server.
We stow our `database` and `dep_graph` in it, so that our query state lives as long our server lives.

`Client` is a `tower-lsp` type.
It represents the client that's connected to our server, letting us push information to the client independent of responding to requests.
We implement `LanguageServer` for our struct:

```rs
impl LanguageServer for PellucidLsp {
  async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> { ... }

  async fn shutdown(&self) -> Result<()> { ... }
}
```

`LanguageServer` only requires two methods of an implementation: `initialize` and `shutdown`.
`initialize` is sent upon a client connecting to our server.
It includes parameters that configure things like workspace directory, capabilities supported by the client, etc.
Our server responds in kind, providing what capabilities, character encodings, etc. it supports.
`initialize` acts like a handshake, getting the client and server to agree to the common subset of the LSP they support.
`shutdown` acts more like a callback, allowing us to do any cleanup we need to do before our server exits.

That's all we before we implement our first LSP request, diagnostics.
Clients send a diagnostics request when they want the diagnostics for a file.
Let's see the code:

```rs
async fn diagnostic(
  &self,
  params: tower_lsp_server::lsp_types::DocumentDiagnosticParams,
) -> Result<DocumentDiagnosticReportResult> {
  let ctx = self.root_query_context();
  let diags = ctx.diagnostics_of(params.text_document.uri);
  if diags.is_empty() {
    return Ok(DocumentDiagnosticReportResult::Report(
      RelatedUnchangedDocumentDiagnosticReport {
        related_documents: None,
        unchanged_document_diagnostic_report: 
        UnchangedDocumentDiagnosticReport {
          result_id: "unchanged".to_string(),
        },
      }
      .into(),
    ));
  }
  Ok(DocumentDiagnosticReportResult::Report(
    RelatedFullDocumentDiagnosticReport {
      related_documents: None,
      full_document_diagnostic_report: FullDocumentDiagnosticReport {
        result_id: None,
        items: diags,
      },
    }
    .into(),
  ))
}
```

Our first two lines produce our list of diagnostics from our query engine.
The remainder is more hammering to get those diagnostics into the shape of an LSP response.

`root_query_context` is a helper that constructs a `QueryContext` with no parent, providing access to our `diagnostics_of` query.
Our query's diagnostics must become a `DocumentDiagnosticReportResult`.
A mouthful that amounts to our list of diagnostics embedded within a JSON message.
It exists to enable fancy features we won't support (yet): partial diagnostics, related files, persistence between requests, etc.

We do make use of one feature it provides.
When our list of diagnostics is empty, we return an unchanged report, rather than a report containing the empty list.
This informs our client it doesn't need to do anything.

With that, our client can requests diagnostics at their leisure.
But that's actually not an ideal workflow.
I mean we're the compiler here, or at least the server wrapping the compiler.
Why must the client ask us when something is wrong.
**We** know when something is wrong!

`diagnostics` is one way we provide diagnostics, but not the main way.
The main way diagnostics get reported is through two other LSP requests: `did_open` and `did_change`.
Both act as notifications to our server, requiring no response.

* `did_open` notifies us when our client opens a file.
* `did_change` notifies us when an open file's contents are changed.

When our client opens a file, we record it's content in our database:

```rs
async fn did_open(&self, params: DidOpenTextDocumentParams) {
  let uri = params.text_document.uri;
  self
    .database
    .set_content(uri.clone(), params.text_document.text);
  let ctx = self.root_query_context();
  let diags = ctx.diagnostics_of(uri.clone());
  self
    .client
    .publish_diagnostics(uri, diags, Some(params.text_document.version))
    .await;
}
```

We can see in that last line that we take this opportunity to sneak in a "response" through `Client`.
`publish_diagnostics` lets us push diagnostics to the client, rather than waiting for them to request them.
We use it when a file is opened and when a file is changed:

```rs
async fn did_change(
  &self, 
  params: DidChangeTextDocumentParams
) {
  let uri = params.text_document.uri;
  let ctx = self.root_query_context();
  let newlines = ctx.newlines_of(uri.clone());
  let mut content = ctx.content_of(uri.clone());
  // Update our content using params...
  self.database.set_content(uri.clone(), content);
  let diags = ctx.diagnostics_of(uri.clone());
  self
    .client
    .publish_diagnostics(uri, diags, Some(params.text_document.version))
    .await;
}
```

We skip over actually applying our changes.
Our playground always updates the entire file contents, but the `did_change` request supports partial updates, so our implementation supports partial updates, complicating the logic more than we need to care about right now.
The part we want to focus on is the overarching flow:

  * Update content
  * Set new content in the database
  * Query diagnostics
  * Public diagnostics

These two avenues are the main lanes that drive diagnostics to the client whenever a file is changed.

## The Heart of a Language Server

Diagnostics gave us an introduction into implementing requests for our LSP.
We've got all the tools we need now to start doing the really cool stuff.
We're going to implement five critically acclaimed LSP features:

  * Goto Definition
  * References
  * Rename
  * Hover
  * Autocomplete

All of these functions rely on two core queries: `syntax_node_starting_at` and `ast_node_of`.
Together these two queries form [the Heart of a Language Server](https://rust-analyzer.github.io//blog/2023/12/26/the-heart-of-a-language-server.html).
As we can see from how many queries they subsist.

`syntax_node_starting_at` handles turning a line/column range into a CST node, and `ast_node_of` turns a CST node into an AST node. 
We always call them in tandem, but we keep them as separate queries for caching.
Multiple line columns might all ultimately reference the same syntax node.
Keeping `ast_node_of` as it's own query means we only cache the AST node once for that syntax node and not once per line/column.

## Turning Spans into Syntax Nodes

Let's look at the code for `syntax_node_starting_at`.
We begin with our `QueryKey` entry:

```rs
enum QueryKey {
  // ...
  NodeStartingAt(Uri, Position),
  // ...
}
```

Unlike our other keys, this query does not process the entire file.
It takes a file and a position in that file (which is LSPs way of spelling line and column).
A query cache will have more than one entry, finally.
I trust you can guess what our database field looks like.
We'll jump straight to our query:

```rs
pub fn syntax_node_starting_at(
  &self, 
  uri: Uri, 
  cursor: Position
) -> Option<SyntaxNodeHandle> {
  self.query(
    QueryKey::NodeStartingAt(uri, cursor),
    &self.db.node_starting_at_query,
    |this, key| {
      todo!("Implement me")
    },
  )
}
```

We're, hopefully, comfortable with the general shape of queries.
The action starts in our producer where we have the logic for turning a line and column into a handle for our CST node:

```rs
let QueryKey::NodeStartingAt(uri, cursor) = key else {
  unreachable!("starting")
};
let (green, _) = this.cst_of(uri.clone());
let cst = SyntaxNode::<Lang>::new_root(green.clone());
let newlines = this.newlines_of(uri.clone());
let byte: u32 = newlines
  .byte_of(cursor.line, cursor.character)?
  .try_into()
  .unwrap();
```

Our preamble gathers all the context we'll need.
`cst`, understandably, holds all our CST nodes.
`newlines_of` also takes care of converting an LSP line and column into a byte offset (It does both actually. I was here yesterday, and it actually goes both ways).
We use `byte` to lookup a token in our `cst`:

```rs
let token = cst.token_at_offset(TextSize::from(byte));
```

This innocuous line is doing quite a bit of work under the hood, traversing our tree with ruthless efficiency to grasp the token at our offset.
Fortunately, we put `rowan = 0.16` in our TOML file, so we're moving along.
We're not immediately done with our token.
Our offset might have landed us perfectly between two tokens (you never know what these users will think up next).
In such a case, we do our best to pick a meaningful token:

```rs
let token = match token {
  parser_base::rowan::TokenAtOffset::None => return None,
  parser_base::rowan::TokenAtOffset::Single(token) => token,
  // Bias away from whitespace as it's unlikely to be what we want.
  // Bias towards identifiers, as they're more likely to be semantically interesting.
  // Othewrise choose the left one
  parser_base::rowan::TokenAtOffset::Between(left, right) => {
    match (left.kind(), right.kind()) {
      (_, Syntax::Whitespaces) | (Syntax::Identifier, _) => left,
      (Syntax::Whitespaces, _) | (_, Syntax::Identifier) => right,
      _ => left,
    }
  }
};
```

Given the choice of two tokens, we favor identifiers and disfavor whitespace.
Whitespace is unlikely to be what our user wanted to interact with, so we can save ourselves some trouble by filtering it here.
Conversely, identifiers are exactly the type of token are users are looking for.
A quick run through of our operations convinces us why this might be:

  * Hover works on identifiers (and maybe numbers but it's certainly less interesting)
  * References only work on identifiers
  * Goto definition only works on identifiers
  * Rename only works on identifiers
  * Completion only works on identifiers (Left as an exercise for the reader to autocomplete numbers)

Really makes you wonder why we've got the other tokens at all.
All the semantic payoff rests in identifiers.
Finally, we turn our token into a syntax node handle:

```rs
let node = token.parent()?;
Some(SyntaxNodeHandle {
  root: green,
  ptr: SyntaxNodePtr::new(&node),
})
```

We've searched up a token, but we actually want the node wrapping our token, so we walk up one level.
Our node is turned into a handle that serves as our result.

{{< notice note >}}
Recall from desugaring, `rowan` uses pointers under the hood for fast traversal, which are not safe to store long term.
Instead, we store the root node of our CST and an index into that tree, confusingly named `SyntaxNodePtr`.
{{</ notice >}}

## Turning Syntax Nodes into AST Nodes

`ast_node_of` takes a handle and a file URI, as we can see from its query key:

```rs
enum QueryKey {
  AstNodeOf(Uri, SyntaxNodeHandle),
}
```

It uses them to find the corresponding AST node in our typed AST: 

```rs
fn ast_node_of(
  &self, 
  uri: Uri,
  node: SyntaxNodeHandle
) -> Option<Ast<TypedVar>> {
  self.query(
    QueryKey::AstNodeOf(uri.clone(), node),
    &self.db.ast_node_query,
    |this, key| {
      todo!("Implement me")
    })
}
```

Nothing surprising here.
Our return type is `Ast<TypedVar>`.
Technically, we could return any `Ast` from desugar, nameres, or types.
We select types because it has the most information, containing both resolved names **and** types.
If we need one of our other `Ast`s, we can always find it via `NodeId`.

Our implementation is pleasingly short:

```rs
let QueryKey::AstNodeOf(uri, node) = key
else {
  unreachable!("ast");
};
let desugar = this.desugar_of(uri.clone());
let id = desugar
  .ast_to_cst
  .iter()
  .find_map(|(id, sync_node)|
    (sync_node == node).then_some(id))?;

let types = this.types_of(uri.clone());
types.ast.find(*id).cloned()
```

We iterate over `ast_to_cst`, looking for `sync_node`'s entry and return the corresponding `NodeId`.
We use `id` to find our `Ast` node in our typed tree.
`find` does a depth-first traversal of our tree, returning the matching node, if found.
That completes our two queries and the heart of our LSP.
Now we start on building our LSP features.

## Goto Definition

Have you ever read a variable and wondered, "Hang on, what's this mean again?".
Goto Definition has the answers you seek.
When we have our cursor over an identifier, goto def jumps us to the definition of that identifier.
For our language, that means either a let binding or a function parameter.
I'm sure you can imagine other cases that arise for languages with more involved definitions.

Since this is an LSP feature, it gets a request:

```rs
async fn goto_definition(
  &self,
  params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
  let ctx = self.root_query_context();
  let uri = params.text_document_position_params.text_document.uri;

  let Some(range) = ctx.definition_of(
    uri.clone(),
    params.text_document_position_params.position
  ) else {
    return Ok(None);
  };
  Ok(Some(GotoDefinitionResponse::Scalar(
    tower_lsp_server::lsp_types::Location {
      // This could be different once we have different files,
      // but for now uri is always the same.
      uri,
      range,
    },
  )))
}
```

We work purely in terms of source text ranges.
Our response provides the range of the definition and trusts the client to handle displaying it.
Our request receives a line and column in source for the client's cursor.
The query `definition_of` turns that into the range of the definition:

```rs
fn definition_of(
  &self, 
  uri: Uri,
  cursor: Position
) -> Option<LspRange> {
  self.query(
    QueryKey::DefinitionOf(uri.clone(), cursor),
    &self.db.definition_query,
    |this, key| {
      todo!("Implement me")
    },
  )
}
```

We start by getting the AST node at our cursor, using our one-two combo of queries:

```rs
let QueryKey::DefinitionOf(uri, cursor) = key else {
  unreachable!();
};
let syntax = this.syntax_node_starting_at(uri.clone(), cursor)?;
let ast_node = this.ast_node_of(uri.clone(), syntax)?;
let Ast::Var(node_id, var) = ast_node else {
  return None;
};
```

If our cursor isn't on a variable, there's no definition to go to and we're done.
From our variable, we want to find the nearest AST node that defines it.
Sounds like a job for name resolution:

```rs
let ast =
  this.nameresolve_of(uri.clone()).ast;
let binder_id = ast
  .parents_of(node_id)?
  .into_iter()
  .find_map(|ast| match ast {
    Ast::Fun(node_id, bind, _) if bind == &var.0 => Some(node_id),
    _ => None,
  })?;
```

We walk the parents of our AST node, looking for a function that defines it.
We're walking the name resolved AST, so we can be confident our variable is defined uniquely somewhere.
If we don't find one, technically an impossible case, we bail.
We need to turn our defining AST node back into a CST node:

```rs
let ast_to_cst = this.desugar_of(uri.clone()).ast_to_cst;
let binder_node = ast_to_cst.get(binder_id)?;
let root = SyntaxNode::new_root(binder_node.root.clone());
let syntax = binder_node.ptr.to_node(&root);
let mut binder = syntax;
if binder.kind() == Syntax::Fun {
  binder = binder.first_child_by_kind(&|kind| kind == Syntax::FunBinder)?;
}
```

We use our trusty `ast_to_cst` map for this.
In the case that our parent is a `Fun` node, we walk to the binder.
This provides a nicer experience by jumping us to the identifier rather than the `|` of the function.

{{<notice note >}}
We don't need to do this for let bindings because their function nodes already point at their `LetBinder` in `ast_to_cst`.
{{</ notice >}}

Our last step is to turn our CST node into an LSP range:

```rs
let range_node = binder.first_token()?;
let newlines = this.newlines_of(uri.clone());
let range = newlines.lsp_range_for(range_node.text_range().into())?;
Some(range)
```

With that we've implemented goto def.

## Hover

Hover shows semantic information when we mouse over interesting bits of our code.
A rather abstract definition, as hover can do a lot.
For us, hover means that when you mouse over an identifier, we show you its type.
Our LSP `hover` request keeps it brief:

```rs
async fn hover(
  &self, 
  params: HoverParams
) -> Result<Option<tower_lsp_server::lsp_types::Hover>> {
  let uri = params.text_document_position_params.text_document.uri;
  let position = params.text_document_position_params.position;
  let ctx = self.root_query_context();
  Ok(ctx.hover_of(uri.clone(), position.clone()))
}
```

The action all lives in `hover_of`.
We've seen enough queries by now, we're going to jump straight into the logic (but imagine it's wrapped in a `query` call).
`hover_of` begins with `syntax_node_starting_at`, just like goto def:

```rs
let QueryKey::HoverOf(uri, position) = key else {
  unreachable!("hover")
};
let cst = this.syntax_node_starting_at(uri.clone(), position.clone())?;
let syntax = SyntaxNode::<Lang>::new_root(cst.root.clone());
// We'll need this later to get the correct range for our hover, so we don't want to shadow
// it.
let cursor_node = cst.ptr.to_node(&syntax);
// We only want to show a hover if our cursor is over a variable, either in expression or
// bound position.
let node = match cursor_node.kind() {
  Syntax::Var | Syntax::LetBinder => cursor_node.clone(),
  Syntax::FunBinder => cursor_node.parent()?,
  _ => return None,
};
```

Unlike goto def, we do quite a bit more with our syntax node before turning it into an AST node.
Hovers can technically be attached anywhere.
When we produce a hover we give the client a range in the file where this hover should be displayed.
We keep around a copy of our original syntax node to get the source range where our hover tooltip should be attached.

Next, we check that we're looking at something hover-able.
A variable, bound or otherwise.
In the case that we're looking at a function parameter, we grab the parent node, which is why we save a copy of `cursor_node`.
We don't map `FunBinder` to an AST node, only `Fun`.
Now we find our AST node:

```rs
let ast_node = this.ast_node_of(
  uri.clone(),
  SyntaxNodeHandle {
    root: cst.root.clone(),
    ptr: SyntaxNodePtr::new(&node),
  },
)?;
let ty = match &ast_node {
  Ast::Var(_, typed_var) => &typed_var.1,
  Ast::Fun(_, typed_var, _) => &typed_var.1,
  _ => return None,
};
```

The AST node gives us our type, the content of our tooltip.
We complete our query by creating a [`Hover`](https://docs.rs/lsp-types/0.94.1/lsp_types/struct.Hover.html) from our CST node and type:

```rs
let newlines = self.newlines_of(uri.clone());
let range = newlines.lsp_range_for(cursor_node.text_range().into());
Some(Hover {
  range,
  contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
    language: "pellucid".to_string(),
    value: prettyprint_ty(ty),
  })),
})
```

We wrap our string up in all the layers to tell the LSP it's a plain string (as opposed to the many other kinds of string a hover could be).
We also specify it's for the language pellucid, but given our language didn't exist till today, I doubt any editors support it's syntax.

## References

References is kind of like the inverse of goto def.
Given a variable, show me all the locations it's used in code.
I don't have to be at the definition of the variable, of course.
We're free to get the references from any occurrence.
Our request is another thin wrapper around our query:

```rs
async fn references(
  &self,
  params: ReferenceParams
) -> Result<Option<Vec<Location>>> {
  let ctx = self.root_query_context();
  let uri = params.text_document_position.text_document.uri;
  let cursor = params.text_document_position.position;

  Ok(ctx.references_of(uri, cursor))
}
```

A `Location` is a URI paired with a line and column.
We're going to return one location per occurrence of the variable under our cursor.
The logic for our `references_of` query begins by ensuring we're looking at a variable:

```rs
let QueryKey::ReferencesOf(uri, cursor) = key else {
  unreachable!()
};
let sync_node = this.syntax_node_starting_at(uri.clone(), cursor.clone())?;
let ast_node = this.ast_node_of(uri.clone(), sync_node)?;
let var = match ast_node {
  Ast::Var(_, var) | Ast::Fun(_, var, _) => var,
  _ => return None,
};
```

We get all references to `var` from our AST:

```rs
let ast = self.nameresolve_of(uri.clone()).ast;
let vars = ast.var_reference(&var.0);
```

`var_reference` huh? A bit like drawing the rest of the owl.
`var_reference` does a depth first search of our AST, like `find`.
Unlike `find`, it returns every node that mentions our `Var`, not just the first one.
We need to turn our list of variables into a list of locations to complete our query:

```rs
let ast_to_cst = self.desugar_of(uri.clone()).ast_to_cst;
let newlines = self.newlines_of(uri.clone());
let references = vars
  .into_iter()
  .filter_map(|var| {
    let id = var.id();
    let sync_node = ast_to_cst.get(&id)?;
    let root = SyntaxNode::new_root(sync_node.root.clone());
    let mut syntax = sync_node.ptr.to_node(&root);
    if syntax.kind() == Syntax::Fun {
      syntax = syntax.first_child_by_kind(&|kind| kind == Syntax::FunBinder)?;
    }
    let token = syntax.first_token()?;
    let range = newlines.lsp_range_for(token.text_range().into())?;
    Some(Location {
      uri: uri.clone(),
      range,
    })
  })
  .collect();
Some(references)
```

We've seen this logic before in `hover`, look up the CST node, get its span, turn it into line and column, but we're doing it once per variable here.
Honestly, we probably could've made it a query.
Whoops.
Either way, Our job is done.

## Rename

Rename takes the variable under cursor and changes its name everywhere it occurs.
"Everywhere it occurs" sounds familiar.
And indeed, a look at our request confirms our suspicions:

```rs {hl_lines=[10]}
async fn rename(
  &self,
  params: RenameParams
) -> Result<Option<WorkspaceEdit>> {
  let ctx = self.root_query_context();
  let uri = params.text_document_position.text_document.uri;
  let cursor = params.text_document_position.position;
  let new_name = params.new_name;

  let edits = ctx.references_of(uri, cursor).map(|locs| {
    let mut changes = HashMap::default();
    for loc in locs {
      let edits = changes.entry(loc.uri).or_insert(vec![]);
      edits.push(TextEdit {
        range: loc.range,
        new_text: new_name.clone(),
      });
    }
    WorkspaceEdit {
      changes: Some(changes),
      ..Default::default()
    }
  });
  Ok(edits)
}
```

We don't actually need a new query for `rename`.
`references_of` gives us everything we need.
But, rather than return a list of locations, we return a `WorkspaceEdit`.
As the server in this LSP setup, we don't modify files directly.
We don't want to step on a user's toes by modifying the file out from under them.
Instead, we whip up some JSON that says what changes we think would be great, and ship them to the client to actually get made.

Here, that is replacing the old variable name with the new name at every range the old variable appears.
Rename was short and sweet.
You love to see a good abstraction come together.

## Autocomplete

Last, but certainly not least is autocomplete.
This is, in my humble opinion, the most prominent feature of the IDE experience.
As a user types, we plumb the codebase for anything they could possibly mean to type and present it to them as an option for avoiding further keystrokes.
What an elating experience to have the compiler understand what I need and hand it to me on a silver platter.

Autocompletion can get extremely fancy.
You can figure out the type that should slot into the current cursor and only present options of that type.
You can track the type of object our user is currently accessing and only present methods available on that object.
The more type information you can procure, the better you can tailor your recommendations.
We will save that for another tutorial.

Our autocomplete won't be that fancy, but still will be plenty useful.
Given a partially completed identifier, we figure out all the variables defined in scope at that location and offer them as completions. 
We don't even have to filter them down to identifiers prefixed by our partial input.
That's handled by the client.
Like our other requests, we call our query and return its results as our response:

```rs
async fn completion(
  &self,
  params: CompletionParams
) -> Result<Option<CompletionResponse>> {
  let ctx = self.root_query_context();
  let completions = ctx.completion_of(
    params.text_document_position.text_document.uri,
    params.text_document_position.position,
  );
  Ok(completions)
}
```

We built the whole query engine and dagnabit we're gonna use the whole query engine!
`completion_of` starts by immediately calling another query `scope_at`:

```rs
let QueryKey::CompletionOf(uri, cursor) = key else {
  unreachable!();
};
let scope = this
  .scope_at(uri.clone(), *cursor)?
  .into_iter()
  .collect::<Vec<_>>();
```

The subtlety of completions is that we only want to recommend identifiers that are in scope at the cursor.
We can't simply build the scope for our entire program and offer it up.
We'd end up recommending variables that aren't defined where the user is typing.
`scope_at` solves this for us by building our scope as seen at a specific position in the program:

```rs
fn scope_at(&self, uri: Uri, cursor: Position) -> Option<HashMap<String, String>> {
  self.query(
    QueryKey::ScopeOf(uri, cursor),
    &self.db.scope_query,
    |this, key| {
      todo!("Implement me")
    },
  )
}
```

It's implementation starts with our power duo:

```rs
let QueryKey::ScopeOf(uri, cursor) = key else {
  unreachable!()
};

let node = this.syntax_node_starting_at(uri.clone(), cursor.clone())?;
let ast_node = this.ast_node_of(uri.clone(), node)?;
```

We're going to walk the parents of our AST node to build our scope:

```rs
let desugar = this.desugar_of(uri.clone());
let types = this.types_of(uri.clone());
let scoped_ast = zip_ast(desugar.ast, types.ast);
let Some(parents) = scoped_ast.parents_of(ast_node.id()) else {
  return Some(HashMap::default());
};
```

Our eventual completions are in terms of the identifiers written in our source file.
But most of our ASTs work in `Var`, which does not make for a very good completion.
We bridge this gap by zipping our desugared AST and our typed AST together.
This gives us an `Ast<(String, TypedVar)>` that contains both human readable variable name and the type.

We walk our parents in the zipped AST.
If we don't have any parents, we must be at the root of our AST and nothing is defined in scope.
We finish by collecting our in scope bindings into a hashmap:

```rs
let scope: HashMap<String, String> = parents
  .into_iter()
  .rev()
  .filter_map(|ast| match ast {
    Ast::Fun(_, (name, typed_var), _) => 
      Some(( name.clone()
           , prettyprint_ty(&typed_var.1)
           )),
    _ => None,
  })
  .collect();
Some(scope)
```

It's important we walk them in reverse order.
This order ensures the nearest bindings comes last, shadowing any previous bindings in our hashmap.
Maintaining proper shadowing matters here because we're working in user defined variable names, not `Var`.
Back in `completion_of` we use our `scope` to construct an array of completions:

```rs
Some(CompletionResponse::Array(
  scope
    .into_iter()
    .map(|(name, pretty_ty)| CompletionItem {
      label: name,
      kind: Some(CompletionItemKind::VARIABLE),
      detail: Some(pretty_ty),
      ..Default::default()
    })
    .collect(),
))
```

Our completions are done. We've implemented an LSP!

## A Little White Lie

Implemented an LSP...mostly.
If you look at the source code for our LSP, you'll find quite a bit of code that isn't covered here. 
That's not because our tutorial is hiding LSP implementation from you.
I needed quite a bit of code to hook up the LSP to the [playground](TODO) and didn't have an easy way to separate it out from the LSP.
Feel free to peruse it and see how it exploits the LSP protocol to make the playground work, but we won't be covering it here.

## A Moment for Reflection

I can't overstate how happy I am to hit this milestone.
Our language is modest, but we've implemented an LSP that works and you can interact with end to end.
That's nothing to sneeze at.
I really appreciate you coming along with me for this journey.
When I started, my plan was just to write a little post about type inference.
Once I wet my appetite, however, I kept going and was desperate to reach something that could be called "complete".

This meets that milestone.
Hopefully you've found the explanations along the way educational.
There is still plenty more to be done.
Our language lacks top level functions, data types, modules, on and on.
But before setting off after those features I will probably take a pause to do some writing about other things.
If you look at my post history this year, it's almost exclusively making a language posts.
I'd like to frolic for a bit before undertaking writing every pass for another feature (I'm looking at your rows or items).
Eventually, I'd like to return to making a language and cover how we can build on this base to start doing some really fancy stuff with more complicated features.
