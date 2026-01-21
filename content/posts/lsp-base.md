+++
title = "Making an LSP for great good"
date = "2025-12-29T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "LSP"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Language Server Protocol", "Query Based", "Incremental Computation"]
description = "Implementing an LSP from our query-based compiler passes"
+++

{{< accessory title="Making a Language" >}}
This post is part of the [making a language series](/series/making-a-language).
A series that teaches you how to implement a programming language using Rust.

This post brings together all our passes to build a language server.
It mainly relies on the output of the frontend passes (parsing, desugar, nameres, and types).
{{</ accessory >}}

We've talked a big game about resilience this and interactive that.
With [name resolution](/posts/nameres-base) done, it's finally time to put our money where our mouth is.
It's time to tie our passes together into a compiler, but not just any compiler.
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
We're building a [query-based compiler](https://ollef.github.io/blog/posts/query-based-compilers.html).
Atop that, we'll implement a language server for the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) (LSP).
You can see it all come together in the interactive [playground](/making-a-language).

For the uninitiated, LSP was invented to solve an [M:N problem](https://matklad.github.io/2022/04/25/why-lsp.html) between editors and languages.
There are many editors (Vim, VSCode, Helix, etc.) and there are many languages (in fact we're adding to that problem right now).
We want great IDE features in each of our editors, but implementing them once per language per editor is a combinatorial explosion of work.

LSP saves us by introducing a unifying middleman.
Each language implements an LSP server once.
Each editor implements an LSP client once.
Now any language with a server works with any editor with a client.

{{< notice note >}}
Technically what we're implementing is a language server for LSP, but we'll refer to it as an LSP throughout this article.
This is a common shorthand, but we're not making a protocol.
That'd be crazy.
{{</ notice >}}

Our server provides some of your favorite IDE features such as: semantic information on hover, autocomplete, goto definitions, the works.
Clients will connect to our server and send us requests for these features.
Our server chews through compilation to produce a JSON response that the client is responsible for rendering.

While our end result is an LSP, it's only a means to an end.
Our actual goal is to create an incremental query-based compiler.
An LSP just happens to perfectly present the advantages of such a compiler.
Plus it's really cool to see the compiler doing it live as we interact with the code.

A query based compiler is the exact inverse of our batch compiler example above.
Rather than one big pipeline shuffling data from start to finish, compilation is structured as a series of questions (queries if you will).
Queries can depend on each other.
We'll have our final query "what does the WebAssembly for this source code look like?".
En route to answering that query, our compiler will ask a bunch of subqueries:

  * What does the IR for this source look like?
  * What do the types for this source look like?
  * What does the CST for this source look like?

And so much more!
The advantage of this model, over the traditional pipeline, is its ability to answer queries incrementally.
We can ask "what's the type of this variable" and our compiler has to do the work to determine the type of that variable, but only that work.
And queries are super cacheable.

Once we answer the query, "What does the CST for this source look like?" we can save that result and reuse it to answer anybody else who asks.
Even better, when we change our source code, we only have to recompute queries that depend on that change.
If a query depends on an unchanged part of the code, we can just keep using the cached value.

This is especially pertinent for an LSP.
Requests need to be answered as fast as possible, computing our answers incrementally lets us respond quickly.
We might receive any number of requests without any source code changes.
Imagine a user jumping around their codebase by hitting the goto definition key multiple times.
We reuse our cached query results across those jumps and only recompute things once our source code changes.

Our path forward is clear;
Implement a query-based compiler and a language server.
Let's talk about what we're not going to do.
Our focus will be on the compiler and how it fulfills requests for our LSP.
But building a server still involves a lot of server-y stuff: HTTP headers, JSON marshaling, IO, etc.
We won't be touching on any of that at all.
We use the [`tower-lsp`](https://crates.io/crates/tower-lsp) crate, and it handles all that for us.

We won't even cover all the protocol part of LSP.
LSP is a gigantic [spec](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/).
I doubt any server implements every request LSP supports.
We will implement a meaningful, but small, subset of LSP.

## Queries

Our sights are set.
Summiting compilation requires a language server.
Before we can implement our language server, we need some compiler queries.  
Before we can write some compiler queries, we need something that executes queries.  
Before something can execute queries, we need to know what a query is.

A query takes in some inputs, runs some code, and produces an output, which sounds indistinguishable from functions.
What sets a query apart, however, is the result caching, and incremental updating.
As a side effect of this caching, our queries must be pure (i.e. side-effect free).
Caching is key.
We only want to run a query when we know it will produce a fresh value.
In an ideal world, most queries are cached most times.

Queries depend on each other to form a graph, much like a call graph for functions.
But this graph, unlike functions, must be a Directed Acyclic Graph (DAG).
It can't contain cycles.
If a query could end up as it's own input, we'd suddenly have a very hard time telling when its inputs haven't changed.

Most of our queries follow the same logic:

* Check inputs
* If they haven't changed, use the cached value
* If they have changed, run its body and cache the value

But our queries form a DAG, so some of them must have no inputs.
These independent queries are called input queries.
Every non-input query derives it's value form it's inputs, so input queries are the only way we can introduce new information into the graph.
Input queries provide setters.
Whenever an input is set to a new value, that notifies us we need to update any queries that depend on that input.

## Query Engine

Something has got to be responsible for wrangling our query graph and its inputs.
That role rests on the query engine.
The query engine drives executing queries and stores all the state that requires.
It has three responsibilities:

* Execute queries
* Cache query results
* Track query dependencies

As we execute queries, our engine tracks each query's dependencies.
Once our query produces a result, our engine handles caching it.
On the happy path, our engine determines our query don't need to execute at all and returns the cached value.

Our query engine does a lot for us, but the realm of query engines runs deep.
For simplicity, we won't be covering everything a production query engine would want:

* Persisting caches to disk (check out [rustc docs](https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation-in-detail.html#the-real-world-how-persistence-makes-everything-complicated) for details)
* Garbage collection (i.e. Cache Eviction)
* Cycle Detection

All the above are left on the cutting room floor.
Our engine also has quite a bit of boilerplate.
If we're doing it right, this makes it easy to understand how the query engine operates.
In a real implementation, a lot of what we do can be wrapped up with macro magic to save everyone a lot of time.
Heck, once you know how a query engine works, consider an off the shelf solution like [salsa](https://crates.io/crates/salsa).

## Red-Green Algorithm

The efficacy of our query engine lies in its deftness at determining when a query does not need to be run.
We'll need an algorithm to determine when we can skip query execution.
We borrow a tried and true algorithm from rustc: the [red-green algorithm](https://salsa-rs.github.io/salsa/reference/algorithm.html).

The red-green algorithm associates each query with a color, red, or green.
When a query is green we're safe to reuse it's cached value.
When a query is red, we need to reexecute it and update it's cache.

{{< notice note >}}
We say query here, but we really mean a query with a particular set of arguments.
We'll have a query `cst_of(Uri)` that takes in a file URI and outputs a parse tree.
That query's cache will have an entry per file URI we parse, and each entry can be red or green.
{{</ notice >}}

All queries start red.
Upon execution, they're marked green.
When a query's input changes, we mark it red again.
Before our engine executes any query, it checks the colors of that query's dependencies.
If all of them are green, we can eschew our run and use our query's cached value.
We know it's safe to skip our run because our queries are pure, relying only on their inputs to determine their output.

When any dependency is red, our engine executes our query.
Our engine doesn't mark our query red, yet.
We first compare our new value with the cached value.
If our execution produced an equal value, we leave our query green.
We only mark our query red when the values are different.

This is more important in practice than it might seem.
Imagine a scenario where we make a trivial change in our source code, changing whitespace, adding a comment, etc.
We have to rerun our parsing query.
Upon desugaring, however, we produce the same AST, marking our desugaring query green.
Any query that depends on desugaring no longer needs to run.
We've saved ourself from executing the remainder of our passes.

With exposition out of the way, we can begin looking at some actual code.
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
Tracking requires identifying.
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

Each variant represents a query and its arguments.
We can see `content_of`, `cst_of`, and `newlines_of` all take a single argument.
This is because they all work on one file, represented by the path to that file as a `Uri`.
Once we get to our LSP features, we'll see some more granular queries that take, for example, a specific location within a file.

{{< notice note >}}
Our enum is straightforward but has some downsides (mostly around boilerplate).
It gets the job done and makes it easy to see what's going on.
But you can reduce boilerplate by using a fancier solution like a trait.
See rustc's [`QueryConfig`](https://doc.rust-lang.org/stable/nightly-rustc/rustc_query_system/query/trait.QueryConfig.html) for an example.
{{</ notice >}}

We'll see `QueryKey` all over as the key of hashmaps or the vertices of our dependency graph.
It's also the `parent` field of our `QueryContext`, where it tracks the parent of the query we're currently executing.
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
We need it because we're building a server that handles requests asynchronously.
Dashmaps can be thought of as `RwLock<HashMap<K, V>>`.
Their implementation differs for performance, but it's a helpful mental model.

Alongside the caches, our database stores two important pieces of information: our color map and revision.
The color map stores the color of each query, as seen on the red-green algorithm.
The revision is a counter that gets updated whenever we set an input query.
It forces us to check green queries that ran on a previous input and make sure they're still up to date.

Our final field `dep_graph` tracks query dependencies.
Whenever one query executes another query, we write that down in our dependency graph:

```rs
struct DepGraph {
  graph: RwLock<DiGraph<QueryKey, ()>>,
  indices: DashMap<QueryKey, NodeIndex>,
}
```

It offloads storing the graph to `DiGraph` type (which is a DAG) from the [`petgraph`](https://docs.rs/petgraph/0.8.3/petgraph/) crate.
It's critical we have _a_ dependency graph.
It's not critical _how_ that dependency graph works.
Feel free to dig into [the code](https://github.com/thunderseethe/making-a-language/tree/main/lsp/base), if you're interested.
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
It's a lot to take in, but once we understand `query`, implementing our queries on top of it consists of writing normal functions.
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
Our humble engine will just pass it manually.

`producer` houses the logic of our query.
We run `producer` when we can't use our cached value and need to _produce_ a new one.
It takes a `QueryContext`, so that it can call its own queries.
It takes `QueryKey`, so that it has access to our query arguments.

From there, `query`'s implementation begins by defining a helper `update_value`:

```rs
let revision = self.db.revision.load(Ordering::SeqCst);
let update_value = |key: QueryKey| {
  todo!("we'll see what this in a sec")
};
```

At multiple points throughout `query` we might determine we need to update our query's value.
`update_value` shares that logic in one place.
Updating a query value comprises three tasks.
First, we add an edge to our dependency graph:

```rs
if let Some(parent) = &self.parent {
  self.dep_graph.add_dependency(parent.clone(), key.clone());
}
```

Whenever we execute a query, and only then, we record it in the dependency graph.
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

The critical bit here is that we update the parent of `QueryContext` when we run `producer`.
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

If our value hasn't changed, we mark our query green, successfully saving work!
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

Looping over the dependencies recorded in our graph, we return early with red if any of our dependencies are red.
There's a case, noted by our `todo!`, where our dependency query is absent from our color map, or has a stale revision.
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

Upon succeeding we're done, our dependency is green.
Failing that, we still try to mark our query green by just running it and seeing what happens.
Running the query handles the case where our produced value is equal to our cached value.
`run_query` is a helper that dispatches a call to the appropriate query based on `QueryKey`, throwing away the result.

That's everything `query` does.
Our query engine may be rudimentary but it is complete.
We understand how a query engine works now.
Our engine will execute queries incrementally and, once we write some queries, even compile code!

 
## Compiler Passes

I loved learning about queries, but it didn't actually have a lot to do with compilers.
Our queries could compute anything: UI, Game State, an ETL pipeline.
All my [general-purpose incremental computation](https://en.wikipedia.org/wiki/Incremental_computing) heads sound off in the comments.
And I love that for them, but we're here to build a bloody compiler.
It's time to talk about the queries that compile code.

At minimum, we'll need a query for each pass of our compiler (i.e. parsing, type inference, closure conversion, etc.).
Each pass query takes a similar shape.
Every pass contains a top level function as it's entry point (almost like it was designed for querying).
We wrap that function in a call to `query` that performs any dependency queries needed.

We'll walk through what that looks like for our parser query, `cst_of`, to get the idea.
`cst_of` produces the parse tree for an entire file.
First we add an entry to `QueryKey`:

```rs
enum QueryKey {
  // ...
  CstOf(Uri),
  // ...
}
```

Next, database gets a field `cst_query` to hold our cached values:

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

We see that pesky boilerplate we've been talking about.
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
But still we must convince the compiler of that with `unreachable!`.
Boilerplate due to our enum.
A more sophisticated solution would prevent this, but we'll manage, _somehow_.

The remainder of our logic is pure.
We ask for the content of the file with `content_of`.
Under the hood, this is another query and tracks that `cst_of` depends on `content_of`, but we're free to ignore that detail here.
We pass our content to `parse` and then return its results as our queries results.
Our parser works in a specific `ParseError` type, so we wrap that up in a `PellucidError::Parser` variant.

We follow that pattern for all of our passes.
The specific function may change but the formula persists.
Create a query key, add a database field, and wrap our function in a `query` call.

Let's look at one more pass, but faster, to make sure we get the gist.
We jump deeper into the compiler to look at `simple_ir_of`, which optimizes the Intermediate Representation (IR) produced by lowering.
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
Hard crashing, however, does not a great server experience make.
We'd like our queries to be callable on invalid input, even if the underlying passes are not.
We bridge that gap by wrapping the results of our query in `Option`.
If we have errors in the frontend of the compiler, we simply don't call the underlying passes after type inference.

The queries for our other compiler passes can be found in the [full source code](https://github.com/thunderseethe/making-a-language/tree/main/lsp/base).
`wasm_of` breaks the mold a bit to work around our lack of top level items, but otherwise they all follow this pattern.

## Our Sole Input Query

With one exception, our sole input query: `content_of`.
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

We mark our content red and increment our revision, informing our engine to update any queries that depend on content.
Our query's other half, `content_of`, returns the set content for a URI:

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

`content_of` does not contain a call to `query`.
It doesn't have dependencies and will never need to update its cache.
We still want to track dependencies for any parent query, however.
But we always mark our query green and get its value directly from cache.

## Blossoming LSP Implementation

Our compiler passes gave us a taste of writing queries.
We're going to move on to write queries that do more IDE functionality and actually implement an LSP.
Starting with a staple of IDEs: Diagnostics.
Diagnostics are the little red squiggles that appear in our editor when we type the code wrong.

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

Collecting all our errors is common enough to warrant a helper `errors`:

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

We map over each error, and convert it into a diagnostic.
Conversion is done by a gigantic match expression, with each case producing a human readable string and a span from our error.
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
We use `ast_to_cst`, from desugaring, to turn our `NodeId` into its CST node.
Our CST node has a byte range, but LSP works in terms of line/column ranges.

We'll need to convert our byte range to a line and column range.
`newlines_of` helps us do just that.
It processes our source file and finds all the newlines, building a mapping between bytes and line/columns.
Once built, we just have to ask it for the line/columns of our byte range via `lsp_range_for`.

With range at hand, we construct our diagnostic with `new_simple`.
LSP diagnostics support _a lot_ of features.
We, however, do not.
`new_simple` is a convenience constructor that passes reasonable defaults for all the fields of `Diagnostic`.
Leaving us with the task of providing a range and a string message.
We match on `types.mark` to produce our message:

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

`AppExpectedFun` gets a reasonable message using pretty printed types.
Pretty printing handles adding parenthesis, replacing numeric IDs with fancy Greek letters, etc.

{{< notice note >}}
For informational purposes, we tag our diagnostic "types:".
This is just so it's easy to see where diagnostics come from in the playground.
A real implementation doesn't have to do this (but it can if you like it!).
{{</ notice >}}

Our other cases are similar, so we'll save ourselves some time.
With our query covered, we need a server to run it on request.

## We Need a Language Server

Something has to tell our query engine when to run our snazzy `diagnostics` query.
For us, that's the language server.
It cannot be overstated how much `tower-lsp` is doing for us here.
All we have to do is provide an implementation of their `LanguageServer` trait.

We aren't even going implement all of it.
To implement a trait, we'll need a type:

```rs
struct PellucidLsp {
  client: Client,
  database: Arc<Database>,
  dep_graph: Arc<DepGraph>,
}
```

`PellucidLsp` represents our server.
Anything it stores lives and dies with our server.
We stow our `database` and `dep_graph` in it, so that our query state lives as long our server lives.

`Client` is a `tower-lsp` type.
It represents the client that's connected to our server, letting us send the client information independent of responding to requests.
We implement `LanguageServer` for our struct:

```rs
impl LanguageServer for PellucidLsp {
  async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> { ... }

  async fn shutdown(&self) -> Result<()> { ... }
}
```

`LanguageServer` only requires two methods of an implementation: `initialize` and `shutdown`.
`initialize` is sent whenever a client connects to our server.
It includes parameters that configure things like workspace directory, capabilities supported by the client, etc.
Our server responds in kind, providing what capabilities, character encodings, etc. it supports.
`initialize` acts like a handshake, getting the client and server to agree to the common subset of the LSP they support.
`shutdown` acts more like a callback, allowing us to do any cleanup we need to do before our server exits.

You can find their implementation in the [repo](https://github.com/thunderseethe/making-a-language/tree/main/lsp/base).
That's all we need before we implement our first LSP request: diagnostics.
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

It only takes two lines to produce our list of diagnostics using our query.
`root_query_context` is a helper that constructs a `QueryContext` with no parent.
The remainder hammers those diagnostics into the shape of an LSP response.

Our query's diagnostics must become a `DocumentDiagnosticReportResult`.
A mouthful that amounts to our list of diagnostics embedded within a JSON message.
It exists to enable fancy features we won't support (yet): partial diagnostics, related files, persistence between requests, etc.

We do make use of one feature it provides.
When our list of diagnostics is empty, we return an unchanged report, rather than a report containing the empty list.
This informs our client it doesn't need to do anything.

With that, our client can requests diagnostics at their leisure.
But that's kind of not an ideal workflow.
I mean we're the compiler here.
Why must the client ask us when something is wrong?
**We** know when something is wrong!

`diagnostics` is one way we provide diagnostics, but not the main way.
Diagnostics are mainly reported through two other LSP requests: `did_open` and `did_change`.
Both are notifications to our server, requiring no response.

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
We also use it when a file is changed:

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
Our playground always updates the entire file contents, but the `did_change` request supports partial updates.
That means our implementation has to support partial updates.
But that's not important to what we're trying to understand right now.
The part we want to focus on is the overarching flow:

  * Update content
  * Set new content in the database
  * Query diagnostics
  * Public diagnostics

Our query engine incrementally compiles our new content to provide our diagnostics.
These two avenues are the main lanes that drive diagnostics to the client.
Preferred because they happen immediately upon changing the file.

## The Heart of a Language Server

Diagnostics gave us an introduction into implementing requests for our LSP.
We've got all the tools to start doing the really cool stuff.
We're going to implement five critically acclaimed LSP features:

  * Goto Definition
  * References
  * Rename
  * Hover
  * Autocomplete

All of these requests rely on two core queries: `syntax_node_starting_at` and `ast_node_of`.
Together these two queries form [the Heart of a Language Server](https://rust-analyzer.github.io//blog/2023/12/26/the-heart-of-a-language-server.html).
As we can see from how many queries they subsist.

`syntax_node_starting_at` turns a line/column range into a CST node, and `ast_node_of` turns a CST node into an AST node. 
We always call them in tandem, but we keep them as separate queries for caching.
Multiple line columns might all ultimately reference the same syntax node.
Keeping `ast_node_of` as it's own query means we cache the AST node once for that syntax node and not once per line/column.

## Turning Ranges into Syntax Nodes

Our `syntax_node_starting_at` query begins with a new `QueryKey` entry:

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

We're, hopefully, comfortable with the shape of queries.
The action starts in our producer. 
We turn a line and column into a handle for the CST node under our cursor:

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

`cst` lets us traverse our tree to find our node under cursor.
`newlines_of` also takes care of converting an LSP line and column into a byte offset (It does both actually. I was here yesterday, and it actually goes both ways).
We use `byte` to lookup a token in our `cst`:

```rs
let token = cst.token_at_offset(TextSize::from(byte));
```

This innocuous line is doing plenty of work under the hood, scouring our tree to grasp the token at our offset.
Fortunately, we put `rowan = 0.16` in our TOML file, so we're moving along.
We're not immediately done with our token.
Our offset might have landed us perfectly between two tokens (you never know what these users will think up next).
In such a case, we try to pick a meaningful token:

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
Whitespace is unlikely to ever be what our user wanted to interact with.
It lacks any interesting semantics.
Conversely, identifiers are exactly the type of token are users are looking for.
A quick run through of our operations convinces us why this might be:

  * Hover works on identifiers (and maybe numbers but it's certainly less interesting)
  * References only work on identifiers
  * Goto definition only works on identifiers
  * Rename only works on identifiers
  * Completion only works on identifiers (Left as an exercise for the reader to autocomplete numbers)

Really makes you wonder why we've got the other tokens at all.
All the semantic payoff lives in identifiers.
Finally, we turn our token into a syntax node handle:

```rs
let node = token.parent()?;
Some(SyntaxNodeHandle {
  root: green,
  ptr: SyntaxNodePtr::new(&node),
})
```

We actually want the node wrapping our token, so we walk up one level.
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
That completes our two queries.
Onwards to building our LSP features.

## Goto Definition

Have you ever read a variable and wondered, "Hang on, what's this mean again?".
Goto Definition has the answers you seek.
When we have our cursor over an identifier, goto def jumps us to the definition of that identifier.
For our language, that means either a let binding or a function parameter.

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

LSP works purely in source code ranges.
Our request receives a line and column in source for the client's cursor.
Our response provides the range of the definition, relying on the client to display it.
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

`definition_of` leads by retrieving the AST node at our cursor, using our one-two combo of queries:

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

We walk the parents of our variable, looking for a function that defines it.
We're walking the name resolved AST, so we can be confident our variable is defined somewhere.
If we don't find one, technically an impossible case, we bail.
Once we have our defining AST node, we need to turn it back into a CST node:

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

When our parent is a `Fun` node, we walk to the binder.
This provides a nicer experience, jumping to the identifier, rather than the `|` of the function.

{{<notice note >}}
We don't need to do this for let bindings because their function nodes already map onto their `LetBinder`.
{{</ notice >}}

Our last step turns our CST node into an LSP range:

```rs
let range_node = binder.first_token()?;
let newlines = this.newlines_of(uri.clone());
let range = newlines.lsp_range_for(range_node.text_range().into())?;
Some(range)
```

With that, we've implemented goto definition.

## Hover

Hover shows a nifty tooltip when we mouse over interesting bits of our code.
A rather abstract definition, as hover covers a lot of ground.
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

All the action lives in `hover_of`.
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

Unlike goto def, we do modify our syntax node before turning it into an AST node.
Hovers can technically be attached anywhere.
When we produce a hover, we give the client a range in the file where the tooltip should be displayed.
We want that range to be the syntax node under cursor, so we save a copy before modifying it.

Next, we check that we're looking at something hover-able.
A variable, bound or otherwise.
If we're looking at a function parameter, we want the parent node.
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
Our query completes by creating a [`Hover`](https://docs.rs/lsp-types/0.94.1/lsp_types/struct.Hover.html) from our CST node and type:

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

We wrap our string up in all the layers to tell the LSP it's a plain string.
We also specify it's for the language pellucid, but given our language didn't exist till today, I doubt any editors care 😔.

## References

References is like the inverse of goto def.
Given a variable, show me everywhere it's referenced in code.
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
`references_of` commences by ensuring we're looking at a variable:

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

`var_reference` huh? Feels a bit like drawing the rest of the owl.
`var_reference` does a depth first search of our AST, like `find`.
Unlike `find`, it returns every node that mentions our `Var`, not just the first one.
That gives us every occurrence of our variable, but we need a list of locations to complete our query:

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

We've seen this logic before in `hover`, look up the CST node, get its span, turn it into line and column.
We're doing it once per variable this time.
Honestly, we probably should've made it a query.
Whoops.
Either way, our job is done.

## Rename

Rename changes the name of a variable everywhere it occurs.
Hang on, "everywhere it occurs" sounds familiar.
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

Score! We don't need a new query for `rename`.
`references_of` gives us every occurrence of a variable already.
But, rather than a list of locations, we return a `WorkspaceEdit`.
As a server, we don't modify files directly.
We can't risk modifying a file while the user's trying to interact with it.
Instead, we whip up some JSON that says what changes we'd like to make, and ship them to the client to apply.

For rename, that means replacing the text at `loc.range` with `new_name` for every location.
But that's it.
Rename is short and sweet.

## Autocomplete

Last, but certainly not least is autocomplete.
In my humble opinion, the most prominent feature of the IDE experience.
As a user types, we plumb the codebase for anything they could possibly mean and present it to them as an option to save them keystrokes.
What an elating experience to have the compiler understand what I need and hand it to me on a silver platter.

Autocompletion can get extremely fancy.
You can figure out the type that should slot into the current cursor and only present options of that type.
You can track the type of object our user is currently accessing and only present methods available on that object.
The more type information you can procure, the better you can tailor your recommendations.
Alas, we will save that for another tutorial.

Our autocomplete takes a partially completed identifier.
We figure out all the variables defined in scope at that identifier and offer them as completions.
Like our other requests, we call a query and return its results as our response:

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

We built the whole query engine and dagnabbit we're gonna use the whole query engine!
`completion_of` immediately calls another query `scope_at`:

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
We'd recommend variables that aren't defined where the user is typing.
`scope_at` solves this for us by building our scope at a specific position in our program:

```rs
let QueryKey::ScopeOf(uri, cursor) = key else {
  unreachable!()
};

let node = this.syntax_node_starting_at(uri.clone(), cursor.clone())?;
let ast_node = this.ast_node_of(uri.clone(), node)?;
```

You thought we weren't gonna see our power duo didn't you?
Our eventual completions are in terms of the identifiers written in our source file.
But most of our ASTs work in `Var`, which does not make for a very good completion.
We bridge this gap by zipping our desugared AST and our typed AST together:

```rs
let desugar = this.desugar_of(uri.clone());
let types = this.types_of(uri.clone());
let scoped_ast = zip_ast(desugar.ast, types.ast);
let Some(parents) = scoped_ast.parents_of(ast_node.id()) else {
  return Some(HashMap::default());
};
```

This gives us an `Ast<(String, TypedVar)>` that contains both human readable variable name and the type.
We walk the parents of our node in the zipped AST.
If we don't have any parents, we must be at the root of our AST and nothing is defined in scope.
Bindings we find in our parents get collected into a hashmap:

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

It's important we walk our parents in reverse order.
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

Our completions are complete and with them we've implemented an LSP!

## A Little White Lie and Moment for Reflection

Implemented an LSP...mostly.
If you look at the source code for our LSP, you'll find quite a bit of code that isn't covered here. 
That's not because our tutorial is hiding LSP implementation from you.
I needed quite a bit of code to hook up the LSP to the playground and didn't have an easy way to separate it out from the LSP.

But it's worth it for the [playground](/making-a-language).
You should check it out!
Not only does it show off all our LSP features, but it also shows the trees at each stage of our compiler.
It even shows the output of running our code (that's right code can be run; not just compiled) and the log of requests our LSP sends.
All for the low, low price of your adoration.

I can't overstate how happy I am to hit this milestone.
Our language is modest, but we've implemented a full compiler for it end to end with an LSP to boot.
That's nothing to sneeze at.
I really appreciate you coming along with me on this journey.
When I started, my plan was just to write a little post about type inference.

Once I wet my appetite, however, I kept going and was desperate to reach something that could be called "complete".
The overflowing support for the series didn't hurt either, of course.
Thank you to everyone who reached out to say how much they enjoy the series or learned from it.
It means a lot to me.

This meets that milestone.
Hopefully you've found the explanations along the way educational.
There is always plenty more to be done.
Our language lacks top level functions, data types, modules, on and on.

But before setting off after those features I will probably take a break to do some writing about other things.
If you look at my post history over the last year, it's almost exclusively making a language posts.
I'd like to frolic for a bit before undertaking writing every pass for another feature (I'm looking at you rows or items).
Eventually, I'd like to continue making a language so we can build on top of base with the really fancy stuff.
