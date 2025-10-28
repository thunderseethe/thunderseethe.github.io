+++
title = "Reproachfully Presenting Resilient Recursive Descent Parsing"
date = "2025-09-10T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Parsing"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Parsing", "Recursive Descent", "LL(1)", "Lexing", "Concrete Syntax Tree", "Error Recovery"]
description = "Parsing syntax for our base language with error recovery"
+++

I must profess a certain disdain for the parsing tutorial.
I won't insult you by pretending it's fair or rationale.
This may not come as a surprise, given the making a language series has covered [every](/posts/types-base) [part](/posts/lowering-base-ir) [of the](/posts/simplify-base) [compiler](/posts/closure-convert-base) [it can](/posts/emit-base) before parsing.
Parsing tutorials just feel so feckless, sharing an unflattering kinship with the monad tutorial.
Thousands of tutorials written about parsing, and yet PL projects still die bikeshedding syntax.

Ultimately, parsing performs two roles in a compiler:

  * Check a source file contains syntax the compiler recognizes
  * Create an in memory representation that's easier to work with than a big string
    * We're doing compilers, so you _know_ this will be a tree

In theory, a simple task.
Pick some syntax, hopefully that's easy to parse.
Walk your input string and check each character is your selected syntax, build a tree out of it, and maybe report an error or two.
How hard could it be?

Perhaps that's part of my scorn.
I feel like everything that can be said about parsing has been said.
At the same time parsing feels like a labyrinth of endless choices deterring any headway.
There's the first order choices about syntax:

  * Should I be whitespace sensitive or not?
  * What keywords should I have?
  * Are semicolons required?
  * What about trailing commas?

I'm already getting dizzy, but then you have the second order choices.
Not just what should you parse, but how:

  * Should I use a parser generator?
  * What about Earley parsing? Handrolled or generated?
  * Or a parser combinator library?
  * I hear PEG is in vogue

Each of these comes with their own tradeoffs and will make it easier or harder to parse certain syntaxes.
There's a lot to decide, and I don't have the answers.
Yet, we need a parser.
We can hardly say we've made a language if users have to hand assemble bits into our AST and feed that into our running compiler.
Our language needs an interface end users can write and provide us, and that's syntax in a source file.
Despite my misgivings, we need a parser.
And so I place my parsing platitudes upon the pile.

## Text Must Turn To Trees

Parse we must, but that doesn't help us navigate the many choices that remain in doing the parsing.
Our goal here is humble, get in, get a parser, get out.
Do not get caught in the tarpit.
We're not here to design the best syntax, or the optimal parsing strategy.

Doable and passable define our methodology today.
To that end, we'll gather some constraints to help us pick our preferred parsing platform.
First and foremost, is that our parser is handwritten.
There are a bajillion parser generators and libraries that will take some kind of grammar or config file and spit out a parser.
But my experience with these tools is you have to spend quite a bit of time learning the tool and can never quite tune the knobs to do what you want.
They can be a boon, but they can also be a time sink.
We won't need them, and we can save time by avoiding them off the bat.

After handwritten, we have two more constraints that are informed by the goal of our language.
In the modern era, languages (and their compilers) are expected to be more interactive than the bygone days of batch compilation.
It's now table stakes for any new language on the block to support IDE like features, usually via the [Language Sever Protocol](TODO).
As a modern language ourselves, we need to support these fancy features.

For the parser that takes the shape of two requirements:

* Resilience
* Full Fidelity

Resilience means our parser does not stop at the first error.
This might sound a bit javascript-y at first, but stay with me.
Once our language is interactive it will often see source code while the user is editing it.
If our parser stops at the first error, anytime our user modifies a function we become unable to parse the entire file.
Rather than stop at the first error, we want to confine that first error and parse as much valid syntax as possible.

Our second constraint, full fidelity, is related to our ability to do this.
Full fidelity means our parser is going to represent every character of source text in the tree it constructs.
Often this is called a concrete syntax tree (CST) as opposed to an abstract syntax tree (AST).
Our CST will help us with error recovery as it allows our tree to represent invalid syntax.
It also helps us with the LSP functionality we want to support.
A lot of LSP operations take the shape of give me the semantic information for the whatever my cursor is pointing at, and CSTs make that easier by bridging between position in the source file and position in the AST we're going to construct.

With those constraints in mind our parsing strategy will be [Recursive Descent](https://en.wikipedia.org/wiki/Recursive_descent_parser).
A recursive descent parser is a set of functions that call each other to parse our input.
By virtue of just being functions, it is straightforward to handwrite.
Each function looks at the next input and decides how to proceed.
If we want to parse an integer, we'd write an `int` function:

```rs
fn int(
  mut input: &str
) -> (Result<isize, std::num::ParseIntError>, &str) { 
  let mut integer = String::new();
  while let Some(digit) = input.get(0).filter(|c| c.is_digit()) {
    integer.push(digit)
    input = input[1..];
  }
  (integer.parse::<isize>(), input)
}
```

`int` consumes input as long as the next character is a digit.
Once it runs out of digits, we try to parse our `String` into an `isize`.
Regardless of our parses result, we return the remaining input.
This lets the next parser pick up where we left off.

That's a quick taste of recursive descent.
But our example was neither resilient nor full fidelity.
`int` stops at the first error it encounters, and it doesn't handle whitespace at all.
Fortunately, because recursive descent is just functions, we just have to write the functions better to fix that.

## Writing the Functions Better

Our actual parser will produce a full fidelity tree.
We'll need a way to store a full fidelity tree to do that.
Unlike our AST, we won't store our CST in an enum.
We're going to farm out to a library for storing our CST.
We could write our own, but much like the union-find, we're interested in parsing not the particulars of efficiently storing our CST.
Leaning on a library lets us get back to parsing quickly while providing a robust solution.

Our library of choice is [rowan](https://crates.io/crates/rowan).
Rowan is used by rust-analyzer, so I trust it to do its job well.
Under the hood rowan uses a [Red-Green Tree](https://willspeak.me/2021/11/24/red-green-syntax-trees-an-overview.html).
This is a common choice for CSTs (Swift, C#, and Kotlin also use this kind of tree).
If you're interested, you can find details in the [rust-analyzer book](https://rust-analyzer.github.io/book/contributing/syntax.html).

Our purposes are suited just fine by using the fruits of rowan's labor.
What we need to know about rowan is that it stores a homogenous n-ary tree.
Each node of our tree holds a tag and any number of children.
We'll define an enum to represent our tag, named `Syntax`:

```rs
enum Syntax {
  Identifier,
  Integer,
  Functions,
  // ... the rest of our syntax nodes.
}
```

Our enum lacks associated data, unlike our AST.
Each node contains the same data, an arbitrary number of children, regardless of this tag.
Under the hood rowan turns `Syntax` into a `u16`, so we can't attach data even if we wanted to.

This flexibility is what allows for our resilience.
Our CST is perfectly happy to represent a binary expression as:

* BinExpr
  * Integer @ "3"
  * Whitespace @ " "
  * Operator @ "+"
  * Whitespace @ " "
  * Integer @ "1234

But it's equally happy to represent it as:

* BinExpr
  * Integer @ "3"
  * Error @ "%=%"
  * Integer @ "1234"

When the user gives us an operator we don't recognize, it'll happily let us represent a binary expression as:

* BinExpr

Much like dynamic typing, this freedom comes at a cost.
The CST will let us put anything in the tree, but that means it will let us put _anything_ in the tree.
Our CST allows us to represent invalid trees, which is critical to handling erroneous input gracefully, but it also handles any bugs we introduce the same way.
This is a tradeoff.
Swift generates a node for each CST with typed fields.
But then it has [gargantuan nodes](https://swiftpackageindex.com/swiftlang/swift-syntax/601.0.1/documentation/swiftsyntax/functiondeclsyntax/init(leadingtrivia:_:attributes:_:modifiers:_:funckeyword:_:name:_:genericparameterclause:_:signature:_:genericwhereclause:_:body:_:trailingtrivia:)) to represent all the possible failures states.

## Guess we need some syntax

Rowan will let us parse things in full fidelity, resiliently.
All we've got to figure out now is what to parse.
This is a dangerous game.
Picking syntax is literally the worst case of [Wadler's Law](https://wiki.haskell.org/Wadler's_Law).
Thankfully, we won't have comments, yet.

Our previous passes provide guard rails to keep us on track.
Whatever syntax we pick, it must turn into our AST:

```rs
enum Ast<V> {
  /// A local variable
  Var(NodeId, V),
  /// An integer literal
  Int(NodeId, i32),
  /// A function literal (lambda, closure).
  Fun(NodeId, V, Box<Ast<V>>),
  /// Function application
  App(NodeId, Box<Ast<V>>, Box<Ast<V>>),
  /// Typed hole.
  Hole(NodeId, V),
}
```

We're going to need syntax for each of these nodes.
Conversely, any syntax that doesn't map onto one of these nodes will be treated with great skepticism.
First on our list will be variables, represented by identifiers.
Our identifiers will be series of alphanumeric characters that aren't keywords.

An integer will be a series of digits (where a digit is `0-9` here).
We aren't going crazy with the numeric literals, yet.
These are straight forward to parse because they contain one, nebulously defined, "thing" unlike `Fun` and `App` which contain multiple "thing"s.

Thing resides in quotes because we have yet to define what that means for our language.
At minimum, our smallest unit is a character.
Each thing could be a character, but this is too fine-grained and onerous in practice.
Rather than consider each thing individually, we'll group characters together to make them easier to work with.
Grouping is accomplished by an initial pass over our input string called lexing.

## Lexing

The goal of lexing is to take our input string and do a fast pass over it to group characters into tokens.
For example, lexing recognizes the string `"let"` is the keyword `let` or the string `"1234"` is a number literal.
Our parser can then consume these tokens, making its job easier.

Lexing is not mandatory.
Our parser could work directly on characters, such a technique is called [scannerless parsing](https://en.wikipedia.org/wiki/Scannerless_parsing).
To that end, we're happy to employ a lexing library to make life easy.
Anything to stay out of the tarpit.

Our lexing library of choice is [`logos`](https://logos.maciej.codes/).
Under the hood it does a lot of gnarly work to make lexing fast, but we get to ignore all that and use its pleasant API.
Our tokens will reuse our `Syntax` enum from rowan:

```rs
#[derive(Logos)]
enum Syntax {
  #[token("(")]
  LeftParen = 0,
  #[token(")")]
  RightParen,
  #[token("|")]
  VerticalBar,
  #[token("=")]
  Equal,
  #[token(";")]
  Semicolon,
  #[token("let")]
  LetKw,
  #[regex("[\\p{alpha}_]\\w*")]
  Identifier,
  #[regex("\\d+")]
  Int,
  #[regex("\\s+")]
  Whitespaces,
  #[end]
  EndOfFile,
  // ...
}
```

`logos` works through a macro we invoke with `#[derive(Logos)]`.
It lets us annotate variants with the `token` and `regex` directives to define how they are lexed.
`token` takes a string literal and uses it to recognize our token in our input stream.
This suffices for all of our punctuation and keywords because their content is static.

For identifiers, we'll need `#[regex]` because identifiers are an infinite set of valid strings.
regex takes a regex and will treat anything that matches that regex as the corresponding token.
An identifier is an alphabetic character or `"_"` followed by any characters in the word class `\w`.
You might expect our leading character to be `[a-zA-Z_]`.
We use the odder looking `\p{alpha}` because it supports Unicode.

{{< notice info >}}
What do we do if our user wants to name a variable `"let_keyword"`?
How would we tell whether that's an identifier or the `let` keyword followed by identifier `_keyword`?
Logos will take the longest match out of our provided tokens.
Because Identifier will match `let_keyword` it's longer match will win out over LetKw's `let` match.
{{< /notice >}}

We use regex to recognize our integers and whitespace as well.
An integer is one or more digits, recognized by `\d+`.
Whitespace is one or more spaces, recognized by `\s+`.

Our last token `EndOfFile` lacks a recognizer.
It's marked `#[end]` because it has no text associated with it.
Logos will only produce it when it runs out of input.
We'll use it to make sure our program parses the entire source file.

You may have noticed that our tokens don't cover the entire Unicode range, only a small subset.
Users of our language, however, are free to put whatever they want in their source files.
They could even put something nefarious in their like "+" or "{", and we have no valid tokens to assign to such text.

Even though these characters aren't valid syntax we still need to handle them.
If we're stumped by unknown characters, we'll never achieve error resilience.
To this end, we add another variant to `Syntax`:

```rs
enum Syntax {
  // Our tokens...
  Error,
  // ...
}
```

`Error` serves dual duty as both invalid tokens and invalid nodes:

  * In our lexer, text we don't recognize we'll produce an `Error` token.
  * In our parser, a tree that's not valid syntax gets wrapped in an `Error` node.

With lexing out of the way we can now better design our syntax.

## Finish Up Syntax

We now have a better grasp on what we mean when we say a variable is a "thing".
It's a token, specifically an `Identifier`.
All `Var`s are identifiers, but not all identifiers are `Var`s.
A function contains an identifier for its parameter, but it's not a `Var` node.
How will we know when we see an identifier whether it's a `Var` or a `Fun`?

The answer involves an important property of our syntax.
We should be able to tell what we're parsing from the next token of input at all times.
This doesn't mean we have to treat each token the same way when we see it.
Functions and variables use identifiers differently.
But we need enough syntax to distinguish those two uses by looking at the next token.

We'll introduce extra syntax to inform the parser what it's seeing is a function and not a variable:

  * If we see a bare identifier, we'll always treat that as a variable.
  * If we instead see a `|` (VerticalBar), we know we're parsing a function and the following identifier is a function parameter.

This works because we know an identifier must always follow a `|`.
We'll similarly note the switch from function parameters to function body with another `|` giving us our full function syntax:

```rs
|x| x
```

We'll ignore any similarities that might evoke to other function syntax.
This is our own unique language and it's very special!
Our token stream `VerticalBar Identifier VerticalBar Identifier` gives me pause.
How do we know that last `Identifier` is a `Var` despite it being preceded by a `VerticalBar`?
Our parser is allowed to rely on token it's already consumed.

Once we see the first `VerticalBar`, we know we're going to parse `VerticalBar Identifier VerticalBar`.
We only need to consider the next token of input at an inflection point where we might pick one of a few possible parses.
Function bodies are one such position.
If our function was instead `|x| |y| x`, after parsing `|x|` the following `|` would tell us we're starting another function.

Our last node, `App`, needs syntax still.
As a functional language, we expect to apply functions _a lot_.
Accordingly, we're going to give application a relatively terse syntax.
Any two things juxtaposed will be considered an application: `f x`, `x 1`, etc.

That covers all our `AST` nodes.
That's not quite enough syntax to be a usable language in practice.
We have some more syntax we'll _need_ to get the job done, and then we'll have some syntax that's just cute, and we like having around.

A remaining issue with our syntax is how to tell when one expression stops and another starts.
Consider the string `|f||x| f x g`.
Is that string one function with a body `f x g` or an application of function `|f||x| f x` to argument `g`.
We'll need a way to spell both.

Parenthesis provide disambiguation between our two parses.
By default, `|f||x| f x g` parses as one function.
Our alternate interpretation uses parenthesis to bound the function `(|f||x| f x) g`.
Parenthesis will also serve to disambiguate nested applications.
`x z y z` is read "`x` applied to 3 arguments", and `x z (y z)` is "`x` applied to `z` and a nested application `y z`".

Last we have some syntax we're throwing in because it makes the language nicer.
Such syntax is called syntax sugar because it exists solely syntactically.
A desugaring pass turns syntax sugar into our existing `Ast` nodes, effectively erasing it.

We risk entering the tarpit here.
Syntax sugar is by design not necessary and has no `Ast` nodes to ground it.
I think it's worth covering, however, as you'll be hard-pressed to find a language in production with zero syntax sugar.
It's important to cover how it is implemented and supported by IDE-like features.
We'll have to tread carefully.

Our syntax sugar is the let expression.
As you might have guessed from our sole keyword: `let`.
A let expression comprises an identifier, an expression that defines that identifier, followed by an expression that has access to our freshly defined identifier.
For example `let two = add 1 1; add two two`.
We're free to nest lets, so this is also valid:

```hs
let two = add 1 1;
let four = add two two;
add two four
```

Let is syntax sugar because where-ever it appears we could instead write an applied function: `(|two| add two two) (add 1 1)`.
This behaves exactly the same way, but I'd much rather write the `let`.
That covers all our syntax. 
We're finally ready to start parsing.

## Setting the Stage

We'll want to share some mutable state between our functions.
A pattern we've seen before in our type checker.
We'll solve it the same way, by introducing a `Parser` struct and making each function a method of `Parser`:

```rs
struct Parser<'a> {
  input: Input<'a>,
  builder: GreenNodeBuilder<'static>,
  errors: Vec<ParseError>,
  in_error: bool,
}
```

`errors` and `in_error` handle error reporting.
`errors` accumulates all the errors we report.
`in_error` prevents us from reporting multiple errors for the same span of text.
Once we start reporting one error, we don't want to report a new error until we've finished the first one.

`builder` is from the `rowan` crate and maintains our CST.
A completed `GreenNode` is immutable, so while parsing we need a mutable builder.
Last but not least, `input` is a helper struct that manages our source text:

```rs
struct Input<'a> {
  content: &'a str,
  lexer: Peekable<logos::SpannedIter<'a, Syntax>>
}
```

`content` is the source we're parsing, helpfully provided by the user.
We hang onto a reference to it so we can get the source text associated with our tokens.
`lexer` is a `logo::SpannedIter`.
An iterator that produces our `Syntax` tokens with their spans in the source string.

{{< notice note >}}
A span is a range of offsets into our source text represented by `Range<usize>`.
{{< /notice >}}

We call `.peekable` on that iterator to get our full `lexer` type.
Peeking allows us our one lookahead token.
`Input` provides methods to help us navigate our source text.
First is `peek` which lets us see the next token:

```rs
fn peek(&mut self) {
  self
    .lexer
    .peek()
    .map(|(tok, _)| match tok {
      Ok(tok) => *tok,
      Err(_) => Syntax::Error,
    })
    .unwrap_or(Syntax::EndOfFile)
}
```

Logos does a lot of the heavy lifting here to produce `tok` which is a `Result`.
If that result is an `Err` we produce an `Error` token.
If our overall iterator returned `None`, we're out of source text and we return `EndOfFile`.
It's very common we'll want to check if we're at a particular token, so we add a helper `at` for that as well:

```rs
fn at(&mut self, token: Syntax) -> bool {
  self.peek() == token
}
```

Importantly, neither of these methods actually move us forward in our input.
We're free to peek at the next token as many times as we want.
Progressing forward is accomplished by `advance`:

```rs
fn advance(&mut self) -> Option<Range<usize>> {
  let (_, span) = self.lexer.next()?;
  Some(span)
}
```

`advance` returns the span of the token we just passed.
`advance`, perhaps surprisingly, does not return the actual token we just passed, only it's span.
Calls to `advance` are almost always preceded by calls to `at`, so we already know what token we're _at_.
In fact at followed by advance is so common, it gets its own helper `eat`:

```rs
fn eat(&mut self, token: Syntax) -> Option<&str> {
  if self.at(token) {
    self.advance().map(|span| &self.content[span])
  } else {
    None
  }
}
```

`eat` is the method we'll call the most on `Input`.
Given a `token`, if we're at that `token`, advance the input.
`eat` knows what to do on the happy path, but it says nothing about what to do when a `token` is missing.
If we're not at the right token, we simply return `None`.
Error handling is under the purview of the `Parser` as how we handle it depends on what we're parsing.

## The Parsing Play Must Go On

Now that we're familiar with all our parsing state, we can begin the actual work of parsing.
Despite our parser working top down, descending into recursive calls, we'll explain it bottom up.
We'll start with our leaf parsing functions and culminate in our top level `program` parser.

The backbone of our parser is the `expect` function:

```rs
impl Parser<'_> {
  fn expect(
    &mut self, 
    token: Syntax,
    anchor_set: HashSet<Syntax>
  );
}
```

`expect` _expects_ a particular `token` to be the current token in our input.
There are some notable similarities to `Input`'s `eat`.
But `expect` takes an `anchor_set`.
We'll see it is used for error recovery.
Our happy path is straightforward:

```rs
fn expect(&mut self, token: Syntax, mut anchor_set: HashSet<Syntax>) {
  let ControlFlow::Continue(_) = self.ate(token) else {
    return;
  };

  // We didn't see the token we expected
}
```

`ate` maintains our state for us when we do see the expected token:

```rs
fn ate(&mut self, token: Syntax) -> ControlFlow<()> {
  let Some(str) = self.input.eat(token) else {
    // We didn't consume the right token so continue.
    return ControlFlow::Continue(());
  };
  self.in_error = false;
  self.builder.token(token.into(), str);
  self.whitespace();
  // We consumed the expected token, so we break to return early.
  ControlFlow::Break(())
}
```

Upon eating the correct token, we set `in_error` to false.
Since we've consumed expected input, any ongoing errors are resolved, and we're ready to start errors anew.
We also add a leaf node to our tree containing our token and source text.

Finally, we eat any whitespace after this token.
Our syntax isn't whitespace sensitive, so we don't have to be particularly mindful of where it arises.
It's important that we only do this upon eating the expected token, but otherwise we just want to eat whitespace somewhere convenient and out of the way.

{{< notice note >}}
`ate` could return a `bool`, but `ControlFlow<()>` has nice meaningful names for our use case.
{{</ notice >}}

If we don't see the right `token`, `expect` will perform error recovery using `anchor_set`:

```rs
fn expect(&mut self, token: Syntax, mut anchor_set: HashSet<Syntax>) {
  // Our happy path above...
 
  // Otherwise, start error recovery
  // We can always recover to our expected token, so ensure it's in the anchor set.
  anchor_set.insert(token);
  self.recover_until(anchor_set, vec![token]);

  // ...
}
```

Our error recovery strategy discards tokens until we reach a token in our `anchor_set`.
It's pertinent to ask "what's in the anchor set"?
The anchor set is the set of syntax we know how to reset parsing at.
The syntax within the anchor set is contextual based on what we're parsing.

* When we're parsing a program, the only syntax in the set is `EndOfFile` because that's the only point we know how to reset for an entire program.
* When we're parsing a let expression, our anchor set will include the keyword `let` (and `EndOfFile`).

One let can always be followed by another let.
If we encounter an error within a let we can restart parsing safely at the next `let` keyword by completing our current let expression with an error and starting a new let expression.
Any tokens between our current unexpected token and one of the recovery tokens in our anchor set is discarded:

```rs
fn recover_until(&mut self, anchor: HashSet<Syntax>, expected: Vec<Syntax>) {
  let mut discard_toks = vec![];
  while !self.input.at_any(anchor.clone()) {
    let tok = self.input.peek();
    let Some(span) = self.input.advance() else {
      break;
    };
    discard_toks.push((tok, span));
  }

  // ...
}
```

We don't truly mean discarded here, we'll still add them to the tree, but they'll be marked as `Error`.
Why discard a variable number of tokens like this?
Couldn't we leave the token where it is?
A following parse might be able to correctly interpret it.

Consider the input `let  = foo 1; 2`, which is missing an identifier for the let.
If we did no error recovery, our first expect call for an identifier would fail and would leave our `Input` at the whitespace:

```rs
let █= foo 1; 2
```

Our subsequent expect `=` call would fail. The current input is whitespace not `=`
Our missing identifier cascades into the rest of our let expression and our parse tree ends up being `LetKw` followed by all erroneous syntax.

But that's because we didn't consume any input, what if we skipped over one token on error to make sure we're always making progress?
Simply removing the whitespace from our first example shows us where that fails: `let = foo 1; 2`.
Now when we expect our missing identifier, we skip over `=` as an error:

```rs
let =█foo 1; 2
```

But oh no, that means our expect call to `=` now sees whitespace (or `foo`).
It will error, and we're back to a cascade of errors.

We need a Goldilock skip to cover just the right amount of input to get us back on track.
Skipping a static number of tokens won't achieve good error recovery.
Hence, the anchor set.
It informs us what tokens are safe to skip to and prevents the cascade of errors.
With the anchor set when we encounter the missing identifier of `let  = foo 1; 2`, we see `=` is in our anchor set and mark everything until `=` an error.

Our let parser would then be correctly synchronized.
We produce a parse tree of a mostly valid let but with a prominent lack of an identifier.
Luckily, lacking an identifier isn't the Parser's problem.
That's an issue for name resolution.
This synchronization is the secret to our resilience.

With a better understanding of why we're throwing away tokens, let's look at how we actually go about emitting errors:

```rs
fn recover_until(&mut self, anchor: HashSet<Syntax>, expected: Vec<Syntax>) {
  // Discard tokens...

  // If we're already at an anchor there is nothing to do.
  if discard_toks.is_empty() {
    if !self.in_error {
      self.in_error = true;
      self.errors.push(ParseError {
        expected,
        span: self
          .input
          .lexer
          .peek()
          .map(|(_, span)| span.clone())
          .unwrap_or_else(|| {
            let len = self.input.content.len();
            len..len
          }),
      });
    }
    return;
  }

  // ...
}
```

If we didn't actually discard tokens, and we're not already in an error, we still want to emit an error to let the user know expected syntax was missing.
Our error lists the tokens we expected (for `expect` this will always be one token) and a span to show the diagnostic.
Since our error is a missing token, which happens to also be missing a span, we'll put our error on the next token in our input.
If we did discard tokens, our reporting is more interesting:

```rs
fn recover_until(&mut self, anchor: HashSet<Syntax>, expected: Vec<Syntax>) {
  // Discard tokens...

  // Emit empty error...

  // This is safe because discard_toks is not empty.
  let mut err_span = discard_toks[0].1.clone();
  self.with(Syntax::Error, |this| {
    for (tok, span) in discard_toks {
      err_span.end = span.end;
      this.builder.token(tok.into(), &self.input.content[span]);
    }
  });
  if !self.in_error {
    self.in_error = true;
    self.errors.push(ParseError {
      expected,
      span: err_span,
    });
  } 
}
```

{{< notice info >}}
`with` is a straightforward function to make constructing CST nodes easy. It just wraps our closure in calls to `builder.start_node` and `builder.finish_node` so we can't forget to keep them synced up.
{{< /notice >}}

We create a span that covers all our discarded tokens.
As we do this, we also add an `Error` node to our tree containing all the tokens we discarded.
If we're not already in an error, we emit the span as an error.
We mark our nodes in the tree as an error regardless, but we only emit the error to the user if we're not already emitting an error.
One final remnant remains in `expect`: 

```rs
fn expect(&mut self, token: Syntax, mut anchor: HashSet<Syntax>) {
  // Happy path...

  // Error recovery...

  let _ = self.ate(token);
}
```

We end `expect` with another call to `ate` that we do not check.
Our expected token is always added to our anchor set, so error recovery might have left us at the token we expected.
In which case, we want to go ahead and consume it in this call, otherwise subsequent expect calls will error.
Returning to our bountiful let example, imagine if it looked like this `"let x | = foo 1; 2"`.
We have a valid let in their, but with an extra `|` lurking.
Our expect `=` call will see the `|`, discard it as erroneous, and successfully recover to the `=`.
At this point, however, if we do not check for `=` again, we would move on with our parsing leaving `=` as our current input.

## Parsing Applications Atomically

That completes `expect`.
We can now employ it to begin assembling our parsing ensemble.
Our end goal is to parse a program, which for us means an expression.
We'll start building towards parsing an expression by parsing function application.
A naive application parser might be structured as:

```rs   
fn application(&mut self) -> {
  self.with(Syntax::App, |this| {
    this.expr();
    this.expr()
  }) 
}
```

This looks lovely. It mirrors the application node in our AST directly and does exactly what we need it to do.
The problem is it will never return.
The recursive in recursive descent rears its ugly head here.
As we said early to parse an expression we have to parse an application, but as we can see here to parse an application we have to parse an expression.

This kind of mutual recursion is the namesake of recursive descent.
But here we have _bare_ recursion.
At no point in the call chain does our parser consume input.
It's perfectly valid for our parser to call `expr() -> application() -> expr() -> application() -> ...` forever.
Our parser needs to consume input somewhere in that call chain to make sure it will terminate.

Avoiding this is a matter of being more precise in what we want to parse and ensuring that any recursive call to `expr` is only made after consuming input.
Rather than an application being two expressions, an application will be any number of atoms.

{{< notice note >}}
Parsing theory calls this [removing left recursion](https://en.wikipedia.org/wiki/Left_recursion#Removing_left_recursion), if you'd like to read up on it.
{{< /notice >}}

An atom is basically any expression that isn't an application.
For us, that means variables, integers, functions, and parenthesized expressions.
Rather than our naive initial `application`, our actual `app` parses any number of atoms:

```rs
fn app(&mut self, anchor: HashSet<Syntax>) {
  let checkpoint = self.builder.checkpoint();

  let ControlFlow::Continue(()) = self.atom(anchor.clone()) else {
    // An application must have atleast one atom within it
    self.recover_until(anchor, vec![Syntax::Expr]);
    return;
  };

  let ControlFlow::Continue(()) = self.atom(anchor.clone()) else {
    return;
  };

  self.builder.start_node_at(checkpoint, Syntax::App.into());
  self.builder.finish_node();

  while let ControlFlow::Continue(()) = self.atom(anchor.clone()) {
    self.builder.start_node_at(checkpoint, Syntax::App.into());
    self.builder.finish_node();
  }
}
```

Parsing one or more `atom`s is another tactic to avoid diverging.
We want to parse a series of applications `x y z w` as nested applications `(((x y) z) w)`.
When we parse `x`, however, we don't know how many applications it will be nested within until we parse them.
We remedy this by parsing all our atoms in a list and then nesting the applications after we've parsed everything.

`app` starts by ensuring we have at least one `atom` (we'll pretend that's an expression for now).
Next we check for a second atom, if we only have a single atom we return it as is.
We'll use `app` to parse a single expression alongside any number of applications.
Once we have two atoms we make use of the checkpoint we create at the top of our `app`.

A checkpoint, as the name might imply, saves an index into our tree and allows us to add nodes there later.
We need it here because we don't know we want to create an `App` node until we know we have two atoms.
But once we know we want the `App` node, we want it to encompass the two atoms we've already parsed.

We still need to figure out how to parse atoms.
`atom` has to parse four different cases, unlike our other parsers all of them are optional.
If `atom` sees a token of input it doesn't know how to handle it'll return `Break` without consuming it.
This allows `app` to parse any number of atoms but stop once we run out.
Our `atom` starts matching on the current input token:

```rs
fn atom(&mut self, anchor: HashSet<Syntax>) -> ControlFlow<()> {
  match self.input.peek() {
    Syntax::Identifier => {
      self.with(Syntax::Var, 
        |this| this.expect(Syntax::Identifier, anchor));
    }
    Syntax::Int => {
      self.with(Syntax::IntegerExpr, 
        |this| this.expect(Syntax::Int, anchor));
    }
    // Our function and parens cases...
    _ => {
      return ControlFlow::Break(());
    }
  };

  ControlFlow::Continue(())
}
```

We'll focus in on our function and parens cases in a moment.
Our first two cases are straightforward:

* A leading `Identifier` is a variable.
* A leading `Int` is an integer.

In both cases, we wrap our token up as its own syntax node.
This is purely for convenience, wrapping semantically meaningful tokens as nodes makes them easier to find among the whitespace when we're traversing our CST.
Let's look at functions next:

```rs
fn atom(&mut self, anchor: HashSet<Syntax>) -> ControlFlow<()> {
  match self.input.peek() {
    // ...
    Syntax::VerticalBar => {
      self.with(Syntax::Fun, |this| {
        this.expect(Syntax::VerticalBar, anchor.clone());
        this.with(Syntax::FunBinder, |this| {
          this.expect(Syntax::Identifier, anchor.clone())
        });
        this.expect(Syntax::VerticalBar, anchor.clone());
        this.expr(anchor);
      });
    }
    // ...
  }
  // ...
}
```

We can see that our function syntax, `| <identifier> | <expr>`, maps directly onto code.
The body of our function is an expression.
Unlike our naive application, we don't have to worry about this forming a cycle because we'll always consume `|` before recursing into `expr`.
We employ a similar tactic for parenthesized expressions:

```rs
fn atom(&mut self, anchor: HashSet<Syntax>) -> ControlFlow<()> {
  match self.input.peek() {
    // ...
    Syntax::LeftParen => {
      self.with(Syntax::ParenthesizedExpr, |this| {
        this.expect(Syntax::LeftParen, anchor.clone());
        this.expr(unioning(&anchor, [Syntax::RightParen]));
        this.expect(Syntax::RightParen, anchor);
      });
    }
    // ...
  }
  // ...
}
```

A parenthesized expression kind of says it all on the tin.
It's an expression surrounded by parentheses.
It exists to help distinguish how applications are grouped.
If we have four atoms `"f 1 g 2"`, we can use parentheses to express they're grouped as nested applications `"f 1 (g 2)"` and not f applied to 3 arguments.

Because our expression is guarded by the `(`, we're safe to recurse here without fear of infinity.
We also modify our anchor set.
Previously we've threaded through our anchor set unmodified, but that changes now.
When we're within a parenthesized expression, and only then, we want to allow recovery to a following `)`.

## Let Them Parse Let Expressions

With that we've parsed atoms and applications.
We're halfway to parsing full on expressions.
Our other half is `let_` which parses let bindings:

```rs
fn let_(&mut self, anchor: HashSet<Syntax>) {
  self.with(Syntax::Let, |this| {
    this.expect(
      Syntax::LetKw,
      unioning(
        &anchor,
        [Syntax::Identifier, Syntax::Equal, Syntax::Semicolon],
      ),
    );
    this.with(Syntax::LetBinder, |this| {
      this.expect(
        Syntax::Identifier,
        unioning(&anchor, [Syntax::Equal, Syntax::Semicolon]),
      )
    });
    this.expect(Syntax::Equal, unioning(&anchor, [Syntax::Semicolon]));
    this.expr(unioning(&anchor, [Syntax::Semicolon]));
    this.expect(Syntax::Semicolon, &anchor);
  })
}
```

Our let parser doesn't parse the full let binding: `let <identifier> = <expr>; <expr>`.
It only parses `let <identifier> = <expr>;` with no trailing body expression.
Similar to `app` and `atom`, an expression can have zero or more let bindings.
Rather than recursively call a `let_expr` function to handle this, we parse any number of let bindings in a loop and then fix up our tree to scope the let bindings correctly.

Let parsing also makes heavier use of our anchor set, modifying it extensively.
At each expected token, we want our anchor set to include the remaining synchronization points in our let binding.

* For LetKw, this is an Identifier, Equal, or Semicolon because the remainder of our let binding after the keyword is `<identifier> = <expr> ;`.
* For Equal, it's just semicolon because the remainder of our let binding at that point is `<expr> ;`.

We don't include anything from expression in our anchor set.
Expression doesn't make for a good synchronization point because it covers all of our syntax.
If `let_` recovered in the middle of an expression, say at a `|` for example, it wouldn't know how to continue parsing from there.

If we encounter a missing `=`, we'll treat all of `<expr>` as an error and reset parsing at the following `;`.
This is a tradeoff and more art than science.
In practice, I've found this to give pretty reasonable errors for not a lot of effort, but one could absolutely try to do something more sophisticated to recover here.

Honestly all this anchor set management is repetitive and rote.
I bet we could abstract a lot of this out pretty easily.
At each token we want our anchor set to contain the tokens following itself.
We can automatically calculate that from our syntax because it knows what tokens will follow in each of our parsing functions.
For each of our parsing functions, we could create a simple struct like:

```rs
struct Let(Vec<Syntax>);
```

We could configure our vector to be `vec![Syntax::LetKw, Syntax::Identifier, Syntax::Equal, Syntax::Expr, Syntax::Semicolon]` and use that to determine what should be in the anchor set "for free".
Now that you mention it, we could also use that vector to determine the first token of each our parsed items.
`atom` wouldn't have to manually maintain a `match` it could just check if `peek` was in the first of it's four cases!
Now that you mention that you mention it, we don't have to stop at deriving first token of each parser.
We could derive an entire `parse` function from our structs.
All we need is a `Parse` trait with _a few_ associated types for each Synt-

## Dagnabit The Tarpit Got Me Again.

Pretty soon we'll find ourselves writing a parsing library (or heaven forbid a parser generator) but **not** a parser.
Our code is repetitive and rote, but it is also done.
We best move along before the tarpit encroaches further.
With lets squared away we have all the pieces to parse expressions:

```rs
fn expr(&mut self, anchor: HashSet<Syntax>) {
  self.with(Syntax::Expr, |this| {
    while this.input.at(Syntax::LetKw) {
      this.let_(unioning(&anchor, [Syntax::LetKw]));
    }
    this.app(anchor);
  });
}
```

For all our trials and tribulations in `app` and `let_`, `expr` is pleasingly direct.
Too direct in fact.
Are we sure this thing really works?

An expression is any number of let bindings followed by a singular `app` (recall that `app` might be a single atom).
If we see an expression such as:

```rs
let one = |s||z| s z;
let add = |m||n||s||z| m s (n s z);
add one one
```

We'll parse that as the tree:

* Expr
  * Let
    * ...
  * Let
    * ...
  * App
    * App
      * Var
       * `add`
      * ...
    * ...

I've omitted some details, but you get the idea.
The body of our let is implied by whatever follows in our expression, and our expression is a flat sequence of lets ending in an application.
It's important for scoping and name resolution that we remember of let is within each other, but for the parse tree it suffices and simplifies to have them laid out flat.
It is important that our sequence is terminated by the app.
For example, this would be invalid syntax:

```rs
let one = |s||z| s z;
add one one
let add = |m||n||s||z| m s (n s z);
```

Ending in `app` ensures all our let bindings have a body.
Possible parses produced by expression don't stop there, however.
It parses this sequence of lets well, but it's just as happy to parse `3`.
Lacking a `let` keyword we don't parse any let bindings, and if `app` sees a single atom it returns it as is.
`3` turns into the tree:

* Expr
  * IntegerExpr
    * `3`

Wow! `expr` really does it all.
There's one more important role `expr` fulfills.
The role of a `program`.
For our base language, a program _is_ an expression.
We simply lack anything else it could be. 
Expressions being programs makes our `program` parser straightforward:

```rs
fn program(&mut self) {
  self.with(Syntax::Program, |this| {
    this.whitespace();
    this.expr(hashset![Syntax::EndOfFile]);
    if !this.input.at(Syntax::EndOfFile) {
      this.recover_until(hashset![], vec![Syntax::EndOfFile]);
    }
  });
}
```

Maybe not as straightforward as we thought.
Why isn't it just a lone call to `expr()`?

Recall that each call to expect eats up trailing whitespace when it consumes a token.
But there's nothing to eat up any leading whitespace before we've called expect.
We don't need to eat up leading whitespace for every expression, most expression are preceded by _something_.
`program` by definition is preceded by nothing, however, so it must handle leading whitespace.
Otherwise, our parser would be totally stumped by `"\n 3"`.

After calling `expr()` we have some program-specific error handling.
If we have any trailing tokens (i.e. we're not at `EndOfFile`), treat that as an error.
This handles a previous invalid example of ours:

```
let one = |s||z| s z;
add one one
let add = |m||n||s||z| m s (n s z);
```

We parse the valid expression `let one = |s||z| s z; add one one`, but then discover we're at LetKw and not end of file.
Our call to `recover_until` consumes the rest of our input and mark it as an error.

`program` is our top level parser, but it's still a method on our `Parser` struct.
Our entry point to parsing is a top level `parse` function that takes in a string and returns our CST and any errors that ocurred:

```rs
fn parse(input: &str) -> (GreenNode, Vec<ParseError>) {
  let mut parser = Parser::new(input);
  parser.program();
  (parser.builder.finish(), parser.errors)
}
```

We always return both a tree and any errors as part of being resilient.
Upon a successful parse, our errors will be empty and that's fine.
We did it!
We're parsing, and we didn't even get that suck in the tarpit.
Next up we'll handle our syntax sugar by desugaring.
