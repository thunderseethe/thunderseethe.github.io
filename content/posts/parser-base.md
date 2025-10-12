+++
title = "Reproachfully Presenting Resilient Recursive Descent Parsing"
date = "2025-09-10T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Parsing"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Parsing", "Recursive Descent", "LL(1)", "Lexing", "Concrete Syntax Tree", "Error Recovery"]
description = "Parsing syntax for our base language with error recovery"
+++

# Current outline

I must profess a certain disdain for the parsing tutorial.
I won't insult you by pretending it's fair or rationale.
This may not come as a surprise, given the making a language series has covered [every](/posts/emit-base) part of the compiler it can before parsing.
Parsing tutorials just feel so feckless, sharing an unflattering kinship with the monad tutorial.
Thousands of tomes written about parsing, and yet good PL projects still die bikeshedding syntax.

Ultimately, parsing performs two roles in a compiler:

  * Check a source file contains syntax the compiler recognizes
  * Create an in memory representation that's easier to work with than a big string
    * We're doing compilers, so you _know_ this will be a tree

In theory a simple task.
Pick some syntax that's easy to parse, build a tree out of it, and maybe report an error when users give you invalid code.
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

Each of these comes with their own tradeoffs and will make it easier or harder to parser certain syntaxes.
There's a lot to decide, and I don't have the answers.
Yet, we need a parser.
We can hardly say we've made a language if users have to hand assembly bits into our AST and feed that into our running compiler.
Our language needs an interface end users can write and provide us, and that's syntax in a source file.
Despite my misgivings, we need a parser.
And so I place my parsing platitudes upon the pile.
Text must turn to trees.

## We gotta parse somehow

Parse we must, but that doesn't help us navigate the many choices that remain in doing the parsing.
Our goal here is humble, get in, get a parser, get out.
Do not get caught in the tarpit.
We're not here to design the best syntax, or the optimal parsing strategy.

Doable and passable define our methodology today.
To that end, we'll gather some constraints to help us pick our preferred parsing platform.
First and foremost, is that our parser is handwritten.
There are a bajillion parser generators and libraries that will take some kind of grammar or config file and spit out a parser.
But my experience with these tools is you have to spend quite a bit of time learning the tool and can never quite tune the knobs to do what you want.
They can be a boon, but they also can be a time sink.
We won't need them, and we can save time by avoiding them off the bat.

After handwritten, we have two more constraints that are informed by the goal of our language.
In the modern era languages (and their compilers) are expected to be more interactive than the bygone days of batch compilation.
It's not table stakes for any new language on the block to support IDE like features, usually via the [Language Sever Protocol](TODO).
As a modern language ourselves, we want to be able to support those fancy features.

For the parser that takes the shape of two requirements:

* Resilient
* Full Fidelity

Resilience means our parser does not stop at the first error.
This might sound a bit javascript-y at first, but stay with me.
Because our language is interactive it will often see source code while the user is editing it.
If our parser stops at the first error, anytime our user modifies a function, which will temporarily invalidate its parse, we become unable to parse any following functions in the file.
Rather than stop at the first error, we want to contain that first error but pickup parsing valid syntax as soon as possible.

Our second constraint, full fidelity, is related to our ability to do this.
Full fidelity means our parser is going to represent every character of source text in the tree it constructs.
Often this is called a concrete syntax tree (CST) as opposed to an abstract syntax tree (AST).
Our CST will help us with error recovery as it allows us to insert invalid syntax into our tree.
It also helps us with the LSP functionality we want to support.
A lot of LSP operations take the shape of give me the semantic information for the whatever my cursor is pointing at, and CSTs make that easier by bridging between position in the source file and position in the AST we're going to construct.

With all those constraints in mind our parsing strategy will be [Recursive Descent](https://en.wikipedia.org/wiki/Recursive_descent_parser).
It is straightforward to handwrite.
A recursive descent parser is really just a set of functions that call each other to parser our input.
Each function will look at the current input and decide how to proceed.

If we want to parse an integer, we'd write an `int` function:

```rs
fn int(mut input: &str) -> (Result<isize, std::num::ParseIntError>, &str) {
  let mut integer = String::new();
  while let Some(digit) = input.get(0).filter(|c| c.is_digit()) {
    integer.push(digit)
    input = input[1..];
  }
  (integer.parse::<isize>(), input)
}
```

We can see it consumes input as long as the next character is a digit.
Once it runs out of digits, we try to parse our collection into an `isize`.
Regardless of our parses result, we return the remaining input.

We can then use our `int` function to build more complicated parsers.
For example, we can parse a binary expression using:

```rs
struct BinExpr {
  left: isize,
  op: char,
  right: isize
}

fn bin_expr(input: &str) -> (Result<BinExpr, ParseError>, &str) {
  let (left, input) = int(input);
  let left = left.map_error(|err| (Err(err.into()), input))?;
  let mut input = skip_whitespace(input);

  let op = if input[0] == '+' || input[0] == '-' {
    let op = input[0];
    input = input[1..];
    op
  } else {
    return (Err(ParseError::Unexpect(input[0])), input);
  }

  let input = skip_whitespace(input);
  let (right, input) = int(input)
  let right = right.map_error(|err| (Err(err.into()), input))?;

  (Ok(BinExpr {
    left,
    op,
    right
  }), input)
}
```

A little bit longer, but we can still loosely follow the structure.
A binary expression is an integer, an operator, and another integer.
An operator is either `+` or `-` (in a real implementation we would use an enum rather than a `char`).
In between each of those we parse any optional whitespace that might be present.

That helps us see the descent portion.
We're writing functions that descend the callstack to parse our string.
Parsing starts at a big `program` function that parses our entire input and calls other functions to parse subtrees and builds them up into our resulting tree.
In contrast to some parsing approaches, such as [recursive ascent](https://en.wikipedia.org/wiki/Recursive_ascent_parser) that start at the bottom (the leaves of the tree) and construct higher level nodes based on the input they see.

It doesn't help us see the recursive portion.
The recursive refers to some of our parsing functions being recursive.
Consider `bin_expr` again, rather than only supporting integers, we might want it to allow for each operand to itself be another bianry expression.
We would achieve this by simply calling `bin_expr` recursively.
Care must be taken with this, as with all recursion, to ensure termination.
As we'll see, we have to design our real parser to avoid infinite recursion.

So that's recursive descent, but our example was neither resilient nor full fidelity.
`bin_expr` stops at the first error it encounters, and it throws away whitespace.
Fortunately, because recursive descent is just functions we just have to write the functions different.

## Writing the functions different

Our actual parser will produce a full fidelity tree.
To do that, we'll need a full fidelity tree.
We're not going to write this ourselves.
We could, but much like the union-find, there are production grade implementations in the wild we'll lean on.

For a CST, our library of choice is [rowan](https://crates.io/crates/rowan).
Rowan is used by rust-analyzer, so I trust it to do its job well.
Under the hood rowan uses a Red-Green Tree.
This is a common choice for CSTs (Swift, C#, and Kotlin also use this kind of tree).
If you're interested, you can find details in the [rust-analyzer book](https://rust-analyzer.github.io/book/contributing/syntax.html).

Our purposes are suited just fine by using the fruits of rowans labor.
What we need to know about rowan is that it stores a heterogenous n-ary tree.
We'll define an enum to hold all our various node kinds, named `Syntax`:

```rs
enum Syntax {
  Identifier,
  Integer,
  Functions,
  // ... the rest of our syntax nodes.
}
```

This is a C-style enum (no associated data), unlike our AST.
Each node in our tree will either be an internal node or a leaf.
An internal node contains an arbitrary number of other nodes as its children.
A leaf contains a span into the source text.
In either case, the syntax tags what kind our node is, but says nothing about its contents.

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

When the user gives us an operator we don't recognize.
It's even happily let us represent a binary expression as:

* BinExpr

Whoops, we forgot to give that one any children.
Much like dynamic typing, this freedom comes at a cost.
The CST will let us put anything in the tree, but that means it will happily let us introduce bugs into our parse by misconstructing our tree.
This is a tradeoff.
Swift generates a node for each CST with typed fields.
But then it has [gargantuan nodes](https://swiftpackageindex.com/swiftlang/swift-syntax/601.0.1/documentation/swiftsyntax/functiondeclsyntax/init(leadingtrivia:_:attributes:_:modifiers:_:funckeyword:_:name:_:genericparameterclause:_:signature:_:genericwhereclause:_:body:_:trailingtrivia:)) to represent all the possible failures states.

## Guess we need some syntax

Rowan will let us parse things in full fidelity resiliently, but we've still got to figure out what to parse.
This is a dangerous game.
Picking syntax is literally the worst case of [Wadler's Law](https://wiki.haskell.org/Wadler's_Law).
Thankfully we won't have comments, yet.

Luckily, we've arrived prepared today.
We already know where we're going, our AST:

```rs
enum Ast<V> {
  Var(V),
  Int(i32),
  Fun(V, Ast<V>),
  App(Ast<V>, Ast<V>),
}
```

We're going to need syntax for each of these nodes.
For variables and integers, our syntax mirrors our AST nodes pretty directly.
A variable will be an identifier.
  * TODO: Define identifier either here or beforehand
An integer will be an integer, for us that means a series of `0-9` digits.
We aren't going crazy with the numeric literals, yet.
These are straight forward to parse because they contain one, nebulously defined, "thing" unlike `Fun` and `App` which contain multiple "thing"s

Thing resides in quotes because we have yet to define what that means for our language.
At minimum, our smallest unit is a character, so each thing could be a character.
That will turn out to be onerous in practice, rather than consider each thing individually we'll group characters together to make them easier to work with.
This is accomplished by an initial pass over our input string called lexing.

## Lexing

We won't be covering lexing in great detail in this tutorial.
The goal of lexing is to take our input string and do a fast pass over it to group characters.
This is things like recognizing the string `"let"` is the keyword `let` or the string `"1234"` is a number literal (and not an identifier).
Our parser can then consume these tokens, making its job easier.
Importantly, we don't have to lex.
We could rely on our parser to group characters into tokens for us.

This technique, named [scannerless parsing](https://en.wikipedia.org/wiki/Scannerless_parsing),  works but needlessly complicates our parser for little benefit.
Our goal is to avoid the parsing tarpit.
To that end, we're happy to employ a lexing library to make life easy.
Our lexing library of choice is [`logos`](https://logos.maciej.codes/).
Under the hood it does a lot of gnarly work to make lexing fast, but we get to ignore all that and use its pleasant API.

Our tokens will be represented by an enum `Syntax`:

```rs
#[derive(Logos)]
#[repr(u16)]
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

`Syntax` holds variants for both our tokens and the nodes we will parse.
It will be used here as the output of `logos` and also as the enum to tag our `rowan` nodes in our CST.
This is just to keep things conceptually simple.
We could also keep tokens and nodes as separate types (at the cost of wrangling more types).

The first thing to notice is that `Syntax` derives `Logos`.
You might also notice it is `#[repr(u16)]`.
The representation is important for `rowan` but incidental for lexing.
`Logos` is a proc macro that lets us annotate variants with `#[token]` or `#[regex]`, as we can see in our definition of `Syntax`.

`token` takes a string literal and uses it to recognize our token in our input stream.
This suffices for all of our punctuation and keywords.
We always know how they are spelled.
This won't suffice for identifiers, which are user provided.

For identifiers, we'll need `#[regex]` which takes a regular expression instead of a string literal.
An identifier is an alphabetic character or `"_"` followed by any characters in the word class `\w`.
You might expect our leading character to be `[a-zA-Z_]`.
We use the odder looking `\p{alpha}` because it supports Unicode and not just ASCII.
Unicode is table stakes in the modern era even for a toy language such as ours.

{{< notice info >}}
What do we do if our user wants to name a variable `"let_keyword"`?
How would we tell whether that's an identifier or the `let` keyword followed by identifier `_keyword`?
Logos will take the longest match out of our provided tokens.
Because Identifier will match `let_keyword` it's longer match will win out over LetKw's `let` match.
{{< /notice >}}

We use regex to recognize our integers and whitespace as well.
An integer is one or more digits, recognized by `\d+`.
Whitespace is one or more spaces, recognized by `\s+`.

We have one more token `EndOfFile` that lacks any recognizer.
It's marked `#[end]` because it has no text associated with it and logos will only produce it when it runs out of input.
We'll use it to make sure our program parses the entire source file.
That's all our tokens.

You may have noticed that our tokens don't cover the entire Unicode range.
You are correct. 
Our valid tokens are only a small subset of valid Unicode.
Users of our language, however, are free to put whatever they want in the source files they give us.
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

`Error` will represent both invalid tokens and invalid nodes.
In our lexer, whenever we see text we don't recognize we'll produce an `Error` token.
In our parser, when we see a tree that's not valid syntax we'll wrap it in an `Error` node.
With lexing out of the way we can now better design our syntax.

## Finis up syntax

We now have a better grasp on what we mean when we say a variable is a "thing".
It's a token, specifically an identifier.
Similarly, an `Int` is an `Int` token.

All `Var`s are identifiers, but not all identifiers are `Var`s.
A function also contains a "variable" but it's not a `Var` node.
It's the bound parameter of our function expression.
How will we know when we see an identifier whether it's a `Var` or a `Fun`?

The answer involves an important property of our syntax.
One strategy when we see an identifier is to hold off our decision until we see enough to know whether we're looking at a function or a variable.
While this can be effective at times, in general we want to design our syntax to avoid having to delay decisions like this.
Delaying our decision causes us to have to look ahead further into the input before and worst case can cause us to have to backtrack.

Backtracking is when we look ahead some amount, usually more than one token, and then realize our parse will fail and bail to try a different parse.
It is the bane of our existence because it can cause our parser to behave exponentially.
We want to avoid backtracking, so we will design our syntax to require a single token of lookahead to parse.
For our function vs var debacle, this means delaying the decision won't work.

Instead, we'll introduce extra syntax to inform the parser what it's seeing is a function and not a variable.
If we see a bare identifier, we'll always treat that as a variable.
If instead we first see a `|` (VerticalBar), we know we're parsing a function and the following identifier will be treated as a function parameter and not a variable.
We'll similarly note the switch from function parameters to function body with another `|` giving us our full function syntax:

```rs
|x| x
```

We'll ignore any similarities that might evoke to other function syntax.
This is our own unique language and it's very special!

Our last node, `App`, needs syntax still.
As a functional language, we expect to apply functions _a lot_.
Accordingly, we're going to give application a relatively terse syntax.
Any two things juxtaposed will be considered an application: `f x`, `x 1`, etc.

Perhaps a controversial choice, but our functions curry by default and this syntax is commonplace among the ML family of languages that champion that approach.
That covers all our `AST` nodes.
As it stands though that's not quite enough syntax to be a usable language in practice.
We have some more syntax we'll _need_ to get the job done, and then we'll have some syntax that's just cute, and we like having around.

A remaining issue with our syntax is how to tell when one expression stops and another starts.
Consider the string `|f||x| f x g`.
Is that string one function with a body `f x g` or an application of function `|f||x| f x` to argument `g`.
Whichever interpretation we pick, we'll need a way to say both.

For us that will be parenthesis.
We'll pick a way to parse `|f||x| f x g` (For the record we will parse that as one function.), and use parenthesis to specify when we want the other interpretation.
In this case that would be `(|f||x| f x) g`.
Parenthesis will also serve to disambiguate nested applications.
`x z y z` is read `x` applied to 3 arguments, and `x z (y z)` is `x` applied to `z` and a nested application `y z`.

Last we have some syntax we're throwing in for love of the game.
Such syntax is called syntax sugar because it exists purely syntactically.
We will be desugar the syntax into our existing `AST` nodes, effectively erasing it.
Because the syntax maps onto the same set of nodes we already support semantically, it costs us little to add to the language and makes it nicer to use.

We risk entering the tarpit here.
Syntax sugar being not strictly necessary opens us up to bikeshedding.
I think it's worth covering, however, as you'll be hard-pressed to find a language in production with zero syntax sugar.
It's important to cover how it is implemented and supported by IDE-like features.
We'll have to tread carefully.

Our syntax sugar is the let expression.
As you might have guessed from our sole keyword being `let`.
A let expression comprises an identifier, an expression that defines that identifier, tailed by an expression that has access to our freshly defined identifier.
For example `let two = add 1 1; add two two`.
Similar to function, our `let` tells the parser to treat the following identifier as a let binding rather than a variable.
We then use `=` and `;` to distinguish between the defining expression and the body expression.
We're free to nest lets, so this is also valid:

```
let two = add 1 1;
let four = add two two;
add two four
```

Let is syntax sugar because where-ever it appears we could instead write an applied function `(|two| add two two) (add 1 1)`.
This behaves exactly the same way, but I'd much rather write the `let`.
That covers all our syntax, we're finally ready to start parsing.

## Parsing

Our recursive descent parser comprises a bunch of functions that call each other recursively.
These functions will want to share some mutable state between them.
A pattern we've seen before in our typechecker.
We'll solve it the same way here by introducing a `Parser` struct to hold our state and making each function a method on that struct:

```rs
struct Parser<'a> {
  input: Input<'a>,
  builder: GreenNodeBuilder<'static>,
  errors: Vec<ParseError>,
  in_error: bool,
}
```

`errors` and `in_error` handle error reporting.
Parsing doesn't stop at the first error, so we keep a running vector of errors.
`in_error` prevents us from reporting multiple errors for the same span of text.
Once we start reporting one error, we don't want to report a new error that overlaps the initial error.

`builder` is from the `rowan` crate and is what maintains our CST.
Once completed our CST will be an immutable `GreenNode`, so we need a builder during parsing that we can modify.
Last but not least, `input` is a helper struct that manages our source text:

```rs
struct Input<'a> {
  content: &'a str,
  lexer: Peekable<logos::SpannedIter<'a, Syntax>>
}
```

`content` is just the source we're parsing.
We hang onto a reference to it so we can get the source text associated with our tokens.
`lexer` is a `logo::SpannedIter`.
An iterator that produces our `Syntax` tokens with their spans in the source string.
TODO: Introduce a span.
We call `.peekable` on that iterator to get our full `lexer` type.
Peeking allows us to lookahead one token without consuming it.
We'll need this to determine what to parse before committing to advancing our parser through the input.
  * Note we only support 1 token of lookahead. This is important for performance of our parser. Arbitrarily lookahead will degrade a linear time parser into an exponential parser.

We'll create some methods on `Input` to help us navigate our source text.
First is `peek` which lets us see the next token:

```rs
impl Input<'_> {
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
}
```

Logos does a lot of the heavy lifting here to produce `tok` which is a `Result`.
If that result is an `Err` we produce an `Error` token.
If our overall iterator returned `None`, we're out of source text and we return `EndOfFile`.
It's very common we'll want to check if we're at a particular token, so we add a helper `at` for that as well:

```rs
impl Input<'_> {
  fn at(&mut self, token: Syntax) -> bool {
    self.peek() == token
  }
}
```

Neither of these methods actually move us forward in our input.
They only check where we're currently at.
Progressing forward is accomplished by `advance`:

```rs
fn advance(&mut self) -> Option<Range<usize>> {
  let (_, span) = self.lexer.next()?;
  Some(span)
}
```

`advance` returns the span of the token we just passed.
`advance`, perhaps surpisingly, does not return the actual token we just passed, only it's span.
This is because calls to `advance` are basically always preceded by calls to `at`, so we already know what token we're _at_.
Our final helper `eat` makes this very clear by calling `at` immediately followed by `advance`:

```rs
impl Input<'_> {
  fn eat(&mut self, token: Syntax) -> Option<&str> {
    if self.at(token) {
      self.advance().map(|span| &self.content[span])
    } else {
      None
    }
  }
}
```

`eat` is the method we'll call the most on `Input`.
Given a `token`, if we're at that `token`, advance the input.
Makes sense if our parser sees what it expects to see, consume it and look at the next thing.
`eat` knows what to do on the happy path, but it says nothing about what to do when a `token` is missing.
If we're not at the right token, we simply return `None`.
That's because we need the state stored in `Parser` to handle our errors.

## Back to Parsing

Now that we're familiar with all our parsing state, we can begin the actual work of parsing.
Our recursive descent parser begins its execution by calling a top level parsing function.
This function parses our string top down, _descending_ into sub parsing functions recursively until we reach the leaf nodes of our parse.
A great way to parse, but not as great a way to explain how our parser works.
We'll instead take the opposite approach.
Start with our leaf parsing functions and then see how they're composed into higher level functions ending with our top level `program` parsing function.

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
Our happy path is straightforward:

```rs
fn expect(&mut self, token: Syntax, mut anchor: HashSet<Syntax>) {
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
This marks any ongoing error's span as ending before the token we ate and means we're now ready to report a new error the next time we see invalid syntax.
We also add a leaf node to our tree containing our token, and it's source text.
Finally, we eat any whitespace after this token.
We choose to do this here, so that we can have whitespace between any token and don't have to litter `whitespace` calls all over our parsing logic.
It's important that we only do this upon eating the expected token.

{{< notice note >}}
`ate` could return a `bool`, but `ControlFlow<()>` is equivalent and has nice semantically meaningful names for our usecase.
{{</ notice >}}

If we don't see the right `token`, `expect` will perform error recovery using `anchor_set`:

```rs
fn expect(&mut self, token: Syntax, mut anchor: HashSet<Syntax>) {
  // Our happy path above...
 
  // Otherwise, start error recovery
  // We can always recover to our expected token, so ensure it's in the anchor set.
  anchor.insert(token);
  self.recover_until(anchor, vec![token]);

  // ...
}
```

Our error recovery strategy is to discard tokens until we reach a token in our `anchor_set` at which point we know how to continue parsing:

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

The idea behind discarding tokens is to minimize the span of an error by resetting our input to a known good token.
For example, consider the input `"let  = foo 1; 2"`, which is missing an identifier for the let.
If we did no error recovery, our first expect call for an identifier would fail.
At which point we would consume no input and our subsequent expect `=` call would fail, because our input is still at the whitespace where an identifier should be.
Our missing identifier cascades into the rest of our let expression and our parse tree ends up being LetKw followed by all erroneous syntax.

But that's because we didn't consume any input, what if we skipped over a token on error to make sure we're moving forward?
It's trivial to concoct an example where that cascades catastrophically.
Simply removing the whitespace from our first example: `"let = foo 1; 2"`.
Now when we expect our missing identifier, we skip over `=` as an error.
But oh no, that means our expect call to `=` is now looking at `foo`.
It will then skip over that, and we're back to a cascade of errors.

We need a Goldilock skip to cover just the right amount of input to get us back on track.
To achieve this, we can't simply skip a static number of tokens.
Instead, we maintain the `anchor_set`.
A set of tokens we know will leave us in a good state to continue parsing.
By maintaining a dynamic set we can contextualize the anchor set for the current node we're parsing.
For example, in our previous example `"let  = foo 1; 2"` our anchor set would be something like `[Syntax::Equal, Syntax::Semicolon, Syntax::LetKw Syntax::EndOfFile]`.
We would know to discard our whitespace because it does not appear in our recovery set.
We stop, however, at `=` because it does appear in our recovery set.

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

If we didn't actually discard tokens, and we're not already in an error, we still need to emit an error to let the user know expected syntax was missing.
This is a matter of listing the set of tokens we expected (for `expect` this will always be one token) and a span to show the diagnostic.
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

We create a span that covers all our discard tokens.
As we do this, we also add an `Error` node to our tree containing all the tokens we discarded.
This only works because our CST is heterogenous.
It doesn't matter what the current node we're building is.
We're free to add an error node as child of any node.
Make no mistake, we'll have to deal with these someday, but not today!
If we're not already in an error, we emit the span as an error.

We have one final remnant in `expect`: 

```rs
fn expect(&mut self, token: Syntax, mut anchor: HashSet<Syntax>) {
  // Happy path...

  // Error recovery...

  let _ = self.ate(token);
}
```

We end `expect` with another call to `ate` that we do not check.
Error recovery might have left us at the token we expected.
In which case, we want to go ahead and consume it in this call, otherwise subsequent expect calls will error out.
This handles situations where our error is superfluous tokens leading up to our expected token.
Returning to our bountiful let example, imagine if it looked like this `"let x | = foo 1; 2"`.
We have a valid let in their, but with an extra `|` lurking.
Our expect `=` call will see the `|` discard it as erroneous and successfully recover to the `=`.
At this point, however, if we do not check for `=` again, we would move on with our parsing leaving `=` as our current input.

## Parsing Applications Atomically

That completes `expect`.
We can now employ it to being assembling our parsing ensemble.
Our end goal is to parse a program, which for us means an expression for the time being.
If you'll allow a spot of yak-shaving:
  * To parse a program, we must parse an expression
  * To parse an expression, we must parse lets and applications
  * To parse an application, we must parse something called an `atom`

An `atom` for us is any expression that isn't an application (mostly).
Syntax like `x`, `1`, and `|x| |y| foo x y` are all atoms, but things like `x 1`, and `foo x (|y| y)` are not.
Atoms are a distinction that exist purely in the parser.
After parsing you'll find no atoms in our CST.
They exist to avoid infinite recursion while parsing applications.

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
It's perfectly valid for our parser to call `expr() -> application() -> expr()`.
Without moving the input forward, and if the input never moves forward nothing will ever stop our parser.
We're in an infinite loop.

Avoiding this is a matter of being more precise in what we want to parse and ensuring that any recursive call to `expr` is only made after consuming input.
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

Quite a bit more complicated than our first attempt.
A small price to pay for finishing in finite time.
`app` starts by ensuring we have at least one `atom` (we'll pretend that's an expression for now).
An empty application is an error.

Next we check for a second atom, if we only have a single atom we return it as is.
This handles the special case where we don't have an application at all and just have a single expression.
Once we have two atoms we make use of the checkpoint we create at the top of our `app`.

A checkpoint, as the name might imply, saves an index into our tree and allows us to add nodes there later.
We need it here because we don't know we want to create an `App` node until we know we have two atoms.
But once we know we want the `App` node, we want it to encompass the two atoms we've already parsed.
The checkpoint lets us delay deciding if we need an `App` node or not.

Not just for a single `App` node, but any number of `App` nodes.
Part of the complications in our `app` parser is we no longer parse a single `App` node, but any number of them.
Our terminal `while` loop will happily consume atoms and construct apps as long as the input contains them.
This works because our checkpoint lets us add `App` nodes, with the correct precedence, atop previously created `App` nodes.

Okay, so an app is a series of one or more atoms, but what is an atom?
An atom is any expression that isn't an application (or a let binding).
For our language, that is:
  * Variables
  * Integers
  * Functions
  * Parenthesized Expressions

We can see this in the definition where we have a case for each of those things:

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
First I want to look at the overall structure of `atom`.
It returns a `ControlFlow<()>` (just like `ate`).
This is because, also like `ate`, `atom` doesn't always consume input.
If our current token isn't an expected one, we don't error, instead we return `Break`.

This optionality is how we're able to parse any number of atoms in `app`.
Correspondingly though, the call of `atom` is now responsible for dealing with whatever input it can't handle.
If we have a malformed application such as `"foo , 1"` it falls to `app` (or `app`s caller) to determine `,` is an error.
`atom` will simply return `break`.

When `atom` does see a token it recognizes, it commits to expecting that input.
For variables, this is an Identifier.
For integers, this is an Int.
In both cases, we wrap our token up as it's own syntax node.
This is purely for convenience, wrapping semantically meaningful tokens as nodes makes them easier to find among the whitespace when we're traversing our CST.

Atom has two more cases, that are more interesting than variables and integers.
Let's look at functions first:

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

A function takes the form `| <identifer> | <expr>`, and we can see that maps pretty directly onto our parsing code.
Again we wrap our Identifier up as its own node.
We also make it a function specific Node `FunBinder`.
We could reuse `Var`, but it will be helpful to distinguish between an identifier showing up as a variable expression vs one showing up to bind a new variable.

The body of our function is an expression.
Unlike our naive application, we don't have to worry about this forming a cycle.
Before parsing a new expression, we must have seen `|`.
We can be confident we've consumed input before recursing, so we won't recurse forever.

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
Another thing to note is we modify our anchor set.
Previously we've threaded through our anchor set but not actually done anything with it, but that finally changes now.
When we're within a parenthesized expression, and only then, we want to allow recovery to a following `)` if one is available.

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
    this.expr(unioning(&anchor, [Syntax::Semicolon, Syntax::LetKw]));
    this.expect(Syntax::Semicolon, unioning(&anchor, [Syntax::LetKw]));
  })
}
```

Our let parser doesn't parse the full let binding: `let <identifier> = <expr>; <expr>`.
It only parses `let <identifier> = <expr>;` with no trailing body expression.
We'll see why in `expr`, but I wanted to call it out here that's by design.

Let parsing also makes heavier use of our anchor set modifying it extensively.
At each expected token we want our anchor set to include the remaining synchronization points in our let binding.
For LetKw, this is an Identifier, Equal, or Semicolon because the remainder of our let binding after the keyword is `<identifier> = <expr> ;`.
For Equal, it's just semicolon because the remainder of our let binding at that point is `<expr> ;`.
We don't include anything from expression in our anchor se.
Expression doesn't make for a good synchronization point because it covers most of our syntax and we wouldn't know what state our input is left in if we recovered to an expr.
If we encounter a missing `=` we'll treat all of `<expr>` as an error and reset parsing at the following `;` (if present).
This is a tradeoff and more art than science.
In practice, I've found this to give pretty reasonable errors for not a lot of effort, but one could absolutely try to do something more sophisticated to recover here.

Honestly all this anchor set management is repetitive and rote.
I bet we could abstract a lot of this out pretty easily.
At each token we want our anchor set to contain the tokens following itself.
For each of our parsing rules, we could create a simple struct like:

```rs
struct Let(Vec<Syntax>);
```

We could configure our vector to be `vec![Syntax::LetKw, Syntax::Identifier, Syntax::Equal, Syntax::Expr, Syntax::Semicolon]` and use that to determine what should be in the anchor set "for free".
Now that you mention it we could also use that vector to determine the first token of each our parsed items.
Then `atom` wouldn't have to manually maintain a `match` it could just check if `peek` was in the first of it's four cases!
Not that you mention it again we could derive how to parse let bindings from our vector of Syntax.
We could make a trait with a `parse` function for each Synt-

Dagnabit the tarpit got me again.
Pretty soon we'll find ourselves writing a parsing library (or heaven forbid a parser generator) and _not_ a parser.
Our code is repetitive and rote, but it is also done.
We best be on to more interesting affairs before the tarpit further ensnares us.

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
How the heck does this thing work?

An expression is any number of let bindings followed by a singular `app` (but recall that `app` might be any number of applications).
Our repeated parsing of let is why `let_` doesn't have to parse a body expression.
Our the "body" of our let bindings will either be another let or an app.
If we see an expression such as:

```
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
The body of our let is implied by whatever follows it in our expression, and our expression is a flat sequence of lets endiending in an application.
It is important that our sequence is terminated by the app.
For example, this would be invalid syntax:

```
let one = |s||z| s z;
add one one
let add = |m||n||s||z| m s (n s z);
```

Ending in `app` ensures all our let bindings have a body.
Our expression is quite broad in what it can parse, however.
It parses this sequence of lets quite well, but it's just as happy to parse `3`.
Without a `let` keyword we don't parse any of those, and if `app` sees a single atom it will return it as is.
`3` turns into the tree:

* Expr
  * IntegerExpr
    * `3`

Wow! `expr` really can do it all.
There's one more important role `expr` fufills.
The role of a `program`.
For our base language, a program _is_ an expression.
We lack anything else for our program to contain.

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

While this is straightforward.
It's maybe not as straightforward as we thought.
Why isn't it just an immediate call to `expr()`?

Each call to expect eats up trailing whitespace when it consumes a token.
Which is very handy, but any leading whitespace has no preceding token to consume it.
Without that initial `whitespace` call our parser would be totally stumped by `"\n 3"`.
Not a great user experience.

After `expr()` we have some error handling to take care of.
If we have any trailing tokens (ie. we're not at `EndOfFile`), treat that as an error.
This handles a previous invalid example of ours:

```
let one = |s||z| s z;
add one one
let add = |m||n||s||z| m s (n s z);
```

We parse the valid expression `let one = |s||z| s z; add one one`, but then discover we're at LetKw and not end of file.
Our call to `recover_until` with the empty set will consume the rest of our input and mark it as an error.
Because we pass an empty set as our anchor we'll never recover.
Only stopping once we run out of input.

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
