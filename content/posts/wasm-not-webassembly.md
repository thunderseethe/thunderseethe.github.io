+++
title = "Wasm Does Not Stand for WebAssembly"
date = "2025-07-15T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Wasm"]
series = []
keywords = ["Programming Languages", "Compiler", "Functions", "IR", "Compiler", "Emission", "Code Generation", "WebAssembly", "Wasm"]
description = "A case for why Wasm is neither Web nor Assembly"
+++

"Wasm does not stand for WebAssembly" might sound like an outrageous claim.
A glance at [webassembly.org](https://webassembly.org) confirms it's not just how it sounds:

![A photo of webassembly.org confirming Wasm does in fact stand for WebAssembly](/img/webassembly_org.png)

Fortunately, I've never been one to let reason stand in the way of a good point.
Let's talk about Wasm's relationship to the name WebAssembly.

As a proponent of Wasm (lengthened _WebAssembly_), I spend a lot of time preaching its positives to the people.
Whilst standing on my soapbox, I encounter two common misunderstandings about Wasm:

* Wasm is a web technology
* Wasm is an assembly language

Understandable mishaps, given the name, but they cause folks unfamiliar with Wasm to write it off and never delve deeper.
This is a shame because Wasm has a lot to offer none of which is a web technology or an assembly language.
I'd like to clear up these misunderstandings and convince you of Wasm's potency as a compilation target.

If you pull the WebAssembly mask from Wasm's face, you'll find it's actually [bytecode](https://en.wikipedia.org/wiki/Bytecode).
Rather than an assembly language such as x86-64 or Arm, Wasm has more in common with JVM or .NET bytecode.
Wasm, being bytecode, is run on a [virtual machine](https://en.wikipedia.org/wiki/Virtual_machine#Process_virtual_machines) (VM), not a real CPU.
This virtual machine lacks any browser specific functionality, as of writing this article Wasm lacks a way to interact with the DOM at all.
You could write a virtual machine for Wasm that runs on the [server](https://github.com/bytecodealliance/wasmtime), or on an [embedded device](https://embedded-wasm.github.io/book/02-getting-started).
[Harfbuzz](https://github.com/harfbuzz/harfbuzz), a prominent font shaping engine, lets you include [Wasm in a font](https://github.com/harfbuzz/harfbuzz/blob/main/docs/wasm-shaper.md) to perform custom shaping, a use case that both allures and alarms me.

How can they be sure a surreptitious font won't execute malicious code that steals all my data or even worse signs me up for their newsletter?
The secret lies in the execution model of Wasm.
Any VM that wants to call themselves a Wasm Runtime must conform to the [Wasm specification](TODO).
A large part of this spec is dedicated to ensuring that execution of Wasm is sandboxed from the host.
If the host doesn't explicitly expose the ability to interact with the filesystem, talk to the network, etc. Wasm can't do it by default.
Harfbuzz doesn't expose anything to the Wasm embedded in a font, so it can trust that Wasm won't do anything malicious.

The security offered by this sandboxing is one of the major selling points of Wasm.
It can offer value where-ever folks want to allow arbitrary execution, but don't want to get pwned.
Turns out that is many places in and outside the web.

## THE WEB ASSEMBLY IN NAME ONLY, FOR IT IS NEITHER

So what's going on here?
If Wasm isn't assembly, and isn't particularly web, why the name?
We can find the answer in Wasm's origin story.
Circa 2015, software engineering found itself in a conundrum.
Programmers desperately wanted to not write Javascript, but simultaneously desperately wanted to run their apps in the browser.

[Attempts](https://dart.dev/) were made to add a new "better" language to the browser, but these had failed, adding a new surface language could only ever kick the can down the road.
Eventually, folks will pine to avoid the new language alongside Javascript.
A new language also does nothing to help folks with existing codebases that want to bring them to the web.
They're still faced with the equally unsavory task of rewriting their app in the new language, rather than Javascript.

Another solution was needed.
If browsers only speak in Javascript, we simply have to teach our compilers to speak Javascript.
[asm.js](https://en.wikipedia.org/wiki/Asm.js) arose to meet this need.
It's a subset of Javascript intended to be targeted by compilers.
asm.js is a technical marvel, providing performance much closer to native that "normal" Javascript.

Did you know, in Javascript, if you bitwise or a value with `0`, as in `x|0`, the output must be an integer?
Neither did I, but the Javascript VM knows and will optimize `x` as an integer rather than the default slower Number class.
asm.js uses all sorts of tricks like these to entice the VM into efficiently executing our compiled language.
It's cool you can do this, but cool like a Rube Goldberg machine, not Archimedes' Law of the Lever.

Our compiled language already knew our value was an integer, and our VM already knows how to handle integers.
We only have to employ tactics like these because Javascript is mediating our execution.
Wouldn't it be nice if instead of talking to Javascript, we could talk directly to the VM we know all browsers contain?
Browsers, however, don't provide a single VM, each browser provides its own VM (V8 might be in 90% of browsers, but it still technically has alternatives) with accompanying API differences that change how we target them.
Javascript provides a unified interface, but as we already noted it's not ideal for our purposes

Wasm answered the call to save us from our conundrum.
It provides a new unified interface to browser execution that is far more amenable to compiled languages.
Thus, we start to see how the name WebAssembly arose.
Building on the work of asm.js, Wasm provides a new low level interface to the browser.

## They Knew the Whole Time

While this origin explains the name, it doesn't encompass the implementation.
The creators of Wasm set out not only to create a new web interface, but a new specification for general computation.
Andreas Rossberg (one of the creators of Wasm) describes [WebAssembly as a name to convince management to fund the project](https://youtu.be/Lb45xIcqGjg?si=wpnZkBq7iGtXa4t4&t=230) (not just once, but [twice!](https://youtu.be/pq-Pa2Fj4nE?si=cgc8onwsoISs_i14&t=138)).
The team behind Wasm was never intending to make just a web technology.

The portable bytecode they designed for Wasm is widely applicable and lacks all the web specific tech of Javascript.
It's also quite high level, making it a much easier compiler target than assembly.
It includes features such as exceptions, structs, higher order functions out of the box without requiring mapping them to lower level constructs.
There's even talk of adding [typed continuations](https://github.com/WebAssembly/stack-switching/blob/main/proposals/stack-switching/Explainer.md) to the language to support async and other delimited control flow patterns.

You can produce Wasm from LLVM, allowing languages like C, Swift, Rust, etc. to target Wasm, but this is leaving value on the table.
Make no mistake, this is the killer app for Wasm.
Your existing Rust app can be compiled to Wasm, and suddenly it runs on the web.
The ability to translate the huge swath of code that depends on LLVM into Wasm paves the way for further investment.

But translating LLVM to Wasm feels like talking through Javascript again.
LLVM's IR uses a unstructured control flow (i.e. basic blocks) whereas Wasm uses structured control flow exclusively (i.e. while, if, functions, etc.).
To translate from unstructured control flow to structured you need an algorithm like [Relooper](https://mozakai.blogspot.com/2012/05/reloop-all-blocks.html).
C, Swift, and Rust all started with structured control flow, LLVM converted those into basic blocks, only for us to turn them back into structured control flow for Wasm.
This is a necessary evil for existing languages.

As a compiler author conceiving a new language, however, you can emit Wasm directly without winding workarounds.
This is an undersold advantage of Wasm, especially with the [garbage collection](https://github.com/WebAssembly/gc) proposal becoming standardized.
Wasm lets you [skip the backend](posts/emit-base) turning a very high level IR into Wasm that can be executed immediately.
If you're invested in writing the backend of a compiler for love of the game, great, follow your heart.
No one is writing a compiler for the money anymore.

## A Taste of Wasm

If you're planning to target LLVM IR because you couldn't care less about the backend and want to get it over with, so you can return to fiddling in the typechecker, consider Wasm as a higher level target that will get you back to fiddling faster.
We've talked a lot about what Wasm isn't.
Now we'll touch, briefly, on what it is.
For full details, [mdn](https://developer.mozilla.org/en-US/docs/WebAssembly/Guides/Understanding_the_text_format) has a great introduction to Wasm.

Wasm is [fully formally specified](https://webassembly.github.io/spec/core/).
The spec has improved invaluable to me countless times when trying to puzzle out why my code was misbehaving.
Because all behavior is specified, they can list out the exact spots where [nondeterminism occurs](https://github.com/WebAssembly/design/blob/main/Nondeterminism.md).
If you're not using any of the functionality on that list, congratulations your Wasm is deterministic.

This can feel subtle, but is a huge boon as a compiler writer.
It's very easy when translating from surface syntax to machine code to let variance sneak in.
Maybe you used a hash map instead of an ordered map and now your compiler's output changes with the seed of your hash algorithm.
Maybe you didn't stabilize assigning IDs to variables and two variables keeps swapping between ID 2 and 3.
These things happen.
Wasm's determinism means you can be confident it's not where the bug lies, removing a huge swath of bugs you need to check when trying to figure out why your compiler broke.

Speaking of removing huge swaths of bugs, another outcome of Wasm's formal specification is static typing.
Wasm instructions, and Wasm's functions, modules, exports, etc. are all typed.
These types are validated before execution, ensuring the Wasm you've carefully spewed from your backend makes sense.
Typing can cause concerns that we're wasting a bunch of time inferring types or checking overloads.
Worry not.
You have to define all your Wasm types ahead of time, all Wasm does is check your work.
A speedy endeavor on a bad day.
 
Wasm needed the name WebAssembly to get off the ground, but the name shackled Wasm with some misconceptions.
Hopefully, I've sweeped away those misunderstandings to reveal a clearer picture of Wasm.
If I've wet your whistle for Wasm, there are a ton of ways to get started.

Rust is host to the premier non-browser Wasm runtime [wasmtime](https://wasmtime.dev/).
The developer of wasmtime, bytecode alliance, provides a whole host of [Wasm tooling](https://github.com/bytecodealliance/wasm-tools/tree/main).
Of those tools, the [wasm-encoder](https://docs.rs/wasm-encoder/0.235.0/wasm_encoder/) crate is especially helpful to compiler-smiths.
It provides a convenient type safe API to encode binary Wasm modules.

Outside the realm of Rust, [binaryen](https://github.com/WebAssembly/binaryen) is a library aiming to make compiling to Wasm easy, fast, and effective.
It provides APIs in C and Javascript.
Whatever your language of choice is, binaryen is almost certainly only an FFI away.
Built atop the binaryen, [wasm-opt](https://github.com/WebAssembly/binaryen?tab=readme-ov-file#binaryen-optimizations) provides a CLI that takes in Wasm and returns optimized Wasm.

[The WebAssembly Binary Toolkit](https://github.com/WebAssembly/wabt) provides another toolkit of CLIs that makes working with Wasm easier.
Notable entries in this toolkit are wasm2wat and wat2wasm, converting between the text and binary format which is invaluable for debugging.

There are a ton of ways to get started with Wasm.
If you're feeling pioneering, you can even slap some strings together into the shape of Wasm's [text format](https://webassembly.github.io/spec/core/text/index.html) and compile it in your local browser using [the Javascript API](https://developer.mozilla.org/en-US/docs/WebAssembly/Guides/Using_the_JavaScript_API)

