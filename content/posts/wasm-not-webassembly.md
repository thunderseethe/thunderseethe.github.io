+++
title = "Wasm Does Not Stand for WebAssembly"
date = "2025-07-14T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Wasm"]
series = []
keywords = ["Programming Languages", "Compiler", "Functions", "IR", "Compiler", "Emission", "Code Generation", "WebAssembly", "Wasm"]
description = "A case for why Wasm is neither Web nor Assembly"
draft = true
+++

"Wasm does not stand for WebAssembly" might sound like an outrageous claim.
A glance at [webassembly.org](https://webassembly.org) confirms it's not just how it sounds:

![A photo of webassembly.org confirming Wasm does in fact stand for WebAssembly](/img/webassembly_org.png)

Fortunately, I've never been one to let reason stand in the way of a good point.
Let's talk about Wasm's relationship to the name WebAssembly.

As a proponent of Wasm (lengthened WebAssembly), I spend a lot of time preaching its positives to the people.
Whilst standing on my soapbox, I encounter two common misunderstandings about Wasm:

* Wasm is a web technology
* Wasm is an assembly language

Understandable mishaps, of course, but they cause folks unfamiliar with Wasm to write it off and never look past the name WebAssembly.
This is a shame because Wasm has a lot to offer and is neither a web technology nor an assembly language.
I'd like to clear up these misunderstandings and shine some light on Wasm as a compilation target.

If you pull the WebAssembly mask from Wasm's face, you'll find it's actually portable bytecode.
Rather than an assembly language such as x86-64 or Arm, Wasm has more in common with JVM or .NET bytecode.
Wasm, being bytecode, is run on a virtual machine (VM), not a real CPU.
This virtual machine, however, lacks any browser specific functionality, as of writing this article Wasm lacks a way to interact with the DOM.
You could write a virtual machine for Wasm that runs on the [server](https://github.com/bytecodealliance/wasmtime), or on an [embedded device](https://embedded-wasm.github.io/book/02-getting-started).

## What's in a Name?

Okay, so how did we get here?
If Wasm isn't assembly, and isn't particularly tied to the browser, why the name?
We can find the answer in Wasm's origin story.
Circa 2015, software engineering found itself in a conundrum.
Programmers desperately wanted to not write Javascript, but simultaneously desperately want to run their app on the browser.

[Attempts](https://dart.dev/) were made to add a new "better" language to the browser, but these had failed.
Adding a new surface language just kicks the can down the road anyway. 
Eventually, folks will pine to avoid Javascript and the new language.
This gave rise to languages compiling to Javascript.

If browsers only speak in Javascript, we simply have to teach our compilers to speak Javascript.
[asm.js](TODO) arose to meet this need.
It's a subset of Javascript intended to be output by compilers and provide performance closer to native code.
asm.js is a technical marvel.

Did you know, in Javascript, if you bitwise or a value with `0`, as in `x|0`, the output must be an integer?
Neither did I, but it's true, and the VM knows this and will optimize that value to be an integer rather than the more generic Number class.
asm.js uses all sorts of hacks like these to trick the VM into efficiently executing our compiled language.
It's cool you can do this, but it really feels like bending over backwards to arrive where we started.

Our compiled language already knew our value was an integer.
We only have to employ tricks like these because Javascript is mediating our execution.
Wouldn't it be nice if instead of going through Javascript, we could directly target the VM we know browsers have with our compiled language?
Browsers, however, don't provide a single VM, each browser provides its own VM (V8 might be in 90% of browsers, but it still technically has alternatives) with API differences that affect how we target them.
Javascript provides one such unified interface, but as we already noted it's not ideal for our purposes.

Wasm arrives to save us from our conundrum.
It provides a new interface to browser execution that is far more amenable to compiled languages.
We start to see how the name WebAssembly came about.
Building on the work of asm.js, Wasm would provide a new lower level entrypoint to the browser's VM.

## They Knew the Whole Time

While this origin explains the name, it doesn't explain the implementation.
The creators of Wasm set out to create a universal execution format that could meet the needs of a new browser VM interface, but wasn't limited to it.
Andreas Rossberg (one of the creators of Wasm) describes [WebAssembly as a name to convince management to fund the team](https://youtu.be/Lb45xIcqGjg?si=wpnZkBq7iGtXa4t4&t=230) working on Wasm.
The team behind Wasm was never intending to make just a web technology though.

Their goal from the outset was to build a platform for computation that happened to work on the web, but also happened to work anywhere else.
The portable bytecode they designed for Wasm is quite generic and lacks all the web specific tech of Javascript.
As bytecode, it's also much higher level than assembly, making it a much easier compiler target.

You can produce Wasm from LLVM, allowing for languages such as C, C++, Rust, etc. to target Wasm.
But this is leaving value on the table.
LLVM's IR uses a unstructured control flow (i.e. basic blocks) whereas Wasm uses structured control flow exclusively.
To translate from unstructured control flow to structured you need an algorithm like [Relooper](https://mozakai.blogspot.com/2012/05/reloop-all-blocks.html).

It feels like we're taking one step back.
C, C++, and Rust all started with bundles of functions, LLVM converted those into basic blocks, only for us to turn them back into bundles of functions for Wasm.
This is a neccessary evil for existing languages.
Being able to emit Wasm from LLVM opens the door for a huge existing ecosystem of tooling to interface with Wasm.

As a compiler author writing a new language, however, you can emit Wasm directly and make full use of it's high level features.
I think this is an undersold advantage of Wasm, especially with the [garbage collection](https://github.com/WebAssembly/gc) proposal becoming standardized.
Wasm lets you [skip the backend](posts/emit-base) turning a very high level IR into Wasm that can be executed immediately.
If you're invested in writing the backend of a compiler for love of the game, great, follow your heart.
No one is writing a compiler for the money anymore.

## A Taste of Wasm

If you're planning to target LLVM IR because you couldn't care less about the backend and want to get it over with, so you can return to fiddling in the typechecker, Wasm is worth consideration as a higher level bytecode target that will get you back to fiddling faster.
Wasm is [fully formally specified](https://webassembly.github.io/spec/core/).
The spec has improved invaluable to me countless times when trying to puzzle out why my code was misbehaving.
Wasm is sandboxed and mostly deterministic, because all behavior is specified they can list out the exact spots where [nondeterminism occurs](https://github.com/WebAssembly/design/blob/main/Nondeterminism.md).

We've talked a lot about what Wasm isn't.
Now we'll touch, briefly, on what it is.
For full details, [mdn](https://developer.mozilla.org/en-US/docs/WebAssembly/Guides/Understanding_the_text_format) has a great introduction to Wasm.

Wasm is a great compilation target because it's high level and fully formally specified.
When I say high level, I don't mean it in the traditional sense one might say python is high level.
I mean as a compiler author Wasm will do a lot for you.

Wasm uses a stack machine as it's model for execution, a common choice among virtual machines.
Atop this stack machine it allows any number of local values, that fill a similar role to registers.
You never have to perform register allocation, you can use as many registers as you want.
Which is not to say you can't do register allocation.
You're still free to do register allocation and efficiently reuse the minimal number of locals your expression requires.
That freedom to write scuffed code that spews out locals initially, and incrementally improve it later is part of the appeal.

Your compiler can spit out suboptimal Wasm, and you can feed it into existing tooling like [wasm-opt](TODO) to get better code by standing on the shoulders of giants.
LLVM, of course, shares in a lot of this ability to offload backend tasks.
LLVM doesn't share in the formal specification Wasm provides.
Wasm being formally specified is perhaps controversial, depending on your feelings about static typing.

I've found it invaluable in my work.
Anytime I emit invalid code, which is often, I can look in the spec and pretty immediately ascertain where I've gone awry.
Much like static typing, the spec helps immensely with comprehension.
The advantages extend to the runtime as well.
One reason to use a VM is security, VMs allow you to isolate execution, so it can't modify the host in arbitrary ways.
Wasm's spec allows it to be confident execution won't stray from it's intended course.

A neat outcome from this is Wasm can list out everywhere that [nondeterminism occurs](https://github.com/WebAssembly/design/blob/main/Nondeterminism.md).
If you're not using any of the functionality on that list, congratulations your code is deterministic.
You'll never have to re-run a failed test wondering if it's flaking.
For a given input, Wasm will give you the same output consistently.

This can feel subtle, but is a huge boon as a compiler writer.
It's very easy when translating from surface syntax to machine code to let variance sneak in.
Wasm's determinism means you can be confident it's not where the bug lies, removing a huge swath of cases you need to check when trying to figure out why your compiler produces invalid code.

## No Free Lunch

Hopefully I've convinced you Wasm is neither Web nor Assembly.
I won't pretend, however, that just because Wasm isn't a web technology browsers have no influence over Wasm.
The Wasm GC proposal is great, but it's also a gigantic addition to the spec and a huge burden to any new Wasm implementations.

Optimistically, it opens the door for GC'd languages to target Wasm without each having to compile their own GC into the final binary.
Lua, Python, and Java can all happily share Wasm's GC now rather than each providing their own.
More cynically, browser's already have world-class GCs lying around, and this proposal paves the way for Wasm to leverage them at the expense of competing implementations.
Not that it has to be one or the other, but I'm not convinced the GC spec would look how it does if browsers didn't already provide GCs to all the major web implementations of Wasm.
If you look at the [feature support](https://webassembly.org/features/) page, every browser supports GC but only a single non-browser engine supports it.

Wasm does not have support for native DOM operations, but they're working hard to remedy that.
Again optimistically, they'll do this in a generic way that is applicable outside the browsers scope.
Perhaps with a generic FFI-like method to expose host functionality more directly to Wasm.
But it can't be argued the needs of the browser guide Wasm's continued development. 

There are three [features](https://webassembly.org/features/) being standardized right now that deal with Javascript interop.
Clearly the web influences Wasm.
As long as the current team helms the ship, I believe they'll propose features that are generally applicable but also meet the needs of the browser.
But eventually leadership will rotate, and we might see more browser specific functionality creep in.
In the interim, maybe server-side Wasm can become a big enough use case that it can compete with the browsers influence, but who knows what the future holds.
 
Wasm needed the name WebAssembly to get off the ground, but the name has shackled Wasm with some misconceptions. 
Hopefully, I've cleared up some of those misconceptions and shone 
