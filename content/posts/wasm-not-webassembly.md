+++
title = "Wasm Does Not Stand for WebAssembly"
date = "2025-06-15T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Wasm"]
series = []
keywords = ["Programming Languages", "Compiler", "Functions", "IR", "Compiler", "Emission", "Code Generation", "WebAssembly", "Wasm"]
description = "A case for why Wasm is neither Web nor Assembly"
+++

* Outrageous claim
  * Cursory glance at WebAssembly.org confirms
  * Never one to let reason stand in the way of a good point

* Often seen WebAssembly cause misunderstandings
  * Wasm is neither web nor Assembly
  * Andreas Rossberg has confirmed, jokingly, wasm does not stand for webassembly
    * Links
  * WebAssembly was how it was billed to get funded but was never the goal of the project

* What is it then?
  * Portable bytecode
  * Language Agnostic
  * Formally Specified

* Not Assembly
  * Stack machine
  * Infinite registers
  * Supports high level features like garbage collection and exceptions

* Not web
  * Used outside the web
  * Wasmtime
  * WASI
  * Servers
  * Games (this is a stretch)

* No free lunch
  * Won't pretend there's no web influence on Wasm
  * GC is a hulking addition to the spec
    * Naively, this enables support for python/lua/ruby/etc.
    * More cynanically, this additions makes a ton of sense if you're already a browser and have a javascript garbage collector lying around.
  * Features are only admitted to the standard if they're adopted by the browser's engines.

* Great compilation target
  * Skip the backend
  * Highlevel features allow you to go from a very high level IR directly to wasm
  * Tooling ecosystem lets you generate really bad code and lean on wasm-opt ot optimize it
  * Text format allows for cobbling together WAT and converting to wasm
    * Huge boon to debugging

