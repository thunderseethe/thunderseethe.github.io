+++
title = "Skipping the Backend by Emitting Wasm"
date = "2025-05-17T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Emit"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Functions", "IR", "Compiler", "Emission", "Code Generation", "WebAssembly", "Wasm"]
description = "Turning our Base IR into executable WebAssembly"
+++

Today is a great day.
We stand together on the precipice of execution.
All the work we've done till now culminates in our final compilation pass: Code Generation.
Our compiler's tires meet road, as we take our closure converted `IR` and turn it into executable code, driving off into the sunset.

For the more seasoned compiler authors, this may come as a surprise.
Closure conversion is traditionally far from the last pass in a compiler, and usually sits more towards the middleend rather than the backend.
Worry not, this is absolutely still the case.
We haven't shifted closure conversion closre to the backend, rather we've vaulted over the backend entirely.
The secret to generating code from the middleend of the compiler lies in our code generation target: [WebAssembly](https://webassembly.org/) (Wasm).

Wasm as a compiler target is quite high level.
It allows us to eschew lower level backend passes such as:

* [Register Allocation](https://en.wikipedia.org/wiki/Register_allocation)
* [Liveness Analysis](https://en.wikipedia.org/wiki/Live-variable_analysis)
* [Instruction Selection](https://en.wikipedia.org/wiki/Instruction_selection)

## Bytecode (WIP Name)

Wasm is able to skip over these passes because it's a [bytecode](https://en.wikipedia.org/wiki/Bytecode) format, despite the implications of the name WebAssembly.
Bytecode is not executed directly by a CPU, but is executed by a Virtual Machine (VM) that translates the bytecode into assembly and executes it.
Wasm shares more DNA with the likes of the [JVM](https://en.wikipedia.org/wiki/Java_virtual_machine) than [x86-64](https://en.wikipedia.org/wiki/X86-64).

Our choice to use bytecode, while pragmatic, is more of a personal preference rather than a utilitarian decision.
My interest in languages is definitely focused on the semantic analysis, hence why we started with the typechecker.
There's a lot of interesting design to be done on the backend, but it does not hold my interest.
Bytecode lets us get something working end to end by standing on the shoulders of giants.
The same way The Cloud is just someone else's computer, The Virtual Machine is just someone else's backend passes.

Bytecode is higher level than assembly, on a fundamental level that's why it exists, but it's still lower level than the `IR` we produce from closure conversion.
Our `IR` is a tree where each expression can contain arbitrary sub expressions, whereas bytecode is a list of operations to be performed.
We'll have to hammer our `IR` flat before it will fit into a list.
Bytecode's operations are also simpler than those available to our `IR`.
There isn't a construct a closure operation in Wasm's bytecode, unfortunately.
We'll have to decompose our more complex expressions into multiple operations.

Bytecode operations resemble assembly.
They are mostly composed of arithmetic and shuffling memory around.
For example the Wasm to add two integers is:

```lisp
(local.get $1)
(local.get $2)
(i32.add)
(local.set $3)
```

We'll touch on the details of this in a moment, but for now notice this looks quite a bit closer to assembly than our `IR`.
We fetch two integers out of registers `$1` and `$2`, add them with `i32.add`, and write the result back to `$3`.
While this does resemble assembly, there are some important differences.
If this were actually assembly, we'd have to know the specific registers available to our CPU rather than the abstract `$1`, `$2`, and `$3`.
We also probably want to do something with our sum after calculating it.
In which case, we'd have to know what register a return value should be written to.
We're also responsible for managing our own stack in assembly.
We have to know which register holds a pointer to the stack and update it when we return from a function.

Bytecode offloads these concerns to the virtual machine.
This eases our conceptual burden and improves portability.
We don't have to know how to emit the assembly of any CPU we target.
We just need someone else to have written a virtual machine for that CPU.

Veterans in the compiler space might again be saying: "Well hang on, doesn't [LLVM](https://www.llvm.org/) provide all of those same benefits while producing native code?".
And to those folks I would say: "Hey! Stop saying that!".
See my previous personal preference point.
This is my blog series bucko, so we're emitting Wasm because I think it's neat.

## Wasm

We know Wasm is bytecode. 
It's run by some virtual machine which takes care of executing our Wasm on the actual machine.
That's a good start, but we're going to need to know more about Wasm if we're going to emit it from our `IR`.
As a bytecode, Wasm's main format is binary.
Each operation is encoded as a byte (or sometimes a few bytes) that are easy for the virtual machine to decode, but nigh impossible for us to read.
Thankfully, Wasm offers a human-readable [text format](https://webassembly.github.io/spec/core/text/conventions.html#grammar) alongside the binary one.
We'll use the text format in our examples, but our actual code produces the binary format.

The text format uses S-expressions, resembling a lisp.
The use of S-expressions allows for some convenient syntax sugar.
Our previous example can be re-written using nested S-expressions to resemble a tree:

```lisp
(local.set $3 
  (i32.add 
    (local.get $1)
    (local.get $2)))
```

This is purely a syntactic convenience.
Under the hood our Wasm is always a flat list of operations.

Wasm code is found in `.wasm` files (or `.wat` for the text format).
Each `.wasm` file contains a single [Wasm module](https://webassembly.github.io/gc/core/syntax/modules.html#modules).
Mirroring our notion of module from other languages, this is a collection of functions that imports and exports things from other Wasm modules.
Functions are Wasm's main unit of computation.
Each function, unsurprisingly, takes some number of arguments and, more surprisingly, returns some number of values.
We won't be using it, but Wasm allows functions to return two or more values.

Within a module, we'll find a section for each thing our module can contain.
A section is an ordered list of entities, in the binary format it gets marked by an ID to signify when a new section begins.
This ordered list serves not only as a container for things, but also as their identification.
For example the function section is an ordered list of function signatures.
When we reference a function, we do so by its index in the function section.

This divvying of entities up into sections is due to Wasm's original motivation as a portable bytecode for the browser.
It's intended to be sent from a server to a client, leading to many of its encoding choices optimizing for compactness and streaming over a network.
Module's support quite a few sections, many of which are optional, but we'll only be covering the ones we'll be using:

* [Type Section](https://webassembly.github.io/gc/core/binary/modules.html#type-section)
* [Function Section](https://webassembly.github.io/gc/core/binary/modules.html#function-section)
* [Export Section](https://webassembly.github.io/gc/core/binary/modules.html#export-section)
* [Code Section](https://webassembly.github.io/gc/core/binary/modules.html#code-section)

### Type Section

Our first section declares all the types our Wasm module needs.
Wasm code is typed, and those types are validated by the VM before executing code.
Not _every_ type requires an index.
Wasm defines a set of value types, these are imminently available.
We'll be using two of these value types:

* I32
* Ref

But there are [many more](https://webassembly.github.io/gc/core/syntax/types.html#value-types). 
I32 is an integer, specifically a 32-bit integer.
Ref is a reference to a compound type.
A compound type is a collection of value types assembled into a larger type, in our case this larger type will be a function or a struct.
Compound types do require an index, so they must be declared in our type section.
`Ref` uses that index to specify what type it references.

If we want to reference a function type, a thing we'll often do as a functional language, we first need to declare our function type:

```lisp
(type (func (param i32 i32) (result i32)))
```

A function type defines its parameters and return types using `param` and `result`.
Our function type here takes two integers and returns an integer.
Function parameters and return types must be value types.
Wasm only knows how to pass around value types.

If we wanted to pass our function to another function, we must wrap it as a reference:

```lisp
(type (func (param (ref 0)) (result i32)))
```

Our previous function occupies index zero in our section, so that's what we list in our reference type `(ref 0)`.
We use that to define our second type, at index one, as a function that takes our previous function and returns an integer.

The other compound type we'll make use of during emission is structs.
Structs comprise a list of fields, which are essentially value types.
Fields support some extra types, `i8` and `i16`, but we won't need them.
If you want to store a function within a struct, you'll still need a reference type:

```lisp
(type (struct (field (ref 0)) (field i32)))
```

Above we define a struct with two fields:

* A reference to our first function type
* An integer

The final feature of types we'll need is subtyping.
We can declare one type is a subtype of another to allow casting between them.
This will be essential to how we implement closures.
Our previous struct could be made available for subtyping by marking it `sub`:

```lisp
(type (sub 
  (struct 
    (field (ref 0)) 
    (field i32))))
```

This doesn't actually make any subtypes, but tells Wasm subtypes of this struct are allowed.
We have to inform Wasm we might want subtyping as by default it is not enabled for types.
With it enabled, we can define a subtype using that structs index:

```lisp
(type (sub final 2 
  (struct 
    (field (ref 0)) 
    (field i32) 
    (field i32))))
```

This struct is a subtype of our previous struct because it shares all the same field as that struct, but adds a new integer field.
Not only is it a subtype, we've marked it `final`, which means our new struct type cannot be subtyped further.
We can now cast between our type indices two and three.

Not all types are valid subtypes, however.
We couldn't declare a function as a subtype of a struct.
We also can't define any old struct as a subtype of another struct.
For a struct to be a subtype of another, it must share the super structs fields in the right order and type.
If we had defined our struct: 

```lisp
(type (sub final 2 
  (struct 
    (field i32) 
    (field (ref 0)) 
    (field i32))))
```

It would not be a valid subtype, because we swapped the order of the fields, even though they are all the right type.
Putting it all together our final type section would look like:

```lisp
(type (func (param (ref 0)) (result i32)))
(type (func (param (ref 0)) (result i32)))
(type (sub 
  (struct 
    (field (ref 0)) 
    (field i32))))
(type (sub final 2 
  (struct 
    (field (ref 0)) 
    (field i32) 
    (field i32))))
```

### Function Section

Much like how the type section declared types, the function section declares functions.
One might reasonably expect that entails the bodies of functions.
And were we working in the text format one would be correct.
The binary format, however, splits function definitions into two sections: function and code.

Splitting functions like this allows the virtual machine to stream and parallelize compilation of functions.
An implementation detail for us, but it does mean we won't find any Wasm operations in the function section.
Instead, the function section contains solely the signatures of functions defined by this module, similar to a header file in C.

There isn't even much to the function signatures themselves:

```lisp
(func (type 0))
```

A function signature is its type, but all our types are defined in the type section.
All we need in the function section is the index that points at our function type.
Note this isn't a reference type, this is one of the few spots we use a function type as is and not as a reference.
The trivial nature of the function section is part of why it does not appear in the text format.

### Export Section

The export section declares the public API of a module.
Our module, as a bundle of functions, doesn't do a lot on its own.
When we load it up in our VM, it won't actually execute anything immediately.
There are ways to define a module that runs some code when it's loaded, allowing for a module to act as a kind of script.
We won't be using those features and won't be discussing them here.

A module is inert for us.
We need something to call one of our functions before any action starts.
The only way something outside the module can call one of the module's functions is if it's exported.
These exports don't rely purely on indices, and actually give our entities a textual name.
For example if we wanted to export our first function:

```lisp
(export "main" (func 0))
```

We declare an export with the name "main" and point it at function index zero.
We need to specify it's a function because Wasm can export other things.
Not a feature we'll need, but if one were so inclined, they could export [memory](https://webassembly.github.io/gc/core/syntax/modules.html#syntax-mem), [globals](https://webassembly.github.io/gc/core/syntax/modules.html#globals), or [tables](https://webassembly.github.io/gc/core/syntax/modules.html#tables) alongside functions.
All things that are too right for our humble language, for now.
The name given to an export has no bearing on how it is used within the module.
It is only meaningful outside the module when our function is imported.

### Code Section and Stack Machines

The code section is where all the magic happens.
Function bodies reside within the code section and with them the instructions that move the machine.
Code lacks an index of its own.
It exists solely to hold function bodies, and accordingly it reuses function indices.

Before we can talk about what goes into the code section, we need to understand some more about Wasm's execution model.
Wasm uses a [stack machine](https://en.wikipedia.org/wiki/Stack_machine) to execute operations.
Named such because it uses a stack to do all of its computation.
If we wanted to compute `2 + 3`, we would use the instructions:

```lisp
(i32.const 2)
(i32.const 3)
(i32.add)
```

`i32.const` places a known integer on the stack, so after executing our first two instructions the stack contains:

```goat {height="12rem" width="5.5rem"}
 +---+
 | 3 |
 +---+
 | 2 |
 +---+ 
-------
 stack
```

Remember that because it's a stack, when we execute `i32.const 3` that value is placed on top of `2`, becoming the top value of the stack.
`i32.add` consumes two values from the stack, replacing them with their sum:

```goat {height="9rem" width="5.5rem"}
 +---+
 | 5 |
 +---+
-------
 stack
```

All our operations take their arguments from the stack and return results to the stack.
It is not always convenient, however, to work purely with the top of the stack.
Quite often we might calculate a value, need to do some further calculations, and then return to working with that value.
We can of course meticulously track what we left where on the stack to try and retrieve our value.
This quickly grows cumbersome and error-prone for involved expressions.

Wasm instead allows for a number of local variables that store values from the stack.
Wasm functions are allowed to use any number of locals, but they must declare ahead of time how many locals they'll need.
As the name might imply, these locals are _local_ to the current function and are erased when the function returns.
Locals are helpful not just to remember values, but also if we need a value multiple times:

```lisp
(i32.const 2)
(i32.const 3)
(i32.add)
(local.set 0)
(local.get 0)
(local.get 0)
(i32.mul)
```

We again calculate five on our stack, but this time we save it into our local zero.
Like most things in Wasm, locals are referred to by their index.
Our local allows us to fetch our five twice to calculate it's square, twenty-five, using `i32.mul`.
Wasm lacks a way to duplicate values on the stack directly.
They must be saved in a local if you need it multiple times.

Now that we're more familiar with Wasm's execution model, we can return to our code section.
For each function index in our function section we provide two things in the code section:

* List of local variables
* List of instructions

Wasm functions declare all the locals they'll need ahead of time.
This is a minor burden on us as the code emitters, but means our virtual machine generates better code as it knows ahead of time how many locals a function requires.
Locals must have a value type.
`local.set` stashes the top value of the stack in a local, and the top value of the stack must always be a value type.
Ergo, the only things that go in locals are value types.

### Wasm Instructions

From there the remainder of our section is a list of instructions.
We won't cover all the instructions available, there are [a lot](https://webassembly.github.io/spec/core/syntax/instructions.html).
We'll touch on the highlights here and cover the instructions we use in detail as we encounter them.

Instructions are organized into categories:

* [Numeric Instructions](https://webassembly.github.io/gc/core/syntax/instructions.html#numeric-instructions)
* [Reference Instructions](https://webassembly.github.io/gc/core/syntax/instructions.html#reference-instructions)
* [Aggregate Instructions](https://webassembly.github.io/gc/core/syntax/instructions.html#aggregate-instructions)
* [Variable Instructions](https://webassembly.github.io/gc/core/syntax/instructions.html#variable-instructions)
* [Control Instructions](https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions)

Again, this is not all of them, but the ones we'll be using.
Numeric instructions deal in all things numbers.
This is arithmetic, boolean operators, and bit operators galore.
We, however, won't be using any of these.
Our language lacks a way to perform even humble addition.
Look we're making a functional language here, not a functional language.
The only numeric instruction we need is `i32.const`, which loads a constant onto the stack.

Reference instructions deal in references.
They create, equate, and cast references.
They also provide support for null references, but we won't need this feature.
We have two highlights:

* `ref.func` creates a function reference from a function index.
* `ref.cast` casts a reference of one type into a reference of a subtype.

Casting to a supertype is implicit, so we only need an instruction when we downcast to a subtype.

Notably missing from our reference instructions are struct references.
That is because structs are handled by aggregate instructions.
Aggregate instructions are part of Wasm's [GC feature](https://github.com/WebAssembly/gc?tab=readme-ov-file).
The extends the default capabilities of Wasm with support for managed heap types that are garbage collected.
Rather than living on the stack, like normal Wasm values, these types live on the heap and are freed for us by the garbage collector once they are no longer referenced.
Wasm's GC feature is not yet part of the standard. 
It has to be enabled by flipping a flag.
But all major runtimes support it, so it's extremely unlikely it won't make it into the standard at this point.

Aggregate instructions deal in GC'd objects.
These come in two flavors: structs and arrays.
We only need structs, so they'll be our focus.
Structs are created by `struct.new` which takes a type index and values from the stack and constructs a reference to a new struct.
Returning to our struct from the type section `(struct (field (ref 0)) (field i32))`, imagine we already had a function zero that takes two integers and returns an integer.
We could construct an instance of our struct using:

```lisp
(ref.func 0)
(i32.const 435)
(struct.new 2)
```

`struct.new` takes the type of struct it's constructing statically, but it's field come from the stack.
The fields must be in order on the stack, it would be a type error if we did:

```lisp
(i32.const 435)
(ref.func 0)
(struct.new 2)
```

Once we have a struct, `struct.get` takes a type index and a field index, consumes a struct reference from the stack, and produces the value at the field of that struct.
Despite structs being categorized as aggregate, they still produce normal references. 
We don't need a special `struct.cast` instruction, `ref.cast` works as expected on structs.
On to variable instructions.

Variable instructions deal with locals.
These are `local.get` and `local.set`, which we've seen before.
A third fun one we'll use is `local.tee`.
`local.tee` combines a get and a set.
It takes a value from the stack and stores it in a local, but leaves a copy on the stack.
This is helpful when we know we'll need the value immediately.
Imagine we're emitting code for:

```rs
let x = ...;
let y = x + 1;
x + y
```

We store `x` in some local `0`, but we'll immediately need x's value to compute `y`.
Rather than `local.set 0` and immediately `local.get 0`, we can `local.tee 0` to store x and leave it's value on the stack for computing `y`.
Later when we compute `x + y`, we can still `local.get 0` to retrieve x's value again.

Control instructions deal in control flow.
For us, as a functional language, this largely means function calls.
But Wasm supports all kinds of control flow: if/else, loops, switches.
Our main takeaway from this category will be `call_ref`.
Like `struct.new`, it takes a static function reference and function arguments from the stack then calls the referenced function. 
It allows us to call the closures we'll emit.

## Emission

That was simultaneously a lot of information and only an amuse-bouche of what Wasm offers.
We turn now towards the process of producing Wasm from our `IR`.
We could, if we had a predilection for pain, cobble together string literals into the shape of Wasm's text format.
While this suffices, its more brittle than over-baked pottery and not well suited for the goal of our compilation.

Emitting the text format also begets an extra step in our pipeline to execution.
Wasm's binary format is what will be executed by the machine.
Regardless of what we emit, it must be translated to binary by the time we execute.
It'd be simpler if we emit the binary format directly.
We can provide ancillary workflows to produce the text format, if need be, but our primary ambition is best served by the binary format.

Placing each byte in our encoding by hand, however, doesn't sound much simpler than concatenating strings into the text format.
Fortunately, rust has a crate, [wasm-encoder](https://crates.io/crates/wasm-encoder), that provides Rust wrapper types to orchestrate a Wasm module in binary format.
One of the advantages of choosing a specified bytecode, such as Wasm, is we can lean on the ecosystem of tooling around the standard.
wasm-encoder takes care of literally spitting out the bytes Wasm expects, but it does not take care of validating those bytes meet Wasm's invariants.

We could hardly call ourselves compilersmiths if offload that much work to a dependency.
We'll still need to ensure things are defined at the right type, defined before usage, etc.
A lot of this we've handled implicitly by virtue of doing compilation up to this point.
We're confident our types will line up because we type checked.
We're confident our names are defined before usage because name resolution succeeded.
If that were not the case, we'd have emitted a compiler error before we reached codegen.

So we aren't spitting out the raw bytes, and we've already ensured correctness, what exactly are we doing during emission?
After closure conversion, we have a collection of items.
One item for our main expression, and one item per closure we converted in our main expression.
We assemble these items into a Wasm module, where each item becomes a Wasm function.

As we turn items into Wasm functions, we'll need to remember what types we use, so we can fill out the type section of our module.
Within each function, we also need to track locals required for our item's body.
Finally, we have to turn `IR` into the list of instructions that produces the result of our expression.
To accomplish these three tasks we'll use three helper structs:

* EmitTypes
* EmitLocals
* EmitWasm

`EmitWasm` is the overarching struct responsible for converting items into functions.
It will call out to `EmitTypes` and `EmitLocals` to produce Wasm types and locals as it converts.
We'll start by covering `EmitTypes` and build towards `EmitWasm`

## Emitting Wasm Types

`EmitTypes` converts our `IR` type into Wasm's type, similar to the process of lowering our `Ast` type into our `IR` type.
Achieving this goal, understandably, requires some metadata:

```rs
struct EmitType {
  types: Vec<PartialTy>,
  supertypes: HashMap<u32, u32>,
}
```

Equivalent Wasm types must be given the same index, so we intern our types in `types` when we convert them.
We can shirk this duty of course, wasm-encoder prints the bytes regardless, but then types that look equal will fail to typecheck causing quite the confounding error messages.
We also track the subtype/supertype of each type, but we'll only list structs as subtypes of each other.

Our most complicated typing comes from closures.
As we saw in closure conversion, closures already exist at two types: `Closure` and `ClosureEnv`.
Each of these are going to need a Wasm type (and we need them to be subtypes of each other).
Closures will inhabit a third time in Wasm: `struct`.

`struct` is the supertype of all other structs.
Like a value type, it requires no index because it has no fields to declare.
Any declared struct type implicitly upcasts to `struct`, and `struct` can downcast into any declared struct.
This is dangerously close to dynamic typing, and our downcast will crash at runtime if we cast it to an invalid struct type.

Consider a closure that takes an integer argument, returns an integer, and captures an integer in its environment.
We'll refer to that closure by three different types throughout code generation:

* `struct` - our abstract supertype
* `(struct (field (ref $fun)))` - a struct with a single field for the function of our closure
* `(struct (field (ref $fun)) (field i32))` - the full struct type containing both the function and the environment.

We'll refer to the latter two as the abstract type and environment type of the closure.

`EmitTypes` is not the final form of our types.
Accordingly, `PartialTy` represents the subset of a type we care about during emission:

```rs
enum PartialTy {
  Func(FuncType),
  Struct(Vec<FieldType>, bool),
}
```

For a function type, this is `wasm_encoder`'s `FuncType` which contains the arguments and return types of our function.
For a struct type, we store a list of `FieldType`s.
Any `ValueType` is a `FieldType`.
`FieldType`s also support extra types we don't need.

The `bool` included in our struct tracks its finality for subtyping.
If we know with certainty our struct has no subtypes, the `bool` will be true.
If our struct might have subtypes, the `bool` will be false.

### Emitting Value Types

Our main entrypoint into `EmitType` is `emit_val_ty`.
This is the function we'll default to when we need a Wasm type from an `IR` type:

```rs
fn emit_val_ty(&mut self, ty: &Type) -> ValType {
  todo!()
}
```

`emit_val_ty` converts our `Type` into a Wasm value type:

```rs 
match ty {
  Type::Int => ValType::I32,
  Type::ClosureEnv(closure, _) => 
    self.emit_val_ty(closure),
  Type::Closure(arg, ret) => 
    self.emit_closure_index(arg, ret)
.struct_index
        .as_val_ty(),
}
```

`Int` becomes `I32`, easy enough.
We've assumed till now our Int was 32-bits because our integer literals use `i32`, but now it's explicit.
Our `ClosureEnv` returns whatever type its `closure` field returns.
Our `Closure` case calls `emit_closure_index`, returning the `struct_index` as a value type, whatever that means.
We'll have to dig deeper to figure out what's going on here.

`emit_closure_index`, one of our helpers, turns a closure type into not one, but two wasm types.
One type for the function underlying our closure, and one type for the closure's abstract type:

```rs
fn emit_closure_index(
  &mut self, 
  arg: &Type, 
  ret: &Type
) -> ClosureTypeIndex {
  todo!()
}
```

Nothing surprising in `ClosureTypeIndex` it just helps us keep our indices straight:

```rs
struct ClosureTypeIndex {
  func_index: u32,
  struct_index: u32,
}
```
Our first step to constructing our types is to get value types for our closure's argument and return type:

```rs
let arg_valty = self.emit_val_ty(arg);
let ret_valty = self.emit_val_ty(ret);
```

With those in hand we can construct our function type:

```rs
let func_index = self.emit_ref_ty(PartialTy::Func(FuncType::new(
  [abstract_struct_ty(), arg_valty],
  [ret_valty],
)));
```

While our IR type has a single argument, our Wasm type has two arguments: One `abstract_struct_ty`, which is a helper that returns Wasm's `struct`, and our closure argument.
Our first argument is the closure environment, typed as `struct`.
Technically, this allows us to pass an invalid struct to our closure.
Wasm would happily pass `{}` as our closure environment, and crash when we try to use it.
We can be confident this won't happen, however, because our `IR` is type checked.

Alright, so we don't necessarily have to worry about passing the wrong `struct`.
But why use the `struct` type and risk it?
We can't use the precise closure environment type, we only know that type within our closure's body.
When passing the closure to itself as the environment parameter, we only know the closures function type.
Wouldn't that be an improvement at least?
It would prevent treating unit structs as environments.

While that would give us more confidence, it would also require recursive types which are a headache to emit.
Our closure's abstract type contains the function type of our closure body.
If our function type uses the closure's abstract type, then our closure's abstract type contains itself.
We've found ourselves typing in circles.

Wasm actually has support for [recursive types](https://webassembly.github.io/gc/core/syntax/types.html#recursive-types).
We _could_ track these recursive groups for each closure and emit them.
But the extra complexity this would incur buys us little in terms of extra safety or performance.
Within our closure body, we'd still have to cast from our closure's abstract type to the closure's environment type.
We opt to cheat our types a little and save ourselves the trouble.

Back in `emit_closure_index` we have another unseen function: `emit_ref_ty`.
This handles interning compound types, so that we can emit them correctly in the type section:

```rs
fn emit_ref_ty(&mut self, key: PartialTy) -> u32 {
  self
    .types
.iter()
    .position(|x| x == &key)
    .unwrap_or_else(|| {
      let indx = self.types.len();
      self.types.push(key);
      indx
    })
    .try_into()
    .unwrap()
}
```

We keep a list of types, and check if our current type is already in the list.
If it is not, we insert it at the end and use that as our type's index.
Back in `emit_closure_index`, again, we'll use `emit_ref_ty` to index our struct type:

```rs
let struct_index = self.emit_ref_ty(PartialTy::Struct(
  vec![FieldType {
    element_type: StorageType::Val(func_index.as_val_ty()),
    mutable: false,
  }],
  false
));
```

A common operation we'll need is to wrap a type index up as a Wasm value type, so common in fact, we already saw it used earlier in `emit_val_ty`.
We define a helper `as_val_ty` to succinctly wrap our `u32` type index:

```rs
impl AsValTy for u32 {
  fn as_val_ty(&self) -> ValType {
    ValType::Ref(RefType {
      nullable: false,
      heap_type: HeapType::Concrete(*self),
    })
  }
}
```

Turning our type index into a value type is a simple matter of treating it as a reference type.
We construct a `Ref` value type with an instance of `RefType` pointing at our type index. 
Wasm supports nullable references, but our language lacks them, so we mark all our reference types non-nullable.
Back in `emit_closure_index`, for one final time, all that remains is to construct our output:

```rs
ClosureTypeIndex {
  func_index,
  struct_index
}
```

After constructing our struct index we assemble our resulting `ClosureTypeIndex`, and we're done.

### Emitting Item Types

We'll need another method to emit types, `emit_item_ty`:

```rs
fn emit_item_ty(
  &mut self, 
  item: &Item
) -> u32 {
  todo!()
}
```

`emit_item_ty` creates function types for the Wasm functions we generate.
Because they are functions, these are not value types, but unwrapped type indices that point at function types.
We already have `emit_ref_ty`, however, that handles function types just fine.
`emit_item_ty` has some special case logic to help us deal with closures that we only want to use for top level items.
We start by generating our return type:

```rs
let ret_ty = self.emit_val_ty(&item.ret_ty);
```

After that we construct a Wasm function type from our item's parameters and return type:

```rs
let func_ty = FuncType::new(
  item.params.iter().map(|var|
    match &var.ty {
      Type::ClosureEnv(_, _) => 
        abstract_struct_ty(),
      ty => self.emit_val_ty(ty),
    }),
  [ret_ty],
);
```

If we encounter a closure environment in our parameter list, we treat it as `struct`.
As we discussed earlier, we want to type closure environment's as `struct` when we pass them to the closure's own implementing function.
We already do that when emitting types for our closure, but we also need to do it here for our closure item's function type.
Finally, we turn our item's type into an index:

```rs
self.emit_ref_ty(PartialTy::Func(func_ty))
```

That polishes off `EmitTypes` for now, there are still some helpers left, but we'll cover them once we see them used in context.

## Emitting Instructions

We'll use our newfound knowledge of type emission to convert types we come across as we acheive our main goal: emitting wasm instructions.
It's all been building to this.
Types are cool, but instructions are where we actually execute code.

We have our work cut out for us on that front.
Before actually emitting instructions, we're going to need some more setup.
First on our list is `EmitWasm`, it will track the state we need between emitting individual functions:

```rs
struct EmitWasm { 
  types: EmitType,
  functions: HashMap<ItemId, u32>,
}
```

`types` we just discussed.
It's how we'll emit types as we go.
`functions` is prepopulated with a function index per `ItemId` we'll encounter.
We use it to map our items to their corresponding Wasm functions.

The sole entrypoint into `EmitWasm` is `emit_item`:

```rs
fn emit_item(
  &mut self, 
  item: Item
) -> Function {
  todo!()
}
```

Given an item, it produces a Wasm function.
A Wasm function is a list of instructions, as we can see if we crack open `Function`'s definition:

```rs
pub struct Function {
  bytes: Vec<u8>,
}
```

..._oh_. Well believe me, those bytes better be a list of instructions.
A cursory glance at `Function`'s API reveals there are two ways to construct it:

```rs
impl Function {
  pub fn new<L>(locals: L) -> Self
  where
    L: IntoIterator<Item = (u32, ValType)>,
  { ... }

  pub fn new_with_locals_types<L>(locals: L) -> Self
  where
    L: IntoIterator<Item = ValType>,
  { ... }
}
```
Regardless of which we pick, we're going to need an iterator of `ValType`s.
These are the types of the local variables predeclared by our function.
Locals, like everything in Wasm, are referenced by their index, so we don't have to name our locals just provide their types in order.

Unfortunately, we have not precomputed the locals used by our `IR`.
This is only a minor setback, however.
Rather than immediately create our function, we delay creation until after emitting our function's instructions:

```rs
let (inss, local_tys) = self.emit_body(&item.params, item.body);

let mut function = Function::new_with_locals_types(local_tys);
for ins in inss {
  function.instruction(&ins);
}
function.instruction(&Instruction::Return);
function.instruction(&Instruction::End);
function
```

Our deference allows us to track how many locals we use as we `emit_body`.
We can then construct our `Function` and stuff it with our list of instructions.
All our functions end the same way, `return` and `end`, so we include those regardless of what our list says.


`return` returns the top value of our stack, and clobbers anything else left on the stack.
`end` ends the current scope.
It's normally used for Wasm's structured control flow: `if`, `loop`, etc.
Functions technically introduce a new scope as well, so it must be ended.
I believe this is to help with streaming, but don't quote me on that.
Let's look at how we turn our body into instructions:

```rs
fn emit_body(
  &mut self,
  params: &[Var],
  body: IR
) -> (Vec<Instruction<'static>>, Vec<ValType>) {
  let mut locals = EmitLocals {
    next_local: 0,
    locals: HashMap::default(),
    local_tys: vec![],
  };

  todo!()
}
```

Technically `emit_body` is cheating.
We don't just take the body of our IR.
We also take its parameters.
It's for a good reason though.

Function parameters in Wasm are accessed just as locals.
If we have a function of two parameters, locals zero and one will refer to those parameters, and the actual local variables start at two.
Parameters, however, don't have to be declared as locals in the function body.
Their type can already be found from the function signature.

Locals are going to be important during emission.
So important, we've introduced a whole type to manage them, `EmitLocals`:

```rs
struct EmitLocals {
  next_local: u32,
  local_tys: Vec<ValType>,
  locals: HashMap<VarId, u32>,
}
```

The most prominent task fulfilled by `EmitLocals` is tracking how many locals we use.
Second to that is tracking the type of those locals
It serves a final function, maintaining a mapping from variables to locals.
It accomplishes these goals through three methods:

* `param_for`
* `local_for`
* `anon_local`

They all do variations on the same thing, so we'll run through them quickly:

```rs
fn param_for(
  &mut self, 
  id: VarId
) -> u32 {
  let local = self.next_local;
  self.next_local += 1;
  self.locals.insert(id, local);
  local
}
```

`param_for` returns the local for a parameter.
Because we know it's a parameter, we don't track it in `local_tys`.
We only map its variable to the local we generate.

```rs
fn anon_local(
  &mut self, 
  ty: ValType
) -> u32 {
  let local = self.next_local;
  self.next_local += 1;
  self.local_tys.push(ty);
  local
}
```

`anon_local` creates a local with no corresponding variable.
Our variables and locals are not in a 1:1 mapping, so we use this when we need a local that doesn't appear in the `IR`.
It's the opposite of `param_for`, we don't map a variable to our local and only track our locals type.

```rs
fn local_for(
  &mut self, 
  id: VarId, 
  ty: ValType
) -> u32 {
  let local = self.next_local;
  self.next_local += 1;
  self.local_tys.push(ty);
  self.locals.insert(id, local);
  local
}
```

`local_for`, our most common case, both tracks our local's type and maps a variable to our generated local.
We will use this for all the variables we encounter that are not a parameter (i.e. most of them).
Back in `emit_body`, we have a more immediate need for `param_for`:

```rs
for param in params {
  locals.param_for(param.id);
}
```

After pre-seeding our locals with our parameters we handle the case where our body is a closure body:

```rs
let mut inss: Vec<Instruction> = vec![];

if let Type::ClosureEnv(closure, env) = &params[0].ty {
  let closure_env_index = self.types.emit_closure_env_index(closure, env);
  let casted_env_local = locals.anon_local(closure_env_index.as_val_ty());
  inss.extend([
    Instruction::LocalGet(locals[&params[0].id]),
    Instruction::RefCastNonNull(HeapType::Concrete(closure_env_index)),
    Instruction::LocalSet(casted_env_local),
  ]);

  locals.locals.insert(
    params[0].id,
    casted_env_local,
  );
}
```

We know we're dealing with a closure body if our first parameter has a `ClosureEnv` type.
When that happens, we need to cast our closure's abstract type to our closure's environment type.
We can see this cast is performed by our first taste of emitting Wasm instructions:

```rs
Instruction::LocalGet(locals[&params[0].id]),
Instruction::RefCastNonNull(HeapType::Concrete(closure_env_index)),
Instruction::LocalSet(casted_env_local),
```

Our first instruction gets the value of the local associated with our first parameter.
We trust this will always be a struct reference.
The struct reference is cast to our closure's environment type, represented by the type index `closure_env_index`
Finally we store our cast struct reference in a new anonymous local `casted_env_local`.

We need a new local for our `env` because its type has changed.
For the remainder of the body, however, we only want to reference our cast environment.
We have no need of the abstract closure our parameter provides.
We surreptitiously swap the local associated with our first parameter to return our `casted_env_local`.
Now whenever our body references its env parameter, it will use `casted_env_local` rather than `params[0]`.

The Wasm type of our closure environment is determined by `emit_closure_env_index`, a helper on `EmitTypes`:

```rs
impl EmiTypes {
  fn emit_closure_env_index(
    &mut self, 
    closure: &Type, 
    env: &[Type]
  ) -> u32 {
    todo!()
  }
}
```

Given the components of a closure environment, `closure` and `env`, we'll produce a Wasm struct type containing a field for each type and intern it to produce a type index.
Our first step is turning our `closure` into its function type index:

```rs
let Type::Closure(arg, ret) = closure 
else {
  panic!("ICE: Non-closure type appeared in ClosureEnv type");
};

let closure_indices = 
  self.emit_closure_index(arg, ret);
```

We reuse `emit_closure_index` to get our function's index.
That index is used to construct the first field of our closure environment struct:

```rs
let code_field = FieldType {
  element_type: StorageType::Val(
    closure_indices.func_index.as_val_ty()),
  mutable: false,
};
```

From there we construct the rest of the fields of our environment's type:

```rs
let fields = std::iter::once(code_field)
  .chain(env.iter().map(|ty| FieldType {
    element_type: 
      StorageType::Val(self.emit_val_ty(ty)),
    mutable: false,
  }))
  .collect();
```

Before constructing our struct type, we first emit a struct for our closure's abstract type:

```rs
let super_indx = self.emit_ref_ty(PartialTy::Struct(vec![code_field], false));
let struct_idx = self.emit_ref_ty(PartialTy::Struct(fields, true));
self.supertypes.insert(struct_idx, super_indx);
struct_idx
```

Our closure's abstract type index acts as the supertype of our full closure environment type.
It's critical we track this supertype relationship whenever we produce a closure environment type.
We have one final stop in `emit_body`:

```rs 
self.emit_ir(body, &mut locals, &mut inss);

(inss, locals.local_tys)
```

With the setup out of the way, we defer to `emit_ir` to flesh out our list of instructions.
Once it's done, doing god knows what, we return our list of instructions and our list of local types.

### Emitting Instructions, Actually For Real This Time

We had to peel away a lot of layers, but `emit_ir` finally takes on a familiar form:

```rs
fn emit_ir(
  &mut self, 
  body: IR, 
  locals: &mut EmitLocals, 
  inss: &mut Vec<Instruction>
) {
  match body {
    // it's about to get wild in here
  }
}
```

Seeing a function immediately match on a tree type feels like coming home to a warm fire at this point.
If our match makes any recursive calls, I'll cry tears of joy.

`emit_ir` has a humble goal: turn an `IR` into a list of instructions.
We're going to take it case by case, but before that there's an important invariant we rely upon.
TODO: Make sure we've explained Wasm is a stack machine by now.
For any given expression in our `IR`, it must produce one value on the stack.
The expression is free to do whatever in the list of instructions it emits, but by the end of those instructions it must have one value remaining.

Our first case, `Var`, makes this easy to uphold:

```rs
IR::Var(var) =>
  inss.push(Instruction::LocalGet(locals[&var.id])),
```

Whatever bound this variable created a local for it.
All that's left for us to do is get the value out of the assigned local.
A similarly self-evident solution presents itself for `Int`:

```rs
IR::Int(i) => 
  inss.push(Instruction::I32Const(i)),
```

When we encounter an integer literal, we immediately place it on the stack with `i32.const`.
For `Closure`s we'll actually need multiple instructions.
First we have to get our indices in order:

```rs
IR::Closure(ty, item_id, vars) => {
  let func_index = self.functions[&item_id];
  let struct_index = self
    .types
    .emit_closure_env_index(&ty, &vars.iter().map(|v| v.ty.clone()).collect::<Vec<_>>());
  let ValType::Ref(RefType { heap_type, .. }) = self.types.emit_val_ty(&ty) else {
    panic!("ICE: Closure assigned to variable with non closure type");
  };
  todo!()
}
```

We start by looking up the function index associated with our closure's `item_id`.
Next we emit the type index for our closure's environment type.
Even though closures are passed around as a single field struct, we need the full environment type here.
It is required to construct our closure's struct, which contains both the function and the captured environment.
We'll still need our closure's external type, so we also emit our closure's type and unwrap it to get at `heap_type`.
Our preparation lets us blitz out our instructions with little fanfare:

```rs
inss.push(Instruction::RefFunc(func_index));
inss.extend(
  vars
    .into_iter()
    .map(|var| Instruction::LocalGet(locals[&var.id])),
);
inss.push(Instruction::StructNew(struct_index));
inss.push(Instruction::RefCastNonNull(heap_type));
```
TODO: Explain what the stack looks like at struct new
`RefFunc` creates a reference to a function from that functions index.
For us, that will be the function that implements our closure.
Our captured variables are variables, so we get the value of their local and load it onto the stack.
All of this is fed into `StructNew` which uses our closure's environment type.
Finally, we cast our newly created struct to `heap_type`, a struct type with a single field for our closure function.

`Apply` has us unpacking closures and feeding them to themselves as environments.
First again we have to gather indices:

```rs
IR::Apply(fun, arg) => {
  let local_ty = fun.type_of();
  let Type::Closure(arg_ty, ret_ty) = local_ty else {
    panic!("ICE: Expected closure type for function of apply");
  };
  let closure_indices =
    self.types.emit_closure_index(&arg_ty, &ret_ty);

  todo!()
}
```

We assume our `fun` has a closure type and produce its indices.
Those are all the indices we need so we can start emitting instructions:

```rs
self.emit_ir(*fun, locals, inss);
let fun_locals = locals.anon_local(
  closure_indices.struct_index.as_val_ty());
inss.push(Instruction::LocalTee(fun_local));
self.emit_ir(*arg, locals, inss);
```

Look at those recursive calls.
It brings a tear to one's eye.
After emitting the instructions for `fun`, we expect the stack to contain one value, a reference to our closure.
We rely on our invariant to ensure that's the case.
We need to use our closure reference multiple times, so we create a local to hold it.
`LocalTee` is a shorthand for a `LocalSet` followed by a `LocalGet`.
It allows us to store our reference in a local while keeping it on top of the stack.

Emitting the instructions for our argument requires nothing more from us than the recursive call.
Our stack is now our argument's value followed by our closure's reference from top to bottom.
That order is important for our next section:

```rs
inss.extend([
  Instruction::LocalGet(fun_local),
  Instruction::StructGet {
    struct_type_index: closure_indices.struct_index,
    field_index: 0, // We know the code field is always 0
  },
  Instruction::CallRef(closure_indices.func_index),
]);
```

We make use of our local to retrieve our closure reference.
Rather than leave it on top of the stack, we get the first field from that reference which will be a function reference.
Our stack heading into `CallRef` is:

```goat
+--------------+
|  func ref    |                                                            
+--------------+                                                                 
|  arg value   |
+--------------+                                                                 
|  struct ref  |
+--------------+
````

`CallRef` expects the top of our stack to be the function being called.
After that should follow N values where N is the number of arguments the function takes.
Importantly, these arguments are bottom to top, so the first argument of our function will be N entries from the top of the stack.
Our closure functions always expect two arguments, the environment and one real argument.
These are our `struct ref` and `arg value` on the stack.
`CallRef` will consume these three stack entries and leave behind the result of calling our function.

IR Local, not to be confused with our shiny new Wasm locals, are up next.
Local doesn't really require any indices, so we jump straight into emission:

```rs
self.emit_ir(*defn, locals, inss);
let val_ty = self.types.emit_val_ty(&var.ty);
let local = locals.local_for(var.id, val_ty);
inss.push(Instruction::LocalSet(local));
self.emit_ir(*body, locals, inss);
```

We start by emitting our definition.
After creating a Wasm local for our variable, we store our definition's value within the local.
Notice this again relies on our invariant that each expression must produce exactly one value.
If our definition produced 2 (or even worse 0) values, `LocalSet` would not behave correctly here.
We finish up by emitting instructions for body.

Unlike our previous run-ins with locals, we have essentially no scope management here.
Our `local` is only set once we execute `LocalSet`, but nothing is stopping us from referencing it anywhere in our line of instructions. 
We rely on our previous passes to ensure that our local is only referenced after it's set in our sea of Wasm code.

Our last case, `Access`, retrieves a capture value from a closure environment.
If we had structs, it would also retrieve values from structs.
Struct abstinence saves us from the consequences of those complications.
What we're left with repeats a pattern we've seen in our other nodes.
First we generate some indices:

```rs
let ty = strukt.type_of();
let Type::ClosureEnv(closure, env) = ty else {
  panic!("ICE: Expected closure env type for struct access");
};
let struct_type_index =
  self.types.emit_closure_env_index(&closure, &env);
```

We assume our `strukt` has type `ClosureEnv` and use that to emit a type index for our Wasm struct type.
Using that type index, we can access our struct:

```rs
self.emit_ir(*strukt, locals, inss);
inss.push(Instruction::StructGet {
  struct_type_index,
  field_index: field.try_into().unwrap(),
});
```

Emitting instructions for `strukt` leaves a struct reference on the stack, which we immediately consume to get the `field` of referenced struct.
Our conversion `.try_into().unwrap()` takes us from a `usize` to a `u32`, so if our closure captures 4 million or more variables we'll crash.
But honestly, I suspect the crash will come before this conversion.
With that we've assembled everything we need to emit wasm from our IR.

## Finishing our Wasm Module

We glue everything together in `emit_wasm`, our main function for this pass:

```rs
fn emit_wasm(
  items: Vec<(ItemId, Item)>,
) -> Vec<u8> {
  todo!()
}
```

We take a list of items as our input.
Our language only produces one "item" at the moment, since we lack top level functions right now.
But recall that each closure receives its own item in closure conversion, so we may very well have multiple items at this point in our compiler.
From these items we produce a `Vec<u8>`, a somewhat surprising choice.
It doesn't appear to have anything to do with Wasm.
Our byte vector is the binary encoding of the Wasm module we will generate.

TODO: Make sure we've explained wasm modules prior to this
A module is just a bunch of sections, so let's get some sections ready:

```rs
let mut types = EmitType::default();
let mut func = FunctionSection::new();
let mut export = ExportSection::new();
```

One of these is not like the others.
`types` isn't a section, but we'll see that it becomes one shortly.
`func` and `export` are used to assign function indices to each of our items:


```rs
let functions: HashMap<ItemId, u32> = items
  .iter()
  .map(|(item_id, item)| {
    let func_index = func.len();
    let type_index = types.emit_item_ty(item);
    func.function(type_index);
    export.export(&format!("func{}", item_id.0), ExportKind::Func, func_index);
    (*item_id, func_index)
  })
  .collect();
```

As we build up our `func` and `export` sections with each item, we also emit each item type and build a map from item id to function index.
This does mean we're exporting every item we emit, making it publicly available.
We'll worry about encapsulation once our language supports more than a single expression.
`functions` is used to build `EmitWasm` which we immediately use to emit code:

```rs
let mut emitter = EmitWasm {
  types,
  functions,
};
let mut code = CodeSection::default();
for (_, item) in items {
  code.function(&emitter.emit_item(item));
}
```

We're well versed in `emit_item` at this point.
It produces a `Function` that we immediately add to our code section.
The order is important when we do this.
We need to ensure that the order of functions in our code section matches the order of function signatures in our function section.
The order of items keeps our function and code sections synced up.

With all our sections built, the final step is to assemble the Wasm module:

```rs
let mut module = Module::default();
module
  .section(&emitter.types.into_type_section())
  .section(&func)
  .section(&export)
  .section(&code);

module.finish()
```

Order is important here, again.
Nobody tells you this, but if you get the section order wrong here your Wasm module will silently break (and fail to validate).
Let's take a peek into `into_type_section`:

```rs
impl EmitTypes {
  fn into_type_section(self) -> TypeSection {
    todo!()
  }
}
```

`EmitTypes` has dutifully tracked all the types we've asked for over the course of emission.
We'll now use that knowledge to construct the perfect `TypeSection` for our module.
The main structure of our method is iterating over `types`:

```rs
let mut sect = TypeSection::new();
for (i, ty) in self.types.into_iter().enumerate() {
  todo!()
}
sect
```

For each of our partial types, we're going to graduate it into a full Wasm type and write it into our section:

```rs
let (inner, is_final) = match ty {
  PartialTy::Func(func_type) => (
    CompositeInnerType::Func(func_type),
    true,
  ),
  PartialTy::Struct(fields, is_final) => (
    CompositeInnerType::Struct(StructType {
      fields: fields.into_boxed_slice(),
    }),
    is_final,
  ),
};
```

`CompositeInnerType` isn't particularly important here.
It's just a helper type `wasm_encoder` provides to help us encode types.
It provides cases for all the Wasm types:

```rs
enum CompositeInnerType {
  /// The type is for a function.
  Func(FuncType),
  /// The type is for an array.
  Array(ArrayType),
  /// The type is for a struct.
  Struct(StructType),
  /// The type is for a continuation.
  Cont(ContType),
}
```

But we only need the function and struct case, since those are the only composite types we have.
We also determine `is_final` for our type.
For functions this is easy, they're never a subtype, so `is_final` is always true.
Structs can be subtypes of each other, but fortunately we've kept track of which structs can be subtyped and which cannot.
All we have to do here is pass along `is_final` from our partial type.

We take our `CompositeInnerType` and finality onwards to emit a `SubType`:

```rs
let indx: u32 = i.try_into().unwrap();
let supertype_idx = self.supertypes.get(&indx).copied();
sect.ty().subtype(&SubType {
  is_final,
  supertype_idx,
  composite_type: CompositeType {
    shared: false,
    inner,
  },
})
```

This looks a little odd.
We know function types are never going to be a subtype of one another, so why do we emit them as a `SubType`?
When our partial type lacks a supertype, `supertype_idx` will be `None`.
In Wasm's encoding, a subtype with no supertype and a type are indistinguishable.
We can actually peek at how subtypes get encoded to convince ourselves of this:

```rs
fn subtype(mut self, ty: &SubType) {
  self.encode_subtype(ty)
}
```

Ah well, that's not helpful.
Let's try one level deeper:

```rs {hl_lines=["5-15"]}
fn encode_subtype(&mut self, ty: &SubType) {
  // We only need to emit a prefix byte before the actual composite type
  // when either the `sub` type is not final or it has a declared super
  // type (see notes on `push_prefix_if_component_core_type`).
  if ty.supertype_idx.is_some() || !ty.is_final {
    if ty.is_final {
      self.bytes.push(0x4f);
    } else {
      if self.push_prefix_if_component_core_type {
          self.bytes.push(0x00);
      }
      self.bytes.push(0x50);
    }
    ty.supertype_idx.encode(self.bytes);
  }
  if ty.composite_type.shared {
    self.bytes.push(0x65);
  }
  match &ty.composite_type.inner {
    CompositeInnerType::Func(ty) => {
      self.encode_function(ty.params().iter().copied(), ty.results().iter().copied())
    }
    CompositeInnerType::Array(ArrayType(ty)) => {
      self.encode_array(&ty.element_type, ty.mutable)
    }
    CompositeInnerType::Struct(ty) => self.encode_struct(ty.fields.iter().cloned()),
    CompositeInnerType::Cont(ty) => self.encode_cont(ty),
  }
}
```

We only do extra stuff when `ty.supertype_idx` is `Some` or `ty.is_final` is false.
Both things we know will never be true for our function types, in which case we fall down to call `self.encode_function` the same as we would when emitting a non subtype.
So for functions, emitting a subtype saves us a little bit of branching and ultimately doesn't impact our encoding.
For structs, our subtype is crucial to our code executing correctly.

Recall that our closures hinge on casting to their specific environment type within the closure's function body.
This only works because we emit each environment type as a subtype of their closure's general type.
If we, naively, emitting all our structs without subtypes, these casts would fail at runtime.
And that's all for creating our type section we turn each of our `PartialTy`s into a full Wasm subtype and return the section containing all of them. 

That's also everything in emitting Wasm period.
We finished our module with the aptly named `module.finish()` which produced our Wasm bytes.

## Watching the magic unfold

Our work up til now culminates in the ability to take an AST and turn it into executable code, truly the essence of compilation.
Let's take a victory lap and try out emitting code for a simple AST:

```rs
let add = Var(...);
let f = Var(...);
let x = Var(...);
Ast::fun(add,
  Ast::app( 
    Ast::fun(f, 
      Ast::app(
        Ast::app(
          Ast::Var(add),
          Ast::app(Ast::Var(f), Ast::Int(400))
        ),
        Ast::app(Ast::Var(f), Ast::Int(1234))
    )),
    Ast::app(Ast::Var(add), Ast::Int(1)))
)
```

I'll omit the `NodeId`s, so the example is easier to read, feel free to insert them in your head.
Our AST takes in a mysterious `add` function, partially applies it to 1 in `f`, and then uses it to add together two calls to `f` with `400` and `1234` as arguments respectively.
You might have noticed, our language lacks primitive numeric operations such as addition. 
But we can fake it by taking a function conveniently named `add`. 
If you look at the emit tests in the [repo](TODO), they actually do this to give us something tangible to check as our execution result.

Running that modest AST through all the passes we've lovingly jammed together gives us some Wasm.
I'll include it in text format, the binary format is much harder to read (feel free to convert it by hand to check it's the same!):

```lisp
(module
  (type (;0;) (func (param (ref struct) i32) (result i32)))
  (type (;1;) (sub (struct (field (ref 0)))))
  (type (;2;) (func (param (ref struct) i32) (result (ref 1))))
  (type (;3;) (sub (struct (field (ref 2)))))
  (type (;4;) (func (param (ref 3)) (result i32)))
  (export "func0" (func 0))
  (func (;0;) (type 4) (param (ref 3)) (result i32)
    (local (ref 3) (ref 1) (ref 3) (ref 1) (ref 1) (ref 1))
    (local.set 2
      (call_ref 2
        (local.tee 1
          (local.get 0))
        (i32.const 1)
        (struct.get 3 0
          (local.get 1))))
    (return
      (call_ref 0
        (local.tee 5
          (call_ref 2
            (local.tee 3
              (local.get 0))
            (call_ref 0
              (local.tee 4
                (local.get 2))
              (i32.const 400)
              (struct.get 1 0
                (local.get 4)))
            (struct.get 3 0
              (local.get 3))))
        (call_ref 0
          (local.tee 6
            (local.get 2))
          (i32.const 1234)
          (struct.get 1 0
            (local.get 6)))
        (struct.get 1 0
          (local.get 5))))))
```

Now this might look like a lot of code for what amounts to calculating `(400 + 1) + (1234 + 1)`.
And it is, but that's because we haven't optimized it yet.
Part of the appeal of Wasm is it comes equipped with a lot of tooling we'd otherwise have to build ourselves.
After running our Wasm through `wasm-opt` we'll get a more optimal version:

```lisp
(module
 (type $0 (func (param (ref struct) i32) (result i32)))
 (type $1 (sub (struct (field (ref $0)))))
 (type $2 (func (param (ref struct) i32) (result (ref $1))))
 (type $3 (sub (struct (field (ref $2)))))
 (type $4 (func (param (ref $3)) (result i32)))
 (export "func0" (func $0))
 (func $0 (type $4) (param $0 (ref $3)) (result i32)
  (local $1 (ref $1))
  (local $2 (ref $2))
  (local $3 (ref $0))
  (local $4 (ref $1))
  (call_ref $0
   (local.tee $4
    (call_ref $2
     (local.get $0)
     (call_ref $0
      (local.tee $1
       (call_ref $2
        (local.get $0)
        (i32.const 1)
        (local.tee $2
         (struct.get $3 0
          (local.get $0)))))
      (i32.const 400)
      (local.tee $3
       (struct.get $1 0
        (local.get $1))))
     (local.get $2)
    ))
   (call_ref $0
    (local.get $1)
    (i32.const 1234)
    (local.get $3))
   (struct.get $1 0
    (local.get $4)))))
```

Okay well...it does use fewer locals, so that's an improvement, certainly.
Hard to say this is a lot better though.
It's a far cry from what we might expect to see:

```lisp
(i32.add
  (i32.add
    (i32.const 400)
    (i32.const 1))
  (i32.add
    (i32.const 1234)
    (i32.const 1)))
```

We're working exclusively in constants, a truly optimized output would be simple:

```lisp
(i32.const 1636)
```

Our code is, ignoring the module stuff, way more instructions than either of these.
This is the result of two factors:

* We pass in our `add` function.
* All functions in our language are implicitly single argument closures.

Because `add` is passed in as a parameter, its source is not available for simplification.
It's a black box.
Compounding this problem, our add isn't a simple function of two integers that returns an integer.
It's a closure that takes one int and returns a new closure.
That closure takes another int and then finally returns an integer.

The vast majority of our Wasm output isn't spent adding integers.
Instead, we spend our instructions constructing and destructing closures.
Which is all to say, we still have a lot of low-hanging fruit in the optimization garden.
Despite that, this code works!

You can run this code and see our function returns `1636`, and in fact if you look at `test_example` in the [repo](TODO) we do just that.
It took decades for people to figure out how to generate optimized code from functional languages.
It'd be crazy to think we could rival that in a couple years of blog posts.
But we don't have to.
Our codegen is correct, even if suboptimal, and that's all we need to get started.
We can iterate on this and improve it in blog posts to come.

With that we've done it.
We've built a compiler.
It feels like just yesterday we were looking upon the steps of compilation, and now we've done them all:

![Infographic for Compiler Pipeline](/img/compiler_pipeline.svg)

Wait a second.
In my exuberance at executing code, I fear I've made a grave mistake.
We've left a gaping parser-shaped hole in the frontend of our compiler.
Can we truly call ourselves compilersmiths if we force our users to feed us hand rolled ASTs?
No.
We'll have to traverse the greatest tarpit known to compilation: The Parser.
