+++
title = "Emit[0].Base: Emitting Wasm For Our Base Language"
date = "2025-05-17T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Emit"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Functions", "IR", "Compiler", "Emission", "Code Generation", "WebAssembly", "Wasm"]
description = "Turning our Base IR into executable WebAssembly"
+++

* Intro
  * Precipice of execution
  
* Why Wasm
  * Cop out
  * Wasm is a great compilation target
    * High level
    * TODO: Think of concrete reasons

* High level what is wasm?
  * Portable bytecode
  * Stack machine
  * Infinite locals
  * Functions
  * Language agnostic

* Know thy enemy (Explain wasm in detail)
  * Not comprehensive, see the spec for everything
    * Just covering what we're using
  * A wasm unit of compilation is a module
  * Module composed of sections
  * Sections (Not comprehensive)
    * Types
    * Functions
    * Exports
    * Code - Kept separate from functions
  * Indices
    * Each section maintains its own index
    * Refer to elements of each section by index
  * Using the wasm gc proposal, so things may look different than the v1 spec.
  * Types
    * Numeric types - i32 | i64 | f32 | f64
    * Function types
      * Type given to functions
      * Does not describe a value
    * Reference types
      * Functions
        * Turns a function type into a value type
      * Structs
        * Talk about subtyping and how it's used by closures.
        * TODO: Maybe cover closure type erasure here?
    * Value types
      * Things we can pass around on the stack and store in locals.
      * References
      * Numbers
  * Functions
    * Index
    * Parameters
    * Returns
      * Wasm supports multiple returns, but we'll only be returning one value
  * Code
    * Associated with a function, acts as function body
    * Where our instructions live
    * Wasm has a lot of instructions, so we'll cover them as we go
    * Highlights:
      * Stack machine
      * Instructions take values off the stack and deposit them there
      * Locals - Infinite registers
        * Instructions don't work on locals directly, they must be fetched
    
* Emission
 * Turning a set of items into a wasm module
 * `wasm-encoder`
   * Manages the bit twiddling of wasm's binary format for us
   * Just encodes, and does not validate. We're on our own to ensure we generate correct wasm
 * EmitTypes
 * Predeclare a function per item.
 * EmitWasm
   * EmitLocals

* EmitTypes
* Interns our compound wasm types
  * Types are represented by their index a `u32`
  * Entrypoints:
    * `emit_val_ty` is the main method we'll use to convert types
      * Produces a value type from our `Type`
      * Recall a value type is a thing we can pass around on the stack, integers, references, etc.
    * `emit_item_ty` only used initially when creating functions for items.
      * Handles env parameter specially
      * Type erases returned closures - TODO: We might want to cover this earlier in an overarching section?
    * `emit_closure_env_index`
      * Creates a wasm type for a closure env
      * Not wrapped up as a ValType, instead we return the underlying index for the struct
  * Supporting cast:
    * `emit_ref_ty`
      * Turns a reference type into a type index by interning it
    * `emit_closure_index`
      * Turns a closure into a struct containing a single field that is a function reference

## Emitting Wasm Types

`EmitTypes` converts our `IR` type into Wasm's type, similar to the process of lowering our `Ast` type into our `IR` type.
Achieving this goal requires us to track some metadata:

```rs
struct EmitType {
  types: Vec<WasmTy>,
  supertypes: HashMap<u32, u32>,
}
```

Equivalent Wasm types must be given the same index, so we intern our types in `types` when we convert them.
We also need to track supertypes of each type.

Closures are passed around as a subtype of their actual type.
Each closure includes its environment in its type, but we pass it around with as a struct with a single field for the function reference.
Treating closures as a single field struct is what allows us to pass different closures where the same function type is expected.
Once we pass our closure to its own body, however, we need to cast it back to its full type, so we can access the environment our closure carries.
Casting our closure back to its full type requires us to track the subtype/supertype relationships of our closures.
TODO: Transition

### Emitting Value Types

Our main entrypoint into `EmitType` is `emit_val_ty`:
TODO: Have we introduced `ValType`?

```rs
fn emit_val_ty(&mut self, ty: &Type) -> ValType {
  todo!()
}
```

`emit_val_ty` converts our `Type` into a Wasm value type.
We won't make use of every Wasm value type available, there are a lot, so for us that will mean either:

* An integer
* A function reference
* A struct reference

Our method's body is succinct:

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

Our `Int` type becomes `I32`, easy enough.
Our `ClosureEnv` returns whatever type its `closure` field returns.
Our `Closure`, calls `emit_closure_index`.
We'll have to dig deeper to figure out what's going on here.

`emit_closure_index`, one of our helpers, turns a closure type into not one, but two wasm types.
One type for the function of our closure, and one type for the single field struct that holds a reference to our closure's function:

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
Our first step to constructing our type is to get value types for our closure's argument and return type:

```rs
let arg_valty = self.emit_val_ty(arg);
let ret_valty = self.emit_val_ty(ret);
```

With those in hand we can construct our function type:

```rs
let func_index = self.emit_ref_ty(WasmTy::Func(FuncType::new(
  [abstract_struct_ty(), arg_valty],
  [ret_valty],
)));
```

While our closure type has a single argument, our function type has two arguments: One `abstract_struct_ty`, whatever that means, and our closure argument.
Our first argument is the closure environment.
An abstract struct type, pronounced `struct` in Wasm, is the super type of all struct types:

```rs
fn abstract_struct_ty() -> ValType {
  ValType::Ref(RefType {
    nullable: false,
heap_type: HeapType::Abstract {
      shared: false,
      ty: AbstractHeapType::Struct,
    },
  })
}
```

`abstract_struct_ty` is a helper to construct `struct` whenever we need it.
We can pass any struct type where a `struct` is expected.
Technically this allows us to pass an invalid struct to our closure.
Wasm would happily let us pass `{}` as our closure env, and crash when we try to use it as our closure environment.
We can be confident this won't happen, however, because our `IR` is type checked.
It ensures we always pass the correct struct as our closure's environment.

Alright, so we don't necessarily have to worry about passing the wrong `struct`.
But why use the `struct` type and risk it?
We can't use the precise closure environment type, we only know that type within our closure's body.
Not when we're passing the closure into itself as the environment parameter.
Couldn't we at least use the closure's struct type?

While that would give us more confidence, it would also require recursive types which are quite a pain.
Our closure's struct type is a struct containing a single field, our closure's function type.
But recall dear reader, that our closure's function type has an extra parameter: our closure's struct type.
We've found ourselves typing in circles.

Wasm actually has support for [recursive types](https://webassembly.github.io/gc/core/syntax/types.html#recursive-types).
We _could_ track these recursive groups for each closure and emit them.
But the extra complexity this would incur buys us little in terms of extra safety or performance.
Our IR already checks that all our types are in a row.
Within our closure body, we'd still have to cast from our closure's struct type to the full closure environment type.
We opt to cheat our types a little and save ourselves the trouble.

Back in `emit_closure_index` we have another unseen function: `emit_ref_ty`.
Our function type is known as a composite type in the Wasm parlance.
Composite types are types that are composed of other types.
Unlike our value types, such as `I32`, we must intern these types and reference them via an index.
Interning and indexing is performed by `emit_ref_ty`:

```rs
fn emit_ref_ty(&mut self, key: WasmTy) -> u32 {
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
Back in `emit_closure_index`, we'll use `emit_ref_ty` to index our struct type, another composite type, as well:
TODO: Figure out where to intro WasmTy

```rs
let struct_index = self.emit_ref_ty(WasmTy::Struct(
  vec![FieldType {
    element_type: StorageType::Val(func_index.as_val_ty()),
    mutable: false,
  }],
  false
));
```

A common operation we'll need is to wrap a type index up as a wasm value type.
We define a helper `as_val_ty` to succinctly wrap our `u32` type index (we already saw it used earlier in `emit_val_ty` already):

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
Back in `emit_closure_index`, all that remains is to construct our output:o

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

We'll use this to create type indices for the function types associated with our top level items.
We already have `emit_ref_ty` that handles function types just fine.
`emit_item_ty` has some special case logic to help us deal with closures that we only want to use for top level items.
We start by generating our return type:

```rs
let ret_ty = self.emit_val_ty(&item.ret_ty);
```

After that we construct a Wasm function type from our item's parameters and return type:

```rs
let func_ty = FuncType::new(
  item.params.iter().map(|var|
      // For definition parameters we need to handle closure environment parameters specially.
      // We erase closures to be of type `(ref struct)` when passing them, so we need to emit a
      // `(ref struct)` as the type of any closure env parameters we see.
      match &var.ty {
        Type::ClosureEnv(_, _) => abstract_struct_ty(),
        ty => self.emit_val_ty(ty),
      }),
  [ret_ty],
);
```

Here we see our special case logic.
If we encounter a closure environment in our parameter list, we treat it as our supertype `struct`.
As we know from closure conversion, the only time a closure environment will appear in our parameter list is for the environment of a closure's implementing function.
In that case, we want to type it as `struct`, so we match the type we emit for our closure's function type.
Finally, we turn our item's type into an index:

```rs
self.emit_ref_ty(WasmTy::Func(func_ty))
```

Our index will be used to type the Wasm functions we declare for each of our items.
That polishes off `EmitTypes` for now, there are still some helpers left, but we'll cover them once we see them used in context.

## Emitting Instructions

We'll use our newfound knowledge of type emission to convert types we come across as we acheive our main goal: emitting wasm instructions.
It's all been building to this.
Types are cool, but instructions are where we actually execute code.
Instructions are the tune our machine dances to.

We have our work cut out for us on that front.
Before actually emitting instructions, we're going to need some more setup.
First on our list is `EmitWasm`, it will track the state we need  between emitting individual functions:

```rs
struct EmitWasm { 
  types: EmitType,
  functions: HashMap<ItemId, u32>,
  code: CodeSection,
}
```

`types` is how we'll emit types as we go.
`functions` is prepopulated with a function index per `ItemId` we'll encounter.
We use it to map our items to their corresponding Wasm functions.
`code` is where we store the series of instructions that form the bodies of our functions.

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

...oh. Well believe me, those bytes better be a list of instructions.
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
These are the types of the local variables employed by our function.
Functions mandate that we declare all our locals upfront.
Locals, like everything in Wasm, are referenced by their index, so we don't have to name our locals just provide their types in order.

Unfortunately, we have not precomputed the locals used by our `IR`.
This is only a minor setback, however.
Rather than immediately create our function, we delay creation until after we've emitting the instructions for our function body:

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
We can then construct our `Function` and dump our list of instructions into it.
All our functions end the same way, `return` and `end`.
`return` returns the top value of our stack (or nothing if our function's return type is void).
`end` ends the current scope.
It's normally used for Wasm's structured control flow: `if`, `loop`, etc.
And in the text format, functions don't have to be `end`ed.
In the binary format, however, functions technically introduce a new scope that must be `end`ed.
I believe this is to help with streaming, but don't quote me on that.

(TODO: Fix this) Onwards to `emit_body`.

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
But as we'll see, we need to assign locals to the parameters while emitting our body, and that impacts the remainder of our locals.

Locals are going to be important during emission.
So important that we've introduced a whole type to manage them `EmitLocals`:

```rs
struct EmitLocals {
  next_local: u32,
  locals: HashMap<VarId, u32>,
  local_tys: Vec<ValType>,
}
```

The most prominent task fulfilled by `EmitLocals` is tracking how many locals of what types we use.
It also serves another function, maintaining our mapping from variables to locals.
It accomplishes thsee goals through three methods:

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
Because we know it's a parameter, we don't want to track it in `local_tys`.
We only want to map our variable to the local we generate.

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

`anon_local` is the opposite of `param_for`.
We don't have a corresponding variable, so we only want to track our locals type.
Variables are locals aren't in 1:1 mapping.
`anon_local` provides us a local that isn't referenced by a variable, hence it's anonymity.

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

`local_for`, our most common case, both tracks our locals type and maps a variable to our generated local.
We will use this for all the variables we encounter that are not a parameter (i.e. most of them).
Back in `emit_body`, though, we have a more immediate need for `param_for`:

```rs
for param in params {
  locals.param_for(param.id);
}
```

After pre-seeding our locals with our parameters we have to handle the case where our body is a closure body:

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
When that happens, we need to cast our abstract closure type to the specific closure environment our body uses.
We can see this cast is performed our first taste of Wasm instructions:

```rs
Instruction::LocalGet(locals[&params[0].id]),
Instruction::RefCastNonNull(HeapType::Concrete(closure_env_index)),
Instruction::LocalSet(casted_env_local),
```

Our first instruction gets the value of the local associated with our first parameter.
We know this will always be a struct reference.
The struct reference is cast to our closure's environment type, represented by the type index `closure_env_index`
Finally we store our casted struct reference in a new local `casted_env_local`.

We need a new local for our `env` because its type has changed.
For the remainder of the body, however, we only want to reference our cast environment.
We have no need of the abstract closure our parameter provides.
Some sleight of hand surreptitiously achieves our aim.
We swap the local associated with our first parameter in our `locals.locals` mapping.
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
let Type::Closure(arg, ret) = closure else {
  panic!("ICE: Non-closure type appeared in ClosureEnv type");
};

let closure_indices = self.emit_closure_index(arg, ret);
```

We reuse `emit_closure_index` to get our function's index.
That index is used to construct the first field of our closure environment struct `code_field`:

```rs
let code_field = FieldType {
  element_type: StorageType::Val(closure_indices.func_index.as_val_ty()),
  mutable: false,
};
```

From there we construct the rest of the fields of our struct type:

```rs
let fields = std::iter::once(code_field)
  .chain(env.iter().map(|ty| FieldType {
    element_type: StorageType::Val(self.emit_val_ty(ty)),
    mutable: false,
  }))
  .collect();
```

Before constructing our struct type, we first emit a struct with just our closure function type:

```rs
let super_indx = self.emit_ref_ty(WasmTy::Struct(vec![code_field], false));
let struct_idx = self.emit_ref_ty(WasmTy::Struct(fields, true));
self.supertypes.insert(struct_idx, super_indx);
struct_idx
```

This acts as the super type of `struct_idx` our full closure environment type.
It's critical we track this supertype relationship whenever we produce a closure environment type, otherwise our environment cast will fail.
We have one final stop in `emit_body`:

```rs 
self.emit_ir(body, &mut locals, &mut inss);

(inss, locals.local_tys)
```

With the setup out of the way we defer to `emit_ir` to extend our list of instructions.
Once it's done, doing god knows what, we return our list of instructions and our list of local types.

### Emitting Instructions, Actually For Real This Time

We had to peel away a lot of layers, but we've `emit_ir` takes on a familiar form:

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

                                                                 
* EmitWasm                                                       
  * Converts our Item into a set of wasm instructions            
  * Each item's instructions are kept within the CodeSection
  * `emit_item`
    * Immediately calls `emit_body`.
    * Call returns a list of instructions and list of local types.
    * When we declare a function we have to declare how many locals of each kind it has.
      * The list of local tys from `emit_body` accomplishes this for us
    * Every function has an implied return and end as it's final instructions.
      * We append those after adding the rest of our instructions to our function body.
  * `emit_body`
    * Turns an IR (and the enclosing parameters) into a vector of instructions
    * Functions predeclare how many locals they have of what type
      * Might expect this is number of variables in our term
      * We'll actually have more locals than variables
      * We won't precalc number of locals instead we'll use `EmitLocals` to track and delay constructing our function till after we emit our list of instructions.
      * Segue to emit locals
    * Parameters are considered locals in wasm.
      * We don't have to declare them as locals in our function, but we do reference them with `local.get`.
    * EmitLocals
      * Converts vars into wasm locals.
      * Tracks important metadata about locals:
        * Local index
        * Types of each local
      * Provides 3 functions with which to do so.
      * `param_for`
        * maps var to local.
        * parameters are considered locals by index, but we don't include their type.
        * We only need the types of locals for the function declaration.
        * Parameter types are already known from the function signature, so no need to redeclare their type even though they are considered locals.
        * TODO: We haven't yet explained functions or function declarations.
          * Either move that explanation above this, or move this below that explanation.
      * `local_for`
        * this is our most common case
        * converts a var into a wasm local
        * create a fresh local for it.
        * track that local's type, we'll need this at the end when we construct our wasm function.
        * maps our variable id to our fresh local, alongside its type.
      * `anon_local`
        * sometimes we need a local without a corresponding var
        * we create a fresh local
        * we track it's type, but we do not map a var to it.
    * Back in `emit_body`
    * If we have a closure env as our first parameter
      * Cast our first parameter from the abstract `struct` type to our concrete environment type
      * Rebind the local for our `env` parameter to reference the cast local, not the abstract struct parameter.
  * `emit_ir`
    * Emits instructions to produce the value for a given IR.
    * An important assumption: any `IR` will produce a single value on the stack.
      * We'll make use of this assumption to order our instructions such that we can avoid saving intermediary computations in locals.
    * Cases:
      * Var
        * Load value from the local that holds our var.
      * Int
        * Emit a constant.
        * Takes the literal value we emit here and places it onto the stack, so it's available to other instructions.
      * Closure
        * Allocate a sturct containing:
          * A function reference to our closure body.
          * The free variables captured by our closure.
        * Use item_id to lookup function index. We'll use this index to construct our reference.
          * TODO: Code
        * Get the type of our closure env and our closure as a function reference
          * TODO: Code
          * Closure env is the concrete struct type `{func, env[0], env[1]}`
          * Closure type is a substruct of the concrete type with just the func `{func}`
        * using our function and type indices emit instructions that:
          * TODO: Code
          * create a function reference
          * get the values of free variables, importantly in the right order
          * call struct.new to create a struct with our closure env type
          * cast that created struct to our closure type (`{ func }`)
      * Apply
        * Unpacks a closure and calls its function reference with its env
        * TODO: Haven't talked about `emit_closure_index` yet.
        * Steps
          * Emit instructions to get our closures value
          * Create a local to hold our closure
          * Emit instructions for our argument
          * Using the local we created, extract the function reference from our closure
          * Our stack looks like (top to bottom):
            * function reference
            * arg
            * closure
      * Local
        * Locals turn into a sequencing operation
        * Emit instructions for defn.
        * Save that value in a local.
        * Emit the instructions for body, which will presumably reference the local we just set.
        * This only works because we can be confident that defn is a single value at the top of the stack.
      * Access
        * Straightforward:
          * Emit instructions for struct.
          * Get the field out of said struct.
        * Because wasm is typed, we must determine our struct type to emit these instructions.
        * This is simplified by our assumption that the struct is.

* Conclusion
  * Example output
    * Output doesn't look very efficient
    * Run it through `wasm-opt`
    * It doesn't look any better
  * We finally have a working compiler we can produce executable code
    * Pan to gaping hole in the frontend of our compiler
    * Finally time to do something about it
