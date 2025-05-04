+++
title = "ClosureConvert[0].Base: Removing Functions From Our Language With Closure Conversion"
date = "2025-04-30T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "ClosureConvert"]
series = ["Making a Language"]
keywords = ["Programming Languages", "Compiler", "Functions", "IR", "Compiler", "Runtime", "Closure", "Closure Conversion", "Lambda Lifting"]
description = "Converting our functions into closures for the Base IR."
+++

* Intro
  * Explain our functions are lambdas
    * Explain lambdas?
  * Like polymorphism, no machine instructions for executing lambdas
  * Exemplify problem with variable capture when returning closures.
  * We have to convert them into a form we can execute

* Closure Conversion
  * Closure
    * Captures in scope variables.
      * Possibly explained with lambdas above.
    * A struct containing a field that points to our function body and a field for each of our captured variables
  * Convert each `Fun` node to a closure
    * We must also update our `App` to call closures rather than functions.
  * Our function body will be turned in to a top level item (that captures no environment).
    * In place of implicit capturing, it will take an explicit parameter to the env.
    * Each access of a captured variable in the body will be replaced by indexing into the env parameter.
  * Explain runtime semantics
    * Functions will now allocate a closure struct with the captured environment.
    * Applications will grab the function pointer out of a closure struct and pass the env and argument to the function.    
  * TODO: Do we need to explain more about closures here?

* Tangent: Figure out how to incorporate this organically.
  * Closure conversion is invasive.
  * Originally wanted to use a different techinque lambda lifting
  * Explain lambda lifting with an example.
  * Explain where lambda lifting fails (returning a lambda, partial applications).
  * Lambda lifting is not a wholistic solution, so we closure convert instead.

* Closure conversion requires a new IR.
  * Closures are a fundamentally different type from functions because they track their env.
  * A function type `Type::fun(Type::Int, Type::Int)` turns into one closure type per captured environment.
    * Part of why we have to update `App`.
  * Alongside this main change, our new IR introduces some other changes
    * Top level items
    * Structs to represent environment parameters.
    * Note: These are things we will add to our first IR in rows and items, but we need them in base for closure conversion and we need the new IR anyways.
  * TODO: IR code snippet.
  * Explain what stays the same
    * Var, but with a new type
    * Int
    * Local

* IR Type
  * With the changes to our IR we need a new type for our IR
  * TODO: IR type code snippet.
  * Explain Closure and ClosureEnv
    * Why not Struct for both?
    * Makes our lives easier to remember them as special types during code emission

* Main entry point
  * `closure_convert`
  * TODO: Signature code snippet
  * Takes in lowering::IR spits out `ClosureConvertOutput`.
  * TODO: ClosurecConvertOutput snippet
  * A collection of items produced by lifting function bodies to top level items and the final item representing our input IR.

* closure_convert impl
  * split funs, we don't want to turn top level functions into closures.
    * explain why
  * `var_supply` we've seen before, but now it converts lower::Var into Var
  * `env` tracks mapping from lowering::Var to Var.
  * Convert parameters and insert them in env
  * Lower ret_ty, used for convenience in the item we construct at the end.

* lower_ty
 * TODO: Code snippet
 * Int turns into I32
 * Fun turns into Closure
 * TyFun and Var shouldn't appear because this is post monomorphizing
 * Explain why nothing creates a ClosureEnv type? 

* Construct `ClosureConvert`
* TODO: ClosureConvert snippet
  * item_supply is like var_supply but for top level items
  * items holds the top level items we create for closures

* convert
  * TODO: Signature code snippet.
  * Takes in lowering::IR and env, spits out our new IR
  * Case by case 
    * TODO: Int snippet
    * Int turns into Int

    * TODO: Var snippet
    * Var turns into Var by look up in env
    * Should we talk about why we don't convert it each time?
    
    * TODO: Fun snippet
    * Simple on the surface
    * Convert to var and call make_closure

* make_closure
  * lower return type
  * convert body
  * determine free vars
  * remove our parameter from free vars.
  * create env_var from captured vars.
  * substitute body to replace captured vars with environment accesses.
  * construct an item for our closure body
  * return closure

* Back in `convert`
  * TODO: `App` snippet
  * Turn into an `apply`
  * Should we talk about distinction between apply and app?

  * TODO: `Local` snippet
  * Nothing noteworthy

  * TyFun and TyApp should not appear

* Clean up in convert


* Caveats
  * Closures are single parameter rather than multi parameter
    * Making of a fast curry
  * Cheating on typing
    * Proper typing requires existentials, instead we've just special cased the behavior

* Conclusion
  * Final step before code generation.
  * Closures are something we can see how to implement in a machine, unlike functions.
  * Closure conversion provides a wholistic solution, unlike lambda lifting, at the cost of being more complex and passing around allocated closures.
