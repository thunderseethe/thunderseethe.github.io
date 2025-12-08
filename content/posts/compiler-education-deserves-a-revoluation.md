+++
title = "Compiler Education Deserves a Revolution"
date = "2024-03-09T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages"]
keywords = ["Programming Languages", "Lazy", "Eager", "Call-by-value", "Call-by-name", "Call-by-need", "Call-by-push-value", "Evaluation Order", "Evaluation Strategy", "Type Inference", "Code Generation", "IR", "Functional"]
description = "Why call-by-push-value is the best evaluation strategy, and why it's going to take over"
draft = true
+++

Crack open any \plum{compiler} tome from the last century and you'll find some variant of the same architecture.
A pipeline that runs each pass of the \plum{compiler} over your entire code before shuffling its output along to the next pass.
The pipeline halts at the first error, throwing away any work that's been completed.

Crack open any compiler, written this millennium, and you'll find nothing of the sort.
A silent shift has occurred in compiler architecture.
Modern \plum{compilers} almost unilaterally use a query based model.

Rather than run each pass to completion, compilation is structured as a series of queries depending on each other.
You don't call lexing and then parsing.
You ask the \plum{compiler} "what does the parsed syntax tree of this file look like?" and the \plum{compiler} goes off and lexes the file as part of answering your enquiry.
Compilation no longer stops at the first error. An error in one query does nothing to block another, allowing us to collect multiple errors or even ignore errors in unrelated portions of our code.

Query based compilation is motivated by two factors: incremental reuse and Integrated Development Environments (IDEs).
As languages have grown more featureful, \plum{compilers} have taken on more work to keep up.
It's increasingly important that \plum{compilers} work incrementally, determining the code that has changed since last compilation and only recompiling changed code.
The query model helps with this because each query tracks what queries they depend upon.
If all a query's dependents are unchanged, we know the output of the query is unchanged and we can reuse its cached value.

\begin{wrapfigure}{R}{0.4\textwidth}
\centering
\begin{tikzpicture}[node distance=2.5cm, sibling distance=1.75cm]

\node [startstop, align=center] {Codegen\\ main}
  child {node [startstop, align=center] {Lower\\ main}
    child {node [startstop, align=center] {Lower\\ foo}}
    child {node [startstop, align=center] {Type\\ main}}
  }
  child {node [startstop, align=center] {Codegen\\ foo}
    child[grow=300] {node [startstop, align=center] {Lower\\ foo}}
  };

\end{tikzpicture}
\caption{Query Based Compiler}
\end{wrapfigure}

IDEs are only growing in popularity.
Especially with the arrival of the Language Server Protocol (LSP) bringing IDE features to your favorite editor (unless your favorite editor is nano; very sorry about that).
With this rise in popularity, the way we use \plum{compilers} has changed.
Our usage is more fine grained than before.
We don't want to know the types of our whole program, just the type of the function we're looking at right now.
I don't need the definition of every variable in my program, just the definition of the variable under my cursor.

Queries also help us here.
We can construct queries that run over a single function, or even a single variable, and they'll only depend on the queries for that function.
Executing the minimal set of queries for our function allows us to answer queries faster.
This is important for IDEs because the user is sitting there waiting for the \plum{compiler} to get back to them.
The faster we can answer, the better and queries let us do the minimal amount of work to answer.

Query based \plum{compilers} are all the rage: Rust, Swift, Kotlin, Haskell, and Clang all structure their \plum{compilers} as queries.
If you want to learn how these new optimal incremental \plum{compilers} work, however, you're hard pressed to find resources.
Let this be your call to action: persuade your professors, pester your local PL passionates, phone your representatives.
We need more educational material on query-based \plum{compilers}.
