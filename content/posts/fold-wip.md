+++
title = "In Search of the Perfect Fold"
date = "2023-10-21T00:00:00Z"
author = "thunderseethe"
tags = ["Programming Languages", "Rust"]
series = []
keywords = ["Programming Languages", "Compiler", "Recursion Schemes", "Fold", "Iterators", "AST", "Trees", "Tree Transformations"]
description = "A quest to design the perfect tree fold for Rust"
+++

I'm writing a compiler in Rust.
Which is just to say, I do a lot of tree folds in Rust.
Rust shines at a lot of things.
I'm overall happy with my choice to use Rust.
It's just...for the life of me I can't write a satisfactory fold over trees.

Perhaps it's a personal failing.
The perfect fold is out there, and I'm simply ignorant of its divine design.
If you're reading this and thinking that, please I beg of you enlighten me.
If you're reading this and not thinking that, first off ohmigosh thank you, and secondly welcome to my tar pit.

# What's a fold?

Rust aficionado's might eagerly point out Rust has a builtin fold: [Iterator::fold]

# Not talking about Iterator fold
