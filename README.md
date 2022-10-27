lua2llvm
========

In this toy project, I intend to write a compiler for Lua.

I want to see whether static analysis of code for dynamic languages can be utilized to

generate fast binaries.


Goals:

- performance


Explicit non-goals:

- Lua feature parity

- fast compile times


Currently this project contains my lua "reference implementation" interpreter.

It can be invoked with `cargo run <main.lua>`


The project now also contains a simple compiler from Lua to LLVM IR.

`file.lua` can be compiled and executed by running `main.sh`.
