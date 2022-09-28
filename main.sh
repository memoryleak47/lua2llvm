#!/bin/sh

echo compiling lua2llvm ...
cargo b
echo running lua2llvm ...
./target/debug/lua2llvm file.lua 2> file.ll
echo running clang ...
clang file.ll -o exe
echo executing exe
./exe
