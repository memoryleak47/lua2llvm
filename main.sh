#!/bin/sh

echo compiling lua2llvm ...
cargo b

[ ! -d build ] && mkdir build

echo running lua2llvm ...
./target/debug/lua2llvm file.lua 2> build/file.ll

echo compiling extra.cpp ...
clang++ extra/extra.cpp -S -emit-llvm -o build/extra.ll

echo running clang ...
clang++ build/*.ll -o build/exe

echo executing exe ...
./build/exe
