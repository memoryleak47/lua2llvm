#!/bin/sh

echo compiling lua2llvm ...
cargo b --release

rm -r build
mkdir build

echo running lua2llvm ...
./target/release/lua2llvm --compile file.lua 2> build/file.ll

echo compiling extra files ...
for x in $(cd extra; ls *.cpp)
do
    clang++ -O3 "extra/$x" -S -emit-llvm -o "build/${x%.cpp}.ll"
done

echo running clang ...
clang++ -O3 build/*.ll -o build/exe

echo executing exe ...
./build/exe
