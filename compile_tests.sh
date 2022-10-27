#!/bin/sh

[ ! -d .git ] && "You need to be in the repo root to execute this" && exit

rm -r build
mkdir build

echo compiling lua2llvm ...
cargo b

echo compiling extra files ...
for x in $(cd extra; ls *.cpp)
do
    clang++ "extra/$x" -S -emit-llvm -o "build/${x%.cpp}.ll"
done

for i in $(find tests -type f | cut -d "/" -f 2 | cut -d "." -f 1 | sort -h)
do
    echo "========="
    echo "tests/${i}.lua"
    echo "========="
    echo

    [ -f ./build./exe ] && rm ./build/exe
    ./target/debug/lua2llvm --compile "tests/${i}.lua" 2> build/file.ll
    clang++ build/*.ll -o build/exe
    [ ! -f ./build/exe ] && echo "compilation failed!" && exit
    res1=$(./build/exe)
    echo "$res1"
    res2=$(lua "tests/${i}.lua")
    if [[ ! "$res1" == "$res2" ]]; then
        echo different output:
        echo lua:
        echo "$res2"
        exit
    fi

    echo
    echo
done
