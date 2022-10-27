#!/bin/sh

[ ! -d .git ] && "You need to be in the repo root to execute this" && exit

for i in $(find tests -type f | cut -d "/" -f 2 | cut -d "." -f 1 | sort -h)
do
    echo "========="
    echo "tests/${i}.lua"
    echo "========="
    echo

    cp tests/${i}.lua file.lua
    [ -f ./build./exe ] && rm ./build/exe
    ./main.sh
    [ ! -f ./build/exe ] && echo "compilation failed!" && exit
    res1=$(./build/exe)
    echo "$res1"
    res2=$(lua "tests/${i}.lua")
    if [[ ! "$res1" == "$res2" ]]; then
        echo different output:
        echo lua2llvm:
        echo "'$res1'"
        echo lua:
        echo "'$res2'"
        exit
    fi

    echo
    echo
done
