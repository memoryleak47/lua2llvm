#!/bin/sh

[ ! -d .git ] && "You need to be in the repo root to execute this" && exit

for i in $(find tests -type f | cut -d "/" -f 2 | cut -d "." -f 1 | sort -h)
do
    res1=$(cargo r "tests/${i}.lua")
    if [[ ! "$?" == 0 ]]; then
        echo error!
        exit
    fi
    res2=$(lua "tests/${i}.lua")
    if [[ ! "$res1" == "$res2" ]]; then
        echo different output:
        echo lua2llvm:
        echo "'$res1'"
        echo lua:
        echo "'$res2'"
    fi
done
