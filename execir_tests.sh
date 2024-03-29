#!/bin/sh

[ ! -d .git ] && "You need to be in the repo root to execute this" && exit

for i in $(find tests -type f | cut -d "/" -f 2 | cut -d "." -f 1 | sort -h)
do
    echo "========="
    echo "tests/${i}.lua"
    echo "========="
    echo

    res1=$(cargo r --release "tests/${i}.lua")
    if [[ ! "$?" == 0 ]]; then
        echo error!
        exit
    fi
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
