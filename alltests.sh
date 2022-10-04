#!/bin/sh

[ ! -d .git ] && "You need to be in the repo root to execute this" && exit

for i in $(find tests -type f | cut -d "/" -f 2 | cut -d "." -f 1 | sort -h)
do
    cargo r -- --exec "tests/${i}.lua";
    if [[ ! "$?" == 0 ]]; then
        echo error!
        exit
    fi
done
