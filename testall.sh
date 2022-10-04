#!/bin/sh

[ ! -d .git ] && "You need to be in the repo root to execute this" && exit

for file in $(find tests -type f)
do
    cargo r -- --exec "$file";
    if [[ ! "$?" == 0 ]]; then
        echo error!
        exit
    fi
done
