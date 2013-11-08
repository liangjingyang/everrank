#!/bin/bash

NUM=1
if [ "$1" != '' ]; then
    NUM=$1
fi

erl -pa ../ebin ../deps/*/ebin \
    -name ever_debug_$NUM@127.0.0.1 \
    -root_path $PWD/../ \
    -remsh ever@127.0.0.1
