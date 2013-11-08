#!/bin/bash

START=server_app


erl -pa ../ebin ../deps/*/ebin \
    -detached \
    -name ever@127.0.0.1 \
    -mnesia dump_log_write_threshold 100000 \
    -mnesia no_table_loaders 100 \
    -mnesia dir \"../ever_database\" \
    -root_path $PWD/../ \
    -s $START 
