


erl -pa ../ebin ../deps/cowboy/ebin ../deps/cowlib/ebin ../deps/jsx/ebin ../deps/ranch/ebin -name ever@127.0.0.1 -mnesia dump_log_write_threshold 100000 -mnesia no_table_loaders 100 -mnesia dir \"../ever_database\" -root_path ../ -s everrank_app
#    -detached \
