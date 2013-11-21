
-module(ever_db).

                
-define(EVER_DB2, "ever_db2").
-compile(export_all).

abort(Reason) ->
    mnesia:abort(Reason).

activity(AccessContext, Fun, Args, AccessMod) ->
    mnesia:activity(AccessContext, Fun, Args, AccessMod).

add_table_copy(Tab, Node, Type) ->
    mnesia:add_table_copy(Tab, Node, Type).

add_table_index(Tab, AttrName) -> 
    mnesia:add_table_index(Tab, AttrName).

all_keys(Tab) ->
    mnesia:all_keys(Tab).

backup(Opaque) ->
    mnesia:backup(Opaque).

change_config(Config, Value) ->
    mnesia:change_config(Config, Value).

change_table_copy_type(Tab, Node, To) ->
    mnesia:change_table_copy_type(Tab, Node, To).

change_table_load_order(Tab, LoadOrder) ->
    mnesia:change_table_load_order(Tab, LoadOrder).

clear_table(Tab) ->
    mnesia:clear_table(Tab).

create_schema(NodeList) ->
    mnesia:create_schema(NodeList).

create_table(Name, TabDef) ->
    mnesia:create_table(Name, TabDef).

del_table_copy(Tab, Node) ->
    mnesia:del_table_copy(Tab, Node).

del_table_index(Tab, AttrName) ->
    mnesia:del_table_index(Tab, AttrName).

delete({Tab, Key}) ->
    mnesia:delete({Tab, Key}).

delete_object(Record) ->
    mnesia:delete_object(Record).

delete_table(Tab) ->
    mnesia:delete_table(Tab).

dirty_all_keys(Tab) ->
    mnesia:dirty_all_keys(Tab).

dirty_delete(Tab, Key) ->
    mnesia:dirty_delete(Tab, Key).

dirty_delete_object(Record) ->
    mnesia:dirty_delete_object(Record).

dirty_delete_object(Tab, Record) ->
    mnesia:dirty_delete_object(Tab, Record).

dirty_read(Tab, Key) ->
    ever_db2:dirty_read(Tab, Key).
    %mnesia:dirty_read(Tab, Key).

dirty_update_counter(Tab, Key, Incr) ->
    mnesia:dirty_update_counter(Tab, Key, Incr).

dirty_write(Tab, Record) ->
    ever_db2:dirty_write(Tab, Record).
    %mnesia:dirty_write(Tab, Record).

index_read(Tab, SecondaryKey, Pos) ->
    mnesia:index_read(Tab, SecondaryKey, Pos).

info() ->
    mnesia:info().

load_textfile(Filename) ->
    mnesia:load_textfile(Filename).

move_table_copy(Tab, From, To) ->
    mnesia:move_table_copy(Tab, From, To).

set_master_nodes(MasterNodes) ->
    mnesia:set_master_nodes(MasterNodes).

set_master_nodes(Tab, MasterNodes) ->
    mnesia:set_master_nodes(Tab, MasterNodes).

start() ->
    mnesia:start(),
    init().

stop() ->
    mnesia:stop().

system_info(Key) ->
    mnesia:system_info(Key).

table_info(Tab, InfoKey) ->
    mnesia:table_info(Tab, InfoKey).

wait_for_tables(TabList, Timeout) ->
    mnesia:wait_for_tables(TabList, Timeout).

frag_dirty_read(Tab, Key) ->
    Read = fun(T, K) -> mnesia:dirty_read(T, K) end,
    mnesia:activity(async_dirty, Read, [Tab, Key], mnesia_frag).

frag_dirty_write(Tab, Record) ->
    Write = fun(T, R) -> mnesia:dirty_write(T, R) end,
    mnesia:activity(sync_dirty, Write, [Tab, Record], mnesia_frag).

frag_info(Tab) ->
    Info = fun(T) -> mnesia:table_info(T, frag_properties) end,
    mnesia:activity(async_dirty, Info, [Tab], mnesia_frag).

frag_size(Tab) ->
    Size = fun(T) -> mnesia:table_info(T, frag_size) end,
    mnesia:activity(async_dirty, Size, [Tab], mnesia_frag).

frag_dist(Tab) ->
    Dist = fun(T) -> mnesia:table_info(T, frag_dist) end,
    mnesia:activity(async_dirty, Dist, [Tab], mnesia_frag).

activate_frag(Tab) ->
    mnesia:change_table_frag(Tab, {activate, []}).

add_frag(Tab, FragDist) ->
    mnesia:change_table_frag(Tab, {add_frag, FragDist}).

del_frag(Tab) ->
    mnesia:change_table_frag(Tab, del_frag).

init() ->
    {Mod,Code} = dynamic_compile:from_string(db_src()),
    code:load_binary(Mod, ?EVER_DB2 ++ ".erl", Code),
    ok.

set_tmp_method() ->
    {Mod,Code} = dynamic_compile:from_string(db_tmp_src()),
    code:load_binary(Mod, ?EVER_DB2 ++ ".erl", Code),
    ok.
set_method() ->
    {Mod,Code} = dynamic_compile:from_string(db_src()),
    code:load_binary(Mod, ?EVER_DB2 ++ ".erl", Code),
    ok.

db_tmp_src() ->
    "-module(" ++ ?EVER_DB2 ++ ").
    -compile(export_all).
    dirty_read(Tab, Key) -> 
        case proplists:get_value(base_table, mnesia:table_info(Tab, frag_properties)) of
            Tab ->
                Read = fun(T, K) -> mnesia:read(T, K) end,
                mnesia:activity(async_dirty, Read, [Tab, Key], mnesia_frag);
            _ ->
                mnesia:dirty_read(Tab, Key)
        end.
    dirty_write(Tab, Record) ->
        case proplists:get_value(base_table, mnesia:table_info(Tab, frag_properties)) of
            Tab ->
                Write = fun(T, R) -> mnesia:write(T, R, write) end,
                mnesia:activity(sync_dirty, Write, [Tab, Record], mnesia_frag);
            _ ->
                mnesia:dirty_write(Tab, Record)
        end.
    ".


db_src() ->
    SchemaList = ets:tab2list(schema),
    [_|FragTab] = lists:foldl(fun({schema, Name, List}, Acc)->
                case proplists:get_value(frag_properties, List) of
                    undefined ->
                        Acc;
                    Properties ->
                        case proplists:get_value(base_table, Properties) of
                            Name ->
                                Acc ++ "," ++ atom_to_list(Name);
                            _ ->
                                Acc
                    end
            end
    end, "", SchemaList),
    "-module(" ++ ?EVER_DB2 ++ ").
    -compile(export_all).
    dirty_read(Tab, Key) -> 
         case lists:member(Tab, [" ++ FragTab ++ "]) of
             true ->
                Read = fun(T, K) -> mnesia:read(T, K) end,
                mnesia:activity(async_dirty, Read, [Tab, Key], mnesia_frag);
             false ->
                mnesia:dirty_read(Tab, Key)
         end.
    dirty_write(Tab, Record) ->
         case lists:member(Tab, [" ++ FragTab ++ "]) of
             true ->
                Write = fun(T, R) -> mnesia:write(T, R, write) end,
                mnesia:activity(sync_dirty, Write, [Tab, Record], mnesia_frag);
             false ->
                mnesia:dirty_write(Tab, Record)
         end.
    ".

do_frag(Tab) ->
    io:format("do_frag, tab: ~w~n", [Tab]),
    set_tmp_method(),
    case proplists:get_value(base_table, mnesia:table_info(Tab, frag_properties)) of
        undefined ->
            activate_frag(Tab),
            add_frag(Tab, frag_dist(Tab));  
        BaseT when is_atom(BaseT) ->
            add_frag(BaseT, frag_dist(BaseT))
    end,
    set_method(),
    ok.
                 
                                 
