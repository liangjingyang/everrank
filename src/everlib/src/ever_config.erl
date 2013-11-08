%% LastChange: 2013-07-03 11:02:38
%% Copr. (c) 2013-2015, Simple <ljy0922@gmail.com>

-module(ever_config).

-export([init/0]).
-export([reload_all/0,reload/1]).
-export([init/1,list/1]).

-export([find/2]).
-export([find_by_module/2, list_by_module/1]).

-define(FOREACH(Fun,List),lists:foreach(fun(E)-> Fun(E)end, List)).

-define(DEFINE_CONFIG_MODULE(Name,FilePath,FileType),{ Name, Name, FilePath, FileType }).

-define(DEFINE_CONFIG_MODULE_EX(Name,FilePath,FileType,KeyType), { Name, Name, FilePath, FileType, KeyType }).

%% 支持4种文件类型：record_consult,key_value_consult,key_value_list,record_list,

-define(CONFIG_FILE_LIST,[    
        %%配置模块名称,路径,类型
        ?DEFINE_CONFIG_MODULE(c_router, "c_router.config", key_value_consult),
        ?DEFINE_CONFIG_MODULE(c_item, "c_item.config", record_consult),
        ?DEFINE_CONFIG_MODULE(c_skill, "c_skill.config", record_consult),
        ?DEFINE_CONFIG_MODULE(c_mon, "c_mon.config", record_consult),
        ?DEFINE_CONFIG_MODULE(c_elite, "c_elite.config", record_consult),
        ?DEFINE_CONFIG_MODULE(c_trap, "c_trap.config", record_consult),
        ?DEFINE_CONFIG_MODULE(c_trap_lv, "c_trap_lv.config", record_consult),
        ?DEFINE_CONFIG_MODULE(c_mon_reborn, "c_mon_reborn.config", record_consult),
        ?DEFINE_CONFIG_MODULE(c_exp, "c_exp.config", key_value_consult),
        ?DEFINE_CONFIG_MODULE(c_mon_to_soul, "c_mon_to_soul.config", key_value_consult),
        ?DEFINE_CONFIG_MODULE(c_trap_to_know, "c_trap_to_know.config", key_value_consult),
        ?DEFINE_CONFIG_MODULE(c_elite_skill, "c_elite_skill.config", key_value_consult),
        ?DEFINE_CONFIG_MODULE(c_hero_lv, "c_hero_lv.config", record_consult)
    ]).


%% ====================================================================
%% API Functions
%% ====================================================================
init()->
    AllFileList = 
    lists:map(
        fun({AtomName,ConfigModuleName,FilePath,FileType}) ->
                {AtomName,ConfigModuleName,get_config_path() ++ FilePath, FileType};
            ({AtomName, ConfigModuleName, FilePath, Type, KeyType}) ->
                {AtomName, ConfigModuleName, get_config_path() ++ FilePath, Type, KeyType}
        end, ?CONFIG_FILE_LIST), 
    ?FOREACH(catch_do_load_config, AllFileList),
    ok.

%%@result   ok | {error,not_found}
init(ConfigName) when is_atom(ConfigName)->
    reload(ConfigName).

reload_all()->    
    AllFileList = 
    lists:map(
        fun({AtomName,ConfigModuleName,FilePath,FileType}) ->
                {AtomName,ConfigModuleName,get_config_path() ++ FilePath, FileType};
            ({AtomName, ConfigModuleName, FilePath, Type, KeyType}) ->
                {AtomName, ConfigModuleName, get_config_path() ++ FilePath, Type, KeyType}
        end, ?CONFIG_FILE_LIST),                          
    ?FOREACH(catch_do_load_config,AllFileList),
    ok.

%%@spec reload(ConfigName::atom())
%%@result   ok | {error,not_found}
reload(ConfigName) when is_atom(ConfigName)->
    AllFileList = 
    lists:map(
        fun({AtomName,ConfigModuleName,FilePath,FileType}) ->
                {AtomName,ConfigModuleName,get_config_path() ++ FilePath, FileType};
            ({AtomName, ConfigModuleName, FilePath, Type, KeyType}) ->
                {AtomName, ConfigModuleName, get_config_path() ++ FilePath, Type, KeyType}
        end, ?CONFIG_FILE_LIST),   
    case lists:keyfind(ConfigName, 1, AllFileList) of
        false->
            {error,not_found};
        ConfRec->
            reload2(ConfRec),
            ok
    end.

reload2({AtomName,ConfigModuleName,FilePath,FileType}) ->
    reload2({AtomName,ConfigModuleName,FilePath,FileType,set});

reload2({AtomName,ConfigModuleName,FilePath,_,_}=ConfRec) ->
    try
        {ok, _Code} = do_load_config(ConfRec)
    catch
        Err:Reason->
            io:format("Reason=~w,AtomName=~w,ConfigModuleName=~p,FilePath=~p~n",[Reason,AtomName,ConfigModuleName,FilePath]),
            throw({Err,Reason})
    end.

%%@spec list/1
%%@doc 为了尽量少改动，接口符合ets:lookup方法的返回值规范，
%%@result   [] | [Result]
list(ConfigName)->
    case do_list(ConfigName) of
        undefined-> [];
        not_implement -> [];
        Val -> Val
    end.

%%@spec find/2
%%@doc 为了尽量少改动，接口符合ets:lookup方法的返回值规范，
%%@result   [] | [Result]
find(ConfigName,Key)->
    case do_find(ConfigName,Key) of
        undefined-> [];
        not_implement -> [];
        Val -> [Val]
    end.

%%@spec list_by_module/1
%%@result   [] | [Result]
list_by_module(ModuleName) when is_atom(ModuleName)->
    case ModuleName:list() of
        undefined-> [];
        not_implement -> [];
        Val -> Val
    end.

%%@spec find_by_module/2
%%@doc  为了尽量少改动，接口符合ets:lookup方法的返回值规范，
%%@result   [] | [Result]
find_by_module(ModuleName,Key) when is_atom(ModuleName)->
    case ModuleName:find_by_key(Key) of
        undefined-> [];
        not_implement -> [];
        Val -> [Val]
    end.


%%@spec do_list/1
do_list(ConfigName) ->
    ConfigName:list().

%%@spec do_find/2
do_find(ConfigName,Key) ->
    ConfigName:find_by_key(Key).

%% ====================================================================
%% Local Functions
%% ====================================================================


%%TODO:get_argument需要修改
get_root_path() ->
    {ok, [[Root]]} = init:get_argument(root_path),
    Root.

get_config_path() ->
    get_root_path() ++ "config/".


catch_do_load_config({AtomName,ConfigModuleName,FilePath,FileType}) ->
    catch_do_load_config({AtomName,ConfigModuleName,FilePath,FileType,set});
catch_do_load_config({AtomName,ConfigModuleName,FilePath,_,_}=ConfRec) ->
    try
        do_load_config(ConfRec)
    catch
        Err:Reason->
            io:format("Reason=~w,AtomName=~w,ConfigModuleName=~p,FilePath=~p~n",[Reason,AtomName,ConfigModuleName,FilePath]),
            throw({Err,Reason})
    end.




do_load_config({_AtomName,ConfigModuleName,FilePath,record_consult, Type}) ->
    {ok,RecList} = file:consult(FilePath),
    KeyValues = [ begin
                Key = element(2,Rec), {Key,Rec}
        end || Rec<- RecList ],
    ValList = RecList,
    do_load_gen_src(ConfigModuleName,Type,KeyValues,ValList);

do_load_config({_AtomName,ConfigModuleName,FilePath,record_list, Type}) ->
    {ok,[RecList]} = file:consult(FilePath),
    KeyValues = [ begin
                Key = element(2,Rec), {Key,Rec}
        end || Rec<- RecList ],
    ValList = RecList,
    do_load_gen_src(ConfigModuleName,Type,KeyValues,ValList);

do_load_config({_AtomName,ConfigModuleName,FilePath,key_value_consult, Type})->
    {ok,RecList} = file:consult(FilePath),
    KeyValues = RecList,
    ValList = RecList,
    do_load_gen_src(ConfigModuleName,Type,KeyValues,ValList);

do_load_config({_AtomName,ConfigModuleName,FilePath,key_value_list, Type})->
    {ok,[RecList]} = file:consult(FilePath),
    KeyValues = RecList,
    ValList = RecList,
    do_load_gen_src(ConfigModuleName,Type,KeyValues,ValList).

%%@doc 生成源代码，执行编译并load
do_load_gen_src(ConfigModuleName,Type,KeyValues,ValList)->
    try
        Src = gen_src(ConfigModuleName,Type,KeyValues,ValList),
        {Mod, Code} = dynamic_compile:from_string( Src ),
        code:load_binary(Mod, misc:to_list(ConfigModuleName) ++ ".erl", Code),
        {ok, Code}
    catch
        Type:Reason -> 
            Trace = erlang:get_stacktrace(), string:substr(erlang:get_stacktrace(), 1,200),
            io:format("Error compiling ~p: Type=~w,Reason=~w,Trace=~w~n", [ConfigModuleName, Type, Reason,Trace ])
    end.

gen_src(ConfModuleName,Type,KeyValues,ValList) ->
    KeyValues2 =
        if Type =:= bag ->
                lists:foldl(fun({K, V}, Acc) ->
                                    case lists:keyfind(K, 1, Acc) of
                                        false ->
                                            [{K, [V]}|Acc];
                                        {K, VO} ->
                                            [{K, [V|VO]}|lists:keydelete(K, 1, Acc)]
                                    end
                            end, [], KeyValues);
           true ->
                KeyValues
        end,
    Cases = lists:foldl(fun({Key, Value}, C) ->
                                lists:concat([C,lists:flatten(io_lib:format("find_by_key(~w) -> ~w;\n", [Key, Value]))])
                        end,
                        "",
                        KeyValues2),
    StrList = lists:flatten(io_lib:format("     ~w\n", [ValList])),
    
"
-module(" ++ misc:to_list(ConfModuleName) ++ ").
-export([list/0,find_by_key/1]).

list()->"++ StrList ++".\n\n" ++ Cases ++
"find_by_key(_Key) -> undefined.
".

