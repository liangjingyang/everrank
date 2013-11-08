%%----------------------------------------------------
%% Erlang模块热更新到所有线路（包括server的回调函数，如果对state有影响时慎用）
%% 调试：ud(mod_xx | [mod_xx, ...])      %% 打开相应mod的?DEBUG调试宏
%%       uud(mod_xx | [mod_xx, ...])    %% 关闭相应mod的?DEBUG宏
%%
%% 检查：uch()                           %% 列出前5分钟内编译过的文件
%%       uch(N)                          %% 列出前N分钟内编译过的文件
%%
%% 更新：uu()                           %% 更新前5分钟内编译过的文件               
%%       uu(N)                          %% 更新前N分钟内编译过的文件   
%%       uu([mod_xx, ...])              %% 指定模块（不带后缀名）
%%       uu(m)                          %% 编译并加载文件
%% 
%% 编译：uc(mod_xx | [mod_xx, ...])     %% 编译文件但不热更新
%%
%% 编译并更新：
%%       ucu(mod_xx | [mod_xx, ...])    %% 编译并热更新
%%       
%% Tips: d - debug, ud - undebug, u - update, c - compile, cu - compile and update, ch - check 
%% 
%% @author ljy0922@gmail.com 
%%----------------------------------------------------

-module(user_default).
-compile(export_all).
-include_lib("kernel/include/file.hrl").
%%-include("common.hrl").
%%-include("proto.hrl").

-define(UPRINT(Format),
    io:format(Format++"~n", [])).
-define(UPRINT(Format, Args),
    io:format(Format++"~n", Args)).

-define(NONE, none).
-define(COMPILE_OPT, 
    [
        {i, "."},
        {i, "../"},
        {i, "../include"},
        {i, "../deps/gateway"},
        {i, "../src"},
        {i, "../src/*"},
        {i, "../src/*/*"},
        report,
        bin_opt_info,
        warn_obsolete_guard,
        warn_shadow_vars,
        %warnings_as_errors,
        verbose
    ]
    ).

%% 禁用的命令
q() ->
    io:format("WARNING!!! You should never use this command...~n\"Ctrl + c\" twice to quit this shell.\n").                         


%% 调试
ud(Mod) ->
    udebug(Mod, true).

uud() ->
    ExcModList = logger:get_exceptions(),
    udebug(ExcModList, false).

uud(Mod) ->
    udebug(Mod, false).

udebug(Mod, Boolean) when is_atom(Mod) ->
    udebug([Mod], Boolean);
udebug(ModList, Boolean) when is_list(ModList) ->
    Res = loglevel:set_exception(ModList, Boolean),
    ExcModList = logger:get_exceptions(),
    ?UPRINT("----debug modules----~n ~w~n", [ExcModList]),
    Res;
udebug(_, _) ->
    {error, badarg}.

%% 检查
uch() ->
    uc(5).
uch(S) when is_integer(S) ->
    c:cd("../ebin"),
    case file:list_dir(".") of
        {ok, FileList} -> 
            Files = get_new_file(FileList, S * 60),
            ?UPRINT("----check modules----~n~w~n====check modules====", [Files]);
        Any -> ?UPRINT("Error Dir: ~w", [Any])
    end;
uch([S]) when is_atom(S) ->
    S1 = misc:to_integer(misc:to_list(S)),
    case is_integer(S1) of
        true  ->
            c:cd("../ebin"),
            case file:list_dir(".") of
                {ok, FileList} -> 
                    Files = get_new_file(FileList, S * 60),
                    ?UPRINT("----check modules----~n~w~n====check modules====", [Files]);
                Any -> ?UPRINT("Error Dir: ~w", [Any])
            end;
        _ ->
            ?UPRINT("ERROR======> Badarg ~p/~p ~n", [S, S1])
    end;
uch(S) -> ?UPRINT("ERROR======> Badarg ~p ~n", [S]).

uadmin()->
    spawn(fun()->uu(m) end),
    ok.

%% 更新
uu() ->
    uu(5).
uu(m) ->
    StartTime = misc:now(),
    ?UPRINT("-----makes-----", []),
    c:cd("../"),
    make:all(),
    c:cd("ebin"),
    EndTime = misc:now(),
    Time = EndTime - StartTime,
    ?UPRINT("Make Time : ~w s", [Time]),
    uu(Time / 60);
uu(S) when is_number(S) ->
    case file:list_dir("../ebin/") of
        {ok, FileList} -> 
            Files = get_new_file(FileList, misc:ceil(S * 60) + 3),
            ul(Files);
        Any -> ?UPRINT("Error Dir: ~w", [Any])
    end;
uu(Files) when (is_list(Files) orelse is_atom(Files)) ->
    ul(Files);
uu(_) -> ?UPRINT("ERROR======> Badarg", []).

%% 编译但不热更新
uc(Mod) when is_atom(Mod) ->
    uc([Mod]);
uc(Mods) ->
    ucu(Mods, false).

%% 编译并更新
ucu(Mod) when is_atom(Mod) ->
    ucu([Mod]);
ucu(Mods) ->
    ucu(Mods, true).

ucu(Mods, IsUpdate) ->
    code:add_path("../ebin"),
    case catch
        [begin
            case catch find_file(atom_to_list(Mod)) of
                ?NONE ->
                    throw({error, {no_mod, Mod}});
                {ok, FilePath} ->
                    ?UPRINT("~ncompiling:~s", [FilePath]),
                    compile_file(Mod, FilePath)
            end
        end || Mod <- Mods] 
    of
        {error, _} = Errors ->
            Errors;
        Result ->
            case IsUpdate of
                true ->
                    uu(Mods),
                    ?UPRINT("~n====== compile and update complete! ======",[]);
                false ->
                    ?UPRINT("~n====== compile complete! ======",[])
            end,
            Result
            
    end.


%% m(['src/data/*','src/lib/lib_goods.erl'])
um(Files) when is_list(Files) ->
    StartTime = misc:now(),
    ?UPRINT("-----makes-----~n~w~n", [Files]),
    c:cd("../"),
    Res = make:files(Files, [debug_info,{i, "include"},{outdir, "ebin"}]),
    c:cd("ebin"),
    EndTime = misc:now(),
    Time = EndTime - StartTime,
    ?UPRINT("Make Time : ~w s", [Time]),
    Res.


%% 更新到所有线路
ul(Files) ->
    Nodes = [node()|nodes()],
    ?UPRINT("~nupdate module: ~w~nupdate nodes: ~w", [Files, Nodes]),
    [rpc:call(Node, ?MODULE, uload, [Files])||Node<-Nodes].

uload([]) -> ok;
uload([FileName | T]) ->
    % 这里要注意 运行老代码的进程小心被kill
    case c:l(FileName) of
        {module, _} ->
            uload(T);
        {error, Error} ->
            {Error, FileName}
    end;
uload(FileName) when is_atom(FileName) ->
    uload([FileName]).

%% 重新加载配置文件
ulc(File) when is_atom(File) ->
    Nodes = [node()|nodes()],
    ?UPRINT("~nreload config: ~w~nupdate nodes: ~w", [File, Nodes]),
    [rpc:call(Node, config_dyn, reload, [File])||Node<-Nodes].
%% 热更新时兼顾使用老代码的进程
%    case code:soft_purge(FileName) of
%        true ->
%            case code:load_file(FileName) of
%                {module, _} ->
%                    ?UPRINT("loaded: ~w", [FileName]),
%                    ok;
%                    %% ?UPRINT("loaded: ~w", [FileName]);
%                {error, What} -> ?UPRINT("ERROR======> loading: ~w (~w)", [FileName, What])
%            end;
%        false -> ?UPRINT("ERROR======> Processes lingering : ~w [zone ~w] ", [FileName, srv_kernel:zone_id()])
%    end,
%    load(T).

%% 内部函数
get_new_file(Files, S) -> 
    get_new_file(Files, S, []).
get_new_file([], _S, Result) -> Result;
get_new_file([H | T], S, Result) ->
    NewResult = case string:tokens(H, ".") of
        [Left, Right] when Right =:= "beam" ->
            case file:read_file_info("../ebin/" ++ H) of
                {ok, FileInfo} -> 
                    Now = calendar:local_time(),
                    case calendar:time_difference(FileInfo#file_info.mtime, Now) of
                        {Days, Times} -> 
                            Seconds = calendar:time_to_seconds(Times), 
                            case Days =:= 0 andalso Seconds < S of
                                true ->
                                    FileName = list_to_atom(Left),
                                    [FileName | Result];
                                false -> Result
                            end;
                        _Error1 -> 
                            ?UPRINT("~w", [_Error1]), 
                            Result
                    end;
                _Error2 -> 
                    ?UPRINT("~w", [_Error2]), 
                    Result
            end;
        _ -> Result
    end,
    get_new_file(T, S, NewResult).

find_file(Mod) ->
    FileName =
    case filename:extension(Mod) of
        "" ->
            Mod ++ ".erl";
        ".erl" ->
            Mod
    end,
    % 寻找模块
    Root = "../",
    %?UPRINT("root路径:~s\n", [Root]),
    [begin
        case filelib:wildcard(FindPath) of
            [] ->
                ok;
            [File] ->
                throw({ok, File})
        end
    end || FindPath <- 
        [
        filename:join([Root, "deps", "*", "src", FileName]),
        filename:join([Root, "deps", "*", "src", "*", FileName]),
        filename:join([Root, "deps", "*", "src", "*", "*", FileName]),
        filename:join([Root, "deps", "*", "src", "*", "*", "*", FileName]),
        filename:join([Root, "src", FileName]),
        filename:join([Root, "src", "*", FileName]),
        filename:join([Root, "src", "*", "*", FileName]),
        filename:join([Root, "src", "*", "*", "*", FileName])
        ]
    ],
    ?NONE.

compile_file(Mod, FilePath) ->
    compile_file(Mod, FilePath, []).

compile_file(_Mod, FilePath, OtherOpts) ->
    SrcN = string:str(FilePath, "src"),
    Path = string:substr(FilePath, 1, SrcN + 3),
    Opts = lists:append([[{outdir, Path ++ "../ebin"} | ?COMPILE_OPT],OtherOpts]),
    %?UPRINT("compile opts:~p~nFilePath:~p~n", [Opts, FilePath]),
    case compile:file(FilePath, Opts) of
        {ok, _Data} ->
            ?UPRINT("compile succ:~p!", [_Data]),
            ok;
        {ok, _, Warnings} ->
            ?UPRINT("compile succ!", []),
            ?UPRINT("warnings:~n~p", [Warnings]),
            ok;
        error ->
            throw({error, {compile_failed, FilePath}});
        {error, Errors, Warnings} ->
            throw({error, {compile_failed, FilePath, {error, Errors}, {warnings, Warnings}}})
    end.

   
