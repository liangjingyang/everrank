
-module(ever_log_level).

-export([start/0, set/1, get/0, set_all/1]).
-export([set_exception/2, set_exception2/2]).

-define(EVER_LOGGER, ever_logger).
-define(EVER_LOG_HANDLER, ever_log_handler).

%% Error levels:
-define(LOG_LEVELS,[ 
        {0, no_log, "No log"},
        {1, critical, "Critical"},
        {2, error, "Error"},
        {3, warning, "Warning"},
        {4, info, "Info"},
        {5, debug, "Debug"},
        {6, test, "Test"}
    ]).

start() ->
    try
        {Mod,Code} = dynamic_compile:from_string(logger_src(3, [])),
        code:load_binary(Mod, atom_to_list(?EVER_LOGGER) ++ ".erl", Code),
        error_logger:add_report_handler(?EVER_LOG_HANDLER, ""),
        error_logger:tty(false)
    catch
        Type:Error -> io:format("Error compiling ever_logger ~w: ~w~n", [Type, Error])
    end.

get() ->
    Level = ?EVER_LOGGER:get(),
    case lists:keysearch(Level, 1, ?LOG_LEVELS) of
        {value, Result} -> Result;
        _ -> erlang:error({no_such_loglevel, Level})
    end.

set_all(Loglevel) when is_integer(Loglevel) ->
    Nodes = [erlang:node()|erlang:nodes()],
    lists:foreach(
      fun(Node) ->
              Result = rpc:call(Node, ?MODULE, set, [Loglevel]),
              io:format("=================================~n"),
              io:format("Set Node: ~w ~w, Result:~w~n", [Node, ?MODULE, Result]),
              io:format("=================================~n")
      end, Nodes),
    ok.

set(LogLevel) when is_atom(LogLevel) ->
    set(level_to_integer(LogLevel));

set(Loglevel) when is_integer(Loglevel) ->
    case catch ?EVER_LOGGER:get_exceptions() of
        ExcModList when is_list(ExcModList) ->
            next;
        _ ->
            ExcModList = []
    end,
    set2(Loglevel, ExcModList);

set(_) ->
    io:format("Loglevel must be an integer:~n~p~n", [?LOG_LEVELS]),
    exit("Loglevel must be an integer:~n").

set2(Loglevel, ExcModList) when is_integer(Loglevel) ->
    try
        {Mod,Code} = dynamic_compile:from_string(logger_src(Loglevel, ExcModList)),
        code:load_binary(Mod, atom_to_list(?EVER_LOGGER) ++ ".erl", Code)
    catch
        Type:Error -> io:format("Error compiling logger (~p): ~p~n", [Type, Error])
    end.

set_exception(ModuleOrList, How) ->
    [rpc:call(Node, ?MODULE, set_exception2, [ModuleOrList, How]) || Node <- [node() | nodes()] ].

set_exception2(Module, How) when is_atom(Module) andalso is_boolean(How) ->
    set_exception2([Module], How);
set_exception2(ModuleList, How) when is_list(ModuleList) andalso is_boolean(How)->
    ExceptionList = merge_exceptions(ModuleList, How),
    LogLevel = ?EVER_LOGGER:get(),
    set2(LogLevel, ExceptionList);
set_exception2(_, _) ->
    io:format("Args not right, Arg1 = Mod,  Arg2 = Boolean").

merge_exceptions(ModuleList, true) ->
    CurrentExceptions = ?EVER_LOGGER:get_exceptions(),
    (CurrentExceptions -- ModuleList) ++ ModuleList;
merge_exceptions(ModuleList, false) ->
    CurrentExceptions = ?EVER_LOGGER:get_exceptions(),
    CurrentExceptions -- ModuleList.

level_to_integer(Level) ->
    case lists:keysearch(Level, 2, ?LOG_LEVELS) of
        {value, {Int, Level, _Desc}} -> Int;
        _ -> erlang:error({no_such_loglevel, Level})
    end.

%% --------------------------------------------------------------
%% Code of the mcs logger, dynamically compiled and loaded
%% This allows to dynamically change log level while keeping a
%% very efficient code.
logger_src(Loglevel, ExcModList) ->
    L = integer_to_list(Loglevel),
    ExcStr = lists:concat(lists:map(fun(E) -> "    
    debug_msg(" ++ atom_to_list(E) ++ ", Line, Format, Args)  ->
            notify(info_msg,
                   \"D(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [node(), self(), " ++ atom_to_list(E)  ++ ", Line]++Args);"
                                        end, ExcModList)),
    ExcModListSrc = lists:flatten(io_lib:format("~w", [ExcModList])),
    "-module(" ++ ?EVER_LOGGER ++ ").

    -export([test_msg/4,
             debug_msg/4,
             info_msg/4,
             warning_msg/4,
             error_msg/4,
             critical_msg/4,
             get_exceptions/0,
             get/0]).

    get() -> "++ L ++".
    get_exceptions() -> " ++ ExcModListSrc ++ ".

    %% Helper functions
    " ++ ExcStr ++ "

    debug_msg(Module, Line, Format, Args) when " ++ L ++ " >= 5 ->
            notify(info_msg,
                   \"D(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [node(), self(), Module, Line]++Args);
    debug_msg(_,_,_,_) -> ok.

    test_msg(Module, Line, Format, Args) when " ++ L ++ " >= 6 ->
            notify(info_msg,
                   \"T(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [node(), self(), Module, Line]++Args);
    test_msg(_,_,_,_) -> ok.

    info_msg(Module, Line, Format, Args) when " ++ L ++ " >= 4 ->
            notify(info_msg,
                   \"I(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [node(), self(), Module, Line]++Args);
    info_msg(_,_,_,_) -> ok.

    warning_msg(Module, Line, Format, Args) when " ++ L ++ " >= 3 ->
            notify(error,
                   \"W(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [node(), self(), Module, Line]++Args);
    warning_msg(_,_,_,_) -> ok.

    error_msg(Module, Line, Format, Args) when " ++ L ++ " >= 2 ->
        case Args of
            %% start with : ** Node php
            [42,42,32,78,111,100,101,32,112,104,112|_] ->
                ok;
            _ ->
                notify(error,
                   \"E(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [node(), self(), Module, Line]++Args)
        end;
    error_msg(_,_,_,_) -> ok.

    critical_msg(Module, Line, Format, Args) when " ++ L ++ " >= 1 ->
            notify(error,
                   \"C(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [node(), self(), Module, Line]++Args);
    critical_msg(_,_,_,_) -> ok.

    %% Distribute the message to the Erlang error logger
    notify(Type, Format, Args) ->
            LoggerMsg = {Type, group_leader(), {self(), Format, Args}},
            gen_event:notify(error_logger, LoggerMsg).
    ".
