%% LastChange: 2013-07-02 17:21:31
%% Copr. (c) 2013-2015, Simple <ljy0922@gmail.com>

-module(ever_log_server).
-behaviour(gen_server).
-export([
        start_link/0
    ]).

-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).

-export([
        reopen_log/0, 
        rotate_log/1
    ]).

-record(state, {fd, file}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    loglevel:start(),
    refresh_log_file(),
    {Fd, File} = make_log_file(),
    {ok, #state{fd = Fd, file = File}}.

reopen_log() ->
    error_logger ! {emulator, noproc, reopen}.

%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info({event, Event}, State) ->
    write_event(State#state.fd, {erlang:localtime(), Event}),
    {noreply, State};

handle_info({emulator, _GL, reopen}, State) ->
    file:close(State#state.fd),
    rotate_log(State#state.file),
    case file:open(State#state.file, [append, raw]) of
        {ok, Fd} ->
            {ok, State#state{fd = Fd}};
        Error ->
            Error
    end;

handle_info({emulator, GL, Chars}, State) ->
    write_event(State#state.fd, {erlang:localtime(), {emulator, GL, Chars}}),
    {ok, State};

handle_info(refresh_log_file, State) ->
    {Fd, File} = make_log_file(),
    refresh_log_file(),
    {noreply, State#state{fd=Fd, file=File}};

handle_info(Info, State) ->
    io:format("ever_log_server unknow info: ~w~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

% Copied from erlang_logger_file_h.erl
%% 正常断线的不要输出日志
write_event(_Fd, {_Time, {error, _GL, {_Pid, _Format, [_, {inet_async,_,_,{error,closed}}, _, _]}}}) ->
    ignore;
write_event(_Fd, {_Time, {error, _GL, {_Pid, _Format, [_, {stop,flash}, _, _]}}}) ->
    ignore;
write_event(_Fd, {_Time, {error, _GL, {_Pid, _Format, [_, {_,_,{error,closed,{error,ebadf}}}, _, _]}}}) ->
    ignore;
write_event(Fd, {Time, {error, _GL, {Pid, Format, Args}}}) ->
    T = write_time(Time),
    case catch io_lib:format(add_node(Format,Pid), Args) of
        S when is_list(S) ->
            file:write(Fd, io_lib:format(T ++ S, []));
        _ ->
            F = add_node("ERROR: ~p - ~p~n", Pid),
            file:write(Fd, io_lib:format(T ++ F, [Format,Args]))
    end;
write_event(Fd, {Time, {emulator, _GL, Chars}}) ->
    T = write_time(Time),
    case catch io_lib:format(Chars, []) of
        S when is_list(S) ->
            file:write(Fd, io_lib:format(T ++ S, []));
        _ ->
            file:write(Fd, io_lib:format(T ++ "ERROR: ~p ~n", [Chars]))
    end;
write_event(Fd, {Time, {info, _GL, {Pid, Info, _}}}) ->
    T = write_time(Time),
    file:write(Fd, io_lib:format(T ++ add_node("~p~n",Pid), [Info]));
write_event(Fd, {Time, {error_report, _GL, {Pid, std_error, Rep}}}) ->
    T = write_time(Time),
    S = format_report(Rep),
    file:write(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []));
write_event(Fd, {Time, {info_report, _GL, {Pid, std_info, Rep}}}) ->
    T = write_time(Time, "INFO REPORT"),
    S = format_report(Rep),
    file:write(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []));
write_event(Fd, {Time, {info_msg, _GL, {Pid, Format, Args}}}) ->
    T = write_time(Time, "INFO REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
        S when is_list(S) ->
            file:write(Fd, io_lib:format(T ++ S, []));
        _ ->
            F = add_node("ERROR: ~p - ~p~n", Pid),
            file:write(Fd, io_lib:format(T ++ F, [Format,Args]))
    end;
write_event(_, _) ->
    ok.

format_report(Rep) when is_list(Rep) ->
    case string_p(Rep) of
        true ->
            io_lib:format("~s~n",[Rep]);
        _ ->
            format_rep(Rep)
    end;
format_report(Rep) ->
    io_lib:format("~p~n",[Rep]).

format_rep([{Tag,Data}|Rep]) ->
    io_lib:format("    ~p: ~p~n",[Tag,Data]) ++ format_rep(Rep);
format_rep([Other|Rep]) ->
    io_lib:format("    ~p~n",[Other]) ++ format_rep(Rep);
format_rep(_) ->
    [].

%add_node(X, Pid) when is_atom(X) ->
%add_node(atom_to_list(X), Pid);
%add_node(X, Pid) when is_pid(Pid) ->
%lists:concat([X,"node ",node(Pid)," "]);
add_node(X, _Pid) ->
    X.

string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
    case string_p1(H) of
        true -> string_p1(T);
        _    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.

write_time(Time) ->
    write_time(Time, "").

write_time({{Y,Mo,D},{H,Mi,S}}, Type) ->
    io_lib:format("~n~s=== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ===",
        [Type, Y, Mo, D, H, Mi, S]).

%% @doc Rename the log file if exists, to "*-old.log".
%% This is needed in systems when the file must be closed before rotation (Windows).
%% On most Unix-like system, the file can be renamed from the command line and
%% the log can directly be reopened.
%% @spec (Filename::string()) -> ok
rotate_log(Filename) ->
    case file:read_file_info(Filename) of
        {ok, _FileInfo} ->
            RotationName = filename:rootname(Filename),
            file:rename(Filename, [RotationName, "-old.log"]),
            ok;
        {error, _Reason} ->
            ok
    end.

%%通知服务器在下一个整点刷新日志文件
refresh_log_file() ->
    {_, {H, M, S}} = erlang:localtime(),
    Time = ((23 - H) * 3600 + (59 - M) * 60 + (59 - S) + 2) * 1000,
    erlang:send_after(Time, self(), refresh_log_file).            

%%生成日志文件名
make_log_file() ->
    {{Year, Month, Day}, {_Hour, _, _}} = erlang:localtime(),
    File = io_lib:format(get_root_path() ++ "logs/ever_~p_~p_~p.log", [Year, Month, Day]),
    case file:open(File, [append, raw]) of
        {ok, Fd} ->
            {Fd, File};
        Error ->
            {Error, File} 
    end.

get_root_path() ->
    {ok, [[Root]]} = init:get_argument(root_path),
    Root.
