%% LastChange: 2013-07-02 17:37:45
%% Copr. (c) 2013-2015, Simple <ljy0922@gmail.com>

-module(ever_log_event).
-behaviour(gen_event).
-export([
        init/1, 
        handle_event/2,
        handle_call/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).

-define(EVER_LOG_SERVER, ever_log_server).

init(_) ->
    {ok, none}.

handle_event(Event, State) ->
    case erlang:whereis(?EVER_LOG_SERVER) of
        undefined ->
            ignore;
        Pid ->
            Pid ! {event, Event}
    end,
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info({'EXIT', _Fd, _Reason}, _State) ->
    remove_handler;

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

