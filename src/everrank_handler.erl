
-module(everrank_handler).

-export([
        init/3,
        handle/2,
        terminate/3
        ]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    case catch do_handle(Req, State) of
        {ok, Req2} ->
            ok;
        {error, Req2, Reason} ->
            do_handle_error(Req2, Reason);
        {error, Req2, Reason, Type} ->
            do_handle_error(Req2, Reason, Type)
    end,
    {ok, Req3, State}.

do_handle_error(Req, Reason) ->
    do_handle_error(Req, Reason, 400).
do_handle_error(Req, Reason, Type) ->
    cowboy_req:reply(Type, [], Reason, Req);

do_handle(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, Req2} ->
            do_handle_post(Req2, State);
        _ ->
            erlang:throw({error, Req, <<"HTTP Request must be POST.">>})
    end.

do_handle_post(Req, State) ->
    case cowboy_req:has_body(Req) of
        true ->
            do_handle_body(Req, State);
        false ->
            erlang:throw({error, Req, <<"Missing body.">>})
    end.

do_handle_body(Req, State) ->
    {ok, PostVals, Req2} = cowboy_req:has_body(Req),
    case proplists:get_value(<<"cmd">>, PostVals) of
        undefined ->
            erlang:throw({error, Req2, <<"What do you want?">>});
        Cmd ->
            do_handle_decrypt(Cmd)
    end.

do_handle_decrypt(Cmd) ->
    case 




    {Method, Req2} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req2),
    case HasBody of
        true ->
            {ok, PostVals, Req3} = cowboy_req:body_qs(Req2),
            case 
            case proplists:get_value(<<"cmd">>, PostVals) of
                undefined ->
                    do_handle_error(<<"what do you want?">>, Req3);
                _->
                    case catch do_handle(Method, Cmd, PostVals, Req3) of
                        {error, Reason, Req4} ->
                            do_handle_error(Reason, Req4);
                        {ok, Req4} ->
                            ok
                    end
            end;
        false ->
            {ok, Req4} = cowboy_req:reply(400, [], <<"Missing body.">>, Req2)
    end,
do_handle(<<"POST">>, <<"login">>, PostVals, Req) ->
    case proplists:get_value(<<"accname">>, PostVals) of
        undefined ->
            Name = undefined,
            erlang:throw({error, <<"What's your name?'">>});
        Name ->
            ok
    end,
    case proplists:get_value(<<"passwd">>, PostVals) of
        undefined ->
            Passwd = undefined,
            erlang:throw({error, <<"What's your password?'">>});
        Passwd ->
            ok
    end,
    case ever_db:dirty_read(?DB_ACCNAME, Name) of
        [#db_accname{}]
    
    {ok, Req2}.

do_handle(<<"POST">>, <<"get">>, Req) ->
    
    {ok, Req2}.

do_handle(<<"POST">>, <<"put">>, Req) ->
    
    {ok, Req2}.


do_handle(<<"POST">>, <<"register">>, Req) ->
    
    {ok, Req2}.

do_handle_error(Reason, Req) ->
    cowboy_req:reply(400, [], Reason, Req).

