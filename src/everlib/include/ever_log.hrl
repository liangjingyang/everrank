%% log define
%  Print in standard output
-define(PRINT(Format),
    io:format(Format++"~n", [])).
-define(PRINT(Format, Args),
    io:format(Format++"~n", Args)).
-define(TEST_MSG(Format),
    logger:test_msg(?MODULE,?LINE,Format, [])).
-define(TEST_MSG(Format, Args),
    logger:test_msg(?MODULE,?LINE,Format, Args)).
-define(DEBUG(Format),
    logger:debug_msg(?MODULE,?LINE,Format, [])).
-define(DEBUG(Format, Args),
    logger:debug_msg(?MODULE,?LINE,Format, Args)).
-define(INFO_MSG(Format),
    logger:info_msg(?MODULE,?LINE,Format, [])).
-define(INFO_MSG(Format, Args),
    logger:info_msg(?MODULE,?LINE,Format, Args)).
-define(WARNING_MSG(Format),
    logger:warning_msg(?MODULE,?LINE,Format, [])).
-define(WARNING_MSG(Format, Args),
    logger:warning_msg(?MODULE,?LINE,Format, Args)).
-define(ERROR_MSG(Format),
    logger:error_msg(?MODULE,?LINE,Format, [])).
-define(ERROR_MSG(Format, Args),
    logger:error_msg(?MODULE,?LINE,Format, Args)).
-define(CRITICAL_MSG(Format),
    logger:critical_msg(?MODULE,?LINE,Format, [])).
-define(CRITICAL_MSG(Format, Args),
    logger:critical_msg(?MODULE,?LINE,Format, Args)).
-define(TRY_CATCH(Expression,ErrReason), 
        try 
            Expression
        catch 
            _:ErrReason -> 
                ?ERROR_MSG("Reason: ~p~nStacktrace: ~p", [ErrReason,erlang:get_stacktrace()]) 
        end).
-define(TRY_CATCH(Expression), ?TRY_CATCH(Expression,ErrReason)).

-define(DO_HANDLE_INFO(Info,State),  
        try 
            do_handle_info(Info, State) 
        catch _:Reason -> 
                ?ERROR_MSG("Info: ~w~n,State: ~w~nReason: ~w~nstrace: ~p", [Info,State, Reason, erlang:get_stacktrace()]),
                {noreply, State}
        end).

-define(DO_HANDLE_CAST(Info,State),  
        try 
            do_handle_cast(Info, State) 
        catch _:Reason -> 
                ?ERROR_MSG("CastInfo: ~w~nState: ~w~nReason: ~w~nstrace: ~p", [Info,State, Reason, erlang:get_stacktrace()]),
                {noreply, State}
        end).

-define(DO_HANDLE_CALL(Request, From, State),  
        try 
            do_handle_call(Request, From, State) 
        catch _:Reason -> 
                ?ERROR_MSG("Request: ~w~nState: ~w~nReason: ~w~nstrace: ~p", [Request,State, Reason, erlang:get_stacktrace()]),
                {reply, error, State}
        end).

