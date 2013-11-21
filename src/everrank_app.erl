
-module(everrank_app).

-export([start/0, start/2, stop/1]).


start() ->
    application:start(crypto),
    application:start(cowlib),
    application:start(ranch),
    application:start(cowboy),
    application:start(everrank).

start(_Type, Args) ->
    Dispatch = cowboy_router:compile([
            {'_', [
                    {"/", everrank_handler, []}
                ]}
        ]),
    AcceptorNum = proplists:get_value(acceptor_num, Args, 100),
    Port = proplists:get_value(port, Args, 8080),
    {ok, _} = cowboy:start_http(http, AcceptorNum, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
    {ok, Sup} = everrank_sup:start_link(),
    case application:get_env(everrank, master) of
        {ok, true} ->
            _Res = supervisor:start_child(Sup, 
                {everrank_master, 
                    {everrank_server, start_link, [everrank_master]}, 
                    permanent, 2000, worker, [everrank_server]
                });
        _ ->
            case application:get_env(everrank, slave_id) of
                N when is_integer(N) and N > 0 ->
                    Name = list_to_atom(lists:concat([everrank_slave_, N])),
                    supervisor:start_child(Sup, 
                        {Name, 
                            {everrank_server, start_link, [Name]}, 
                            permanent, 2000, work, [everrank_server]
                        });
                _ ->
                    ignore
            end
    end,
    {ok, Sup}.


stop(_State) ->
    ok.
