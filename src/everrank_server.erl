
-module(everrank_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link([Name]) ->
    gen_server:start_link({global, Name}, ?MODULE, Args, []).

init(Args) ->
    case application:get_env(everrank, master) of
        {ok, true} ->
            %%TODO: start mnesia frag process
            ok;
        _ ->
            case application:get_env(everrank, master_ip) of
                {ok, Ip} ->
                    Master = list_to_atom(lists:concat([everrank_master@, Ip])),
                    case net_kernel:connect_node(Master) of
                        true ->
                            erlang:send_after(200, self(), {init, init_slave_db});
                        _ ->
                            erlang:send_after(3*1000, self(), {node, connect_master})
                    end
            end
    end,
    {ok, #state{}}.
    

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(Request, State) ->
    {noreply, State}.
    
handle_info(Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.

code_change(_, _, _) ->
    ok.


