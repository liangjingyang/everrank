
-module(everrank_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("everrank.hrl").

-record(state, {}).

-define(ETS_EVERRANK, ets_everrank).
-define(TABLE_FRAG_SIZE, 10000).
-define(FRAGMENTIZE_TIME, 30*1000).

start_link(Name) ->
    gen_server:start_link({global, Name}, ?MODULE, [], []).

init([]) ->
    ets:new(?ETS_EVERRANK, [named_table, public, {read_concurrency, true}]),
    ets:insert(?ETS_EVERRANK, {update_follow_count, 0}),
    case application:get_env(everrank, master) of
        {ok, true} ->
            init_master_db(),
            erlang:send_after(?FRAGMENTIZE_TIME, self(), {frag, check_frag}),
            ok;
        _ ->
            case application:get_env(everrank, master_ip) of
                {ok, Ip} ->
                    Master = list_to_atom(lists:concat([everrank_master@, Ip])),
                    case net_kernel:connect_node(Master) of
                        true ->
                            erlang:send_after(200, self(), {init, init_slave_db});
                        _ ->
                            erlang:send_after(3*1000, self(), {node, connect_master, Master})
                    end
            end
    end,
    {ok, #state{}}.
    

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.
    
handle_info(Info, State) ->
    ?DO_HANDLE_INFO(Info, State),
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.

code_change(_, _, _) ->
    ok.


do_handle_info({init, init_slave_db}, _State) ->
    init_slave_db(),
    ok;
do_handle_info({frag, check_frag}, _State) ->
    check_frag(),
    ok;
do_handle_info({node, connect_master, Master}, _State) ->
    case net_kernel:connect_node(Master) of
        true ->
            erlang:send_after(200, self(), {init, init_slave_db});
        _ ->
            erlang:send_after(3*1000, self(), {node, connect_master, Master})
    end,
    ok;
do_handle_info(Info, State) ->
    ?ERROR_MSG("everrank_server recv unknow info: ~w, state: ~w", [Info, State]).

init_master_db() ->
    ever_db:create_schema([node()]),
    ever_db:start(),
    case application:get_env(everrank, platforms) of
        {ok, Platforms} when is_list(Platforms) ->
            TabList = lists:foldl(fun(P, Acc) ->
                        Tab = everrank_lib:sns_to_tab(P),
                        FDTab = everrank_lib:sns_to_friend_tab(P),
                        FWTab = everrank_lib:sns_to_follow_tab(P),
                        RNTab = everrank_lib:sns_to_relation_tab(P),
                        [
                            {Tab, [{record_name, t}, {attributes, record_info(fields, t)}]},
                            {FDTab, [{record_name, t_fd}, {attributes, record_info(fields, t_fd)}]},
                            {FWTab, [{record_name, t_fw}, {attributes, record_info(fields, t_fw)}]},
                            {RNTab, [{record_name, t_rn}, {attributes, record_info(fields, t_rn)}]}
                            | Acc
                        ]
                end, [], Platforms),
            lists:foreach(fun({T, TabDef}) ->
                        ever_db:create_table(T, [{disc_copies, [node()]}|TabDef])
                end, TabList);
        _ ->
            ignore
    end,
    ever_db:init().

init_slave_db() ->
    ever_db:start(),
    SchemaList = ets:tab2list(schema),
    TabList = lists:foldl(fun({schema, Tab, _List}, Acc) ->
                catch ever_db:add_table_copy(Tab, node(), ram_copies),
                [Tab|Acc]
        end, [], SchemaList),
    ever_db:wait_for_tables(TabList, 5 * 1000).
    
check_frag() ->
    erlang:send_after(?FRAGMENTIZE_TIME, self(), {frag, check_frag}),
    TabList = ever_db:system_info(tables),
    check_frag2(TabList).
check_frag2([Tab|List]) ->
    case ever_db:table_info(Tab, size) > ?TABLE_FRAG_SIZE of
        true ->
            ever_db:do_frag(Tab);
        false ->
            check_frag2(List)
    end;
check_frag2([]) ->
    ok.





