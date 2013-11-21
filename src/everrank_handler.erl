
-module(everrank_handler).

-export([
        init/3,
        handle/2,
        terminate/3
        ]).

-include("everrank.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    case catch do_handle(Req, State) of
        {ok, Req2} ->
            ok;
        {error, Req2, Reason} ->
            do_handle_error(Req2, Reason);
        {error, Req2, Reason, Type} ->
            do_handle_error(Req2, Reason, Type);
        _Else ->
            Req2 = Req,
            ignore
    end,
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

reply(Content, Req) ->
    cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], Content, Req).

abort(Req, Reason) ->
    erlang:throw({error, Req, Reason}).

do_handle_error(Req, Reason) ->
    do_handle_error(Req, Reason, 400).
do_handle_error(Req, Reason, Type) ->
    cowboy_req:reply(Type, [], Reason, Req).

do_handle(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, Req2} ->
            do_handle_post(Req2, State);
        _ ->
            abort(Req, ?RES_ERROR_METHOD)
    end.

do_handle_post(Req, State) ->
    case cowboy_req:has_body(Req) of
        true ->
            do_handle_body(Req, State);
        false ->
            abort(Req, <<?RES_ERROR_BODY>>)
    end.

do_handle_body(Req, State) ->
    {ok, PostVals, Req2} = cowboy_req:has_body(Req),
    case proplists:get_value(<<"content">>, PostVals) of
        undefined ->
            abort(Req2, ?RES_ERROR_CONTENT);
        Data ->
            Data2 = do_handle_decrypt(Data),
            Data3 = do_handle_decode(Data2),
            do_handle_protocol(Data3, Req2, State)
    end.

do_handle_decrypt(Data) ->
    Data.

do_handle_decode(Data) ->
    jsx:decode(Data).

do_handle_protocol([?PROTOCOL_INIT, Data], Req, State) ->
    do_handle_init(Data, Req, State);
do_handle_protocol([?PROTOCOL_UPDATE_FRIEND, Data], Req, State) ->
    do_handle_update_friend(Data, Req, State);
do_handle_protocol([?PROTOCOL_UPDATE_USERDATA, Data], Req, State) ->
    do_handle_update_userdata(Data, Req, State);
do_handle_protocol([?PROTOCOL_GET_FRIEND_USERDATA, Data], Req, State) ->
    do_handle_get_friend_userdata(Data, Req, State);
do_handle_protocol(_, Req, _State) ->
    abort(Req, ?RES_ERROR_PROTOCOL).

do_handle_init(Data, Req, _State) ->
    [SnsType, SnsId, FSnsIdList] = check_init_field(Data, Req),
    Tab = everrank_lib:sns_to_tab(SnsType),
    FDTab = everrank_lib:sns_to_friend_tab(SnsType),
    FWTab = everrank_lib:sns_to_follow_tab(SnsType),
    RNTab = everrank_lib:sns_to_relation_tab(SnsType),
    case ever_db:dirty_read(Tab, SnsId) of
        [] ->
            ever_db:dirty_write(Tab, #t{snsId = SnsId}),
            ever_db:dirty_write(FDTab, #t_fd{snsId = SnsId}),
            ever_db:dirty_write(FWTab, #t_fw{snsId = SnsId});
        _ ->
            ignore
    end,
    [Inited, NotInited] = split_friends(FSnsIdList, Tab, [], []),
    add_inited(Inited, SnsId, Tab, FDTab, FWTab),
    add_notinited(NotInited, SnsId, RNTab),
    add_relation(SnsId, FDTab, FWTab, RNTab),
    reply(?RES_SUCC, Req).

    
check_init_field(Data, Req) ->
    SnsType = check_field_snstype(Data, Req),
    SnsId = check_field_snsid(Data, Req),
    FSnsIdList = check_field_friendlist(Data, Req),
    [SnsType, SnsId, FSnsIdList].

do_handle_update_friend(Data, Req, _State) ->
    [SnsType, SnsId, Cmd, FSnsIdList] = check_update_friend_field(Data, Req),
    Tab = everrank_lib:sns_to_tab(SnsType),
    FDTab = everrank_lib:sns_to_friend_tab(SnsType),
    FWTab = everrank_lib:sns_to_follow_tab(SnsType),
    RNTab = everrank_lib:sns_to_relation_tab(SnsType),
    case Cmd of
        ?CMD_ADD ->
            [Inited, NotInited] = split_friends(FSnsIdList, Tab, [], []),
            add_inited(Inited, SnsId, Tab, FDTab, FWTab),
            add_notinited(NotInited, SnsId, RNTab),
            add_relation(SnsId, FDTab, FWTab, RNTab),
            reply(?RES_SUCC, Req);
        ?CMD_DEL ->
            del_fd(FSnsIdList, SnsId, FDTab),
            del_rn_and_fw(FSnsIdList, SnsId, RNTab, FWTab),
            reply(?RES_SUCC, Req)
    end.

check_update_friend_field(Data, Req) ->
    SnsType = check_field_snstype(Data, Req),
    SnsId = check_field_snsid(Data, Req),
    Cmd = check_field_cmd(Data, Req, ?CMD_LIST_UPDATE_FRIEND),
    FSnsIdList = check_field_friendlist(Data, Req),
    [SnsType, SnsId, Cmd, FSnsIdList].

do_handle_update_userdata(Data, Req, _State) ->
    [SnsType, SnsId, UserData] = check_update_userdata(Data, Req),
    Tab = everrank_lib:sns_to_tab(SnsType),
    case ever_db:dirty_read(Tab, SnsId) of
        [] ->
            ignore;
        [#t{data = OldUserData} = Rec] ->
            case is_replace_userdata(OldUserData, UserData) of
                false ->
                    ignore;
                true ->
                    ever_db:dirty_write(Tab, Rec#t{data = UserData}),
                    spawn(fun() -> update_follow(UserData, SnsId, SnsType) end),
                    reply(?RES_SUCC, Req)
            end
    end.

check_update_userdata(Data, Req) ->
    SnsType = check_field_snstype(Data, Req),
    SnsId = check_field_snsid(Data, Req),
    UserData = check_field_userdata(Data, Req),
    [SnsType, SnsId, UserData].

do_handle_get_friend_userdata(Data, Req, _State) ->
    [SnsType, SnsId, Cmd] = check_get_friend_userdata(Data, Req),
    FDTab = everrank_lib:sns_to_friend_tab(SnsType),
    case ever_db:dirty_read(FDTab, SnsId) of
        [] ->
            ignore;
        [#t_fd{friendList = FDList} = FDRec] ->
            case Cmd of
                ?CMD_ALL ->
                    Res = fdl_to_json(FDList);
                ?CMD_NEW ->
                    Res = fdl_to_json([FDL||FDL<-FDList, FDL#t_fdl.update =:= true])
            end,
            FDList2 = lists:foldl(fun(FDL, Acc) -> 
                        case FDL#t_fdl.update of
                            true ->
                                [FDL#t_fdl{update = false}|Acc];
                            false ->
                                [FDL|Acc]
                        end
                end, [], FDList),
            ever_db:dirty_write(FDTab, FDRec#t_fd{friendList = FDList2}),
            reply(Res, Req)
    end.

check_get_friend_userdata(Data, Req) ->
    SnsType = check_field_snstype(Data, Req),
    SnsId = check_field_snsid(Data, Req),
    Cmd = check_field_cmd(Data, Req, ?CMD_LIST_GET_FRIEND_DATA),
    [SnsType, SnsId, Cmd].

%%===================================================================
is_replace_userdata(_Old, _New) ->
    true.

fdl_to_json(FDList) ->
    TermList = fdl_to_json2(FDList, []),
    jsx:encode(TermList).
fdl_to_json2([FDL|List], TermList) ->
    #t_fdl{snsId = SnsId, data = UserData} = FDL,
    fdl_to_json2(List, [{SnsId, UserData}|TermList]);
fdl_to_json2([], TermList) ->
    TermList.

update_follow(UserData, SnsId, SnsType) ->
    FWTab = everrank_lib:sns_to_follow_tab(SnsType),
    case ever_db:dirty_read(FWTab, SnsId) of
        [] ->
            ignore;
        [#t_fw{followList = FSnsIdList}] ->
            FDTab = everrank_lib:sns_to_friend_tab(SnsType),
            FDLRec = #t_fdl{snsId = SnsId, data = UserData, update = true},
            update_follow2(FSnsIdList, FDLRec, SnsId, FDTab)
    end.
update_follow2([FSnsId|List], FDLRec, MSnsId, FDTab) ->
    %%TODO:transaction
    case ever_db:dirty_read(FDTab, FSnsId) of
        [] ->
            ignore;
        [#t_fd{friendList = FDList} = FDRec] ->
            FDList2 = lists:keystore(MSnsId, #t_fd.snsId, FDList, FDLRec),
            ever_db:dirty_write(FDTab, FDRec#t_fd{friendList = FDList2})
    end,
    update_follow2(List, FDLRec, MSnsId, FDTab);
update_follow2([],  _FDLRec, _MSnsId, _FDTab) ->
    ok.

del_rn_and_fw([FSnsId|List], SnsId, RNTab, FWTab) ->
    %%TODO:transaction
    case ever_db:dirty_read(RNTab, FSnsId) of
        [] ->
            ignore;
        [#t_rn{relationList = RNList} = RNRec] ->
            RNList2 = lists:delete(SnsId, RNList),
            ever_db:dirty_write(RNTab, RNRec#t_rn{relationList = RNList2})
    end,
    case ever_db:dirty_read(FWTab, FSnsId) of
        [] ->
            ignore;
        [#t_fw{followList = FWList} = FWRec] ->
            FWList2 = lists:delete(SnsId, FWList),
            ever_db:dirty_write(FWTab, FWRec#t_fw{followList = FWList2})
    end,
    del_rn_and_fw(List, SnsId, RNTab, FWTab);
del_rn_and_fw([], _SnsId, _RNTab, _FWTab) ->
    ok.

del_fd(FSnsIdList, SnsId, FDTab) ->
    case ever_db:dirty_read(FDTab, SnsId) of
        [] ->
            ignore;
        [#t_fd{friendList = FDList} = FDRec] ->
            FDList2 = del_fd2(FSnsIdList, FDList),
            ever_db:dirty_write(FDTab, FDRec#t_fd{friendList = FDList2})
    end.
del_fd2([FSnsId|List], FDList) ->
    FDList2 = lists:keydelete(FSnsId, #t_fdl.snsId, FDList),
    del_fd2(List, FDList2);
del_fd2([], FDList) ->
    FDList.


remove_dup_fd(FDList, SnsId, FDTab)->
    case ever_db:dirty_read(FDTab, SnsId) of
        [] ->
            FDList;
        [#t_fd{friendList = FDList2}] ->
            remove_dup_fd2(FDList, FDList2, [])
    end.
remove_dup_fd2([FSnsId|FDList], FDList2, FDList3) ->
    case lists:keymember(FSnsId, #t_fdl.snsId, FDList2) of
        true ->
            remove_dup_fd2(FDList, FDList2, FDList3);
        false ->
            remove_dup_fd2(FDList, FDList2, [FSnsId|FDList3])
    end;
remove_dup_fd2([], _FDList2, FDList3) ->
    FDList3.
            
merge_snsid([SnsId|T], List) ->
    case lists:member(SnsId, List) of
        true ->
            merge_snsid(T, List);
        false ->
            merge_snsid(T, [SnsId|List])
    end;
merge_snsid([], List) ->
    List.

add_relation(SnsId, FDTab, FWTab, RNTab) ->
    case ever_db:dirty_read(RNTab, SnsId) of
        [] ->
            ignore;
        [#t_rn{relationList = RelationList} = RNRec] ->
            add_relation2(RelationList, SnsId, FDTab),
            case ever_db:dirty_read(FWTab, SnsId) of
                [] ->
                    ever_db:dirty_write(FWTab, #t_fw{snsId = SnsId, followList = RelationList});
                [#t_fw{followList = FollowList} = FWRec] ->
                    FollowList2 = merge_snsid(RelationList, FollowList),
                    ever_db:dirty_write(FWTab, FWRec#t_fw{followList = FollowList2})
            end,
            ever_db:dirty_delete(RNTab, SnsId)
    end,
    ok.
add_relation2([RSnsId|RelationList], MSnsId, FDTab) ->
    %%TODO:transaction
    case ever_db:dirty_read(FDTab, RSnsId) of
        [] ->
            ignore;
        [#t_fd{friendList = FriendList} = FDRec] ->
            case lists:keymember(MSnsId, #t_fdl.snsId, FriendList) of
                true ->
                    ignore;
                false ->
                    Friend = #t_fdl{snsId = MSnsId, update = true},
                    FDRec2 = FDRec#t_fd{friendList = [Friend|FriendList]},
                    ever_db:dirty_write(FDTab, FDRec2)
            end
    end,
    add_relation2(RelationList, MSnsId, FDTab);
add_relation2([], _MSnsId, _FDTab) ->
    ok.

add_notinited([FSnsId|NotInited], MSnsId, RNTab) ->
    %%TODO:transaction
    case ever_db:dirty_read(RNTab, FSnsId) of
        [] ->
            ever_db:dirty_write(RNTab, #t_rn{snsId = FSnsId, relationList = [MSnsId]});
        [#t_rn{relationList = RelationList} = RNRec] ->
            RNRec2 = RNRec#t_rn{relationList = [MSnsId|lists:delete(MSnsId, RelationList)]},
            ever_db:dirty_write(RNTab, RNRec2)
    end,
    add_notinited(NotInited, MSnsId, RNTab);
add_notinited([], _MSnsId, _RNTab) ->
    ok.

merge_fdl([H|T], List) ->
    case lists:keymember(H#t_fdl.snsId, #t_fdl.snsId, List) of
        true ->
            merge_fdl(T, List);
        false ->
            merge_fdl(T, [H|List])
    end;
merge_fdl([], List) ->
    List.

add_inited(Inited, SnsId, Tab, FDTab, FWTab) ->
    FriendList = add_inited2(Inited, SnsId, Tab, FWTab, []),
    [#t_fd{friendList = FriendList2} = FDRec] = ever_db:dirty_read(FDTab, SnsId),
    FriendList3 = merge_fdl(FriendList2, FriendList),
    ever_db:dirty_write(FDTab, FDRec#t_fd{friendList = FriendList3}),
    ok.

add_inited2([FSnsId|Inited], MSnsId, Tab, FWTab, FriendList) ->
    case ever_db:dirty_read(Tab, FSnsId) of
        #t{data = Data} ->
            Friend = #t_fdl{snsId = FSnsId, data = Data, update = true},
            FriendList2 = [Friend|FriendList];
        _ ->
            FriendList2 = FriendList
    end,
    %%TODO:transaction
    case ever_db:dirty_read(FWTab, FSnsId) of
        #t_fw{followList = FollowList} = FWRec ->
            FollowList2 = [MSnsId|lists:delete(MSnsId, FollowList)],
            ever_db:dirty_write(FWTab, FWRec#t_fw{followList = FollowList2});
        _ ->
            ignore
    end,
    add_inited2(Inited, MSnsId, Tab, FWTab, FriendList2);
add_inited2([], _MSnsId, _Tab, _FWTab, FriendList) ->
    FriendList.

split_friends([SnsId|Friends], Tab, Inited, NotInited) ->
    case ever_db:dirty_read(Tab, SnsId) of
        [] ->
            split_friends(Friends, Tab, Inited, [SnsId|NotInited]);
        _ ->
            split_friends(Friends, Tab, [SnsId|Inited], NotInited)
    end;
split_friends([], _Tab, Inited, NotInited) ->
    [Inited, NotInited].

check_field_userdata(Data, Req) ->
    case proplists:get_value(Data, ?FIELD_USERDATA) of
        undefined ->
            UserData = undefined,
            abort(Req, ?RES_ERROR_FIELD);
        UserData ->
            ok
    end,
    UserData.
check_field_cmd(Data, Req, CmdList) ->
    case proplists:get_value(Data, ?FIELD_CMD) of
        undefined ->
            Cmd = undeifned,
            abort(Req, ?RES_ERROR_FIELD);
        Cmd ->
            case lists:member(Cmd, CmdList) of
                true ->
                    ok;
                false ->
                    abort(Req, ?RES_ERROR_SNSTYPE)
            end
    end,
    Cmd.
check_field_friendlist(Data, Req) ->
    case proplists:get_value(Data, ?FIELD_FRIENDLIST) of
        FriendList when is_list(FriendList) ->
            ok;
        undefined ->
            FriendList = [],
            abort(Req, ?RES_ERROR_FIELD)
    end,
    FriendList.
check_field_snsid(Data, Req) ->
    case proplists:get_value(Data, ?FIELD_SNSID) of
        undefined ->
            SnsId = undeifned,
            abort(Req, ?RES_ERROR_FIELD);
        SnsId ->
            ok
    end,
    SnsId.
check_field_snstype(Data, Req) ->
    case proplists:get_value(Data, ?FIELD_SNSTYPE) of
        undefined ->
            SnsType = undeifned,
            abort(Req, ?RES_ERROR_FIELD);
        SnsType ->
            case lists:member(SnsType, ?SNSTYPE_LIST) of
                true ->
                    ok;
                false ->
                    abort(Req, ?RES_ERROR_SNSTYPE)
            end
    end,
    SnsType.
