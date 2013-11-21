
-module(everrank_lib).

-compile(export_all).

-include("everrank.hrl").

sns_to_tab(SnsType) -> 
    sns_to_tab(SnsType, "").

sns_to_tab(SnsType, Suffix) when is_integer(SnsType) ->
    sns_to_tab(snstype_to_string(SnsType), Suffix);

sns_to_tab(SnsType, Suffix) ->
    list_to_atom(lists:concat([t_, SnsType, Suffix])).

sns_to_friend_tab(SnsType) ->
    sns_to_tab(SnsType, "_friend").
sns_to_follow_tab(SnsType) ->
    sns_to_tab(SnsType, "_follow").
sns_to_relation_tab(SnsType) ->
    sns_to_tab(SnsType, "_relation").

snstype_to_string(?SNSTYPE_SINAWEIBO) ->
    "sw";
snstype_to_string(?SNSTYPE_TENCENTWEIBO) ->
    "tw";
snstype_to_string(?SNSTYPE_RENREN) ->
    "rr".

    
