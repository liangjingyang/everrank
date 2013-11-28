-include("src/everlib/include/ever_log.hrl").


-record(t, {snsId, privateData = <<"">>, privateTime = 0, data = <<"">>, time = 0}).
-record(t_fd, {snsId, friendList = []}).
-record(t_fdl, {snsId, data = <<>>, time = 0}).
-record(t_fw, {snsId, followList = []}).
-record(t_rn, {snsId, relationList = []}).

-define(PROTOCOL_INIT, <<"init">>).
-define(PROTOCOL_UPDATE_FRIEND, <<"update_friend">>).
-define(PROTOCOL_SET_USERDATA, <<"update_data">>).
-define(PROTOCOL_GET_USERDATA, <<"get_data">>).
-define(PROTOCOL_GET_FRIEND_USERDATA, <<"get_friend_data">>).
-define(PROTOCOL_SET_PRIVATE_USERDATA, <<"set_private_data">>).
-define(PROTOCOL_GET_PRIVATE_USERDATA, <<"get_private_data">>).

-define(FIELD_SNSTYPE, <<"type">>).
-define(FIELD_SNSID, <<"id">>).
-define(FIELD_FRIENDLIST, <<"fdl">>).
-define(FIELD_USERDATA, <<"data">>).
-define(FIELD_CMD, <<"cmd">>).
-define(FIELD_RES, <<"res">>).
-define(FIELD_TIME, <<"time">>).

-define(SNSTYPE_SINAWEIBO, <<"sw">>).
-define(SNSTYPE_TENCENTWEIBO, <<"tw">>).
-define(SNSTYPE_RENREN, <<"rr">>).
-define(SNSTYPE_LIST, [?SNSTYPE_SINAWEIBO, ?SNSTYPE_TENCENTWEIBO, ?SNSTYPE_RENREN]).

-define(CMD_ADD, <<"add">>).
-define(CMD_DEL, <<"del">>).
-define(CMD_LIST_UPDATE_FRIEND, [?CMD_ADD, ?CMD_DEL]).

-define(RES_SUCC, <<"succ">>).
-define(RES_ERROR_SYSTEM, <<"error system">>).
-define(RES_ERROR_METHOD, <<"error method">>).
-define(RES_ERROR_BODY, <<"error body">>).
-define(RES_ERROR_CONTENT, <<"error content">>).
-define(RES_ERROR_PROTOCOL, <<"error protocol">>).
-define(RES_ERROR_FIELD, <<"error field">>).
-define(RES_ERROR_SNSTYPE, <<"error type">>).
-define(RES_ERROR_JSON, <<"error json">>).
-define(RES_ERROR_ALREADY_INITED, <<"error already inited">>).
-define(RES_ERROR_NOT_INITED, <<"error not inited">>).

