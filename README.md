# ever sns rank server #
为手游提供好友积分排行等社交交互服务。
虽然叫rank server但并没有去做排序的工作，而是通过社交平台开放的好友关系链，构建游戏的社交关系，在好友之间分享数据，例如积分。只要client能获得好友数据，好友间的排行就不是什么问题了。

## 协议概述 ##
- 一共5个协议
- 使用json格式
- 使用http请求, POST方法
- 支持加密

## 协议 ##
1."init"                    初始化一个用户
2."update_friend"           更新用户的好友，支持增减两种操作
3."set_data"                设置用户数据，可以是复杂结构
4."get_data"                获取用户自己的数据
5."get_friend_data"         获取好友（们）的数据

## 协议字段 ##
- "type"                    社交平台的类型，支持新浪微博，腾讯微博，人人网
- "id"                      社交平台的用户唯一id
- "fdl"                     好友唯一id的列表
- "data"                    用户数据
- "cmd"                     操作命令

## 社交平台 ##
- "sw"                      新浪微博
- "tw"                      腾讯微博
- "rr"                      人人网

## cmd ##
- "add"                     增加（update_friend协议里用到）
- "del"                     删除（update_friend协议里用到）
- "all"                     全部的好友数据（get_friend_data协议里用到）
- "new"                     更新的好友数据（get_friend_data协议里用到）

## 返回结果 ##
- "succ"
- "error system"
- "error method"
- "error body"
- "error content"
- "error protocol"
- "error field"
- "error type"
- "error json"
- 用户自己的数据
- 好友的数据

## 协议示例 ##
新浪微博为例

### init ###
请求: 
{
    "init" : 
    {
        "type" : "sw",
        "id" : 1001,
        "fdl" : [1002,1003]
    }
}
返回: "succ"

### update_friend ###

请求: 
{
    "update_friend :
    {
        "type" : "sw",
        "id" : 1001,
        "cmd" : "add",
        "fdl" : [1004, 1005]
    }
}
返回: "succ"


### set_data ###

请求:
{
    "set_data" :
    {
        "type" : "sw",
        "id" : 1001,
        "data" : data 
    }
}
返回: "succ"

### get_data ###

请求:
{
    "get_data" :
    {
        "type" : "sw",
        "id" :1001 
    }
}
返回: {"id":1001, "data":data}

### get_friend_userdata ###

请求: 
{
    "get_friend_data" :
    {
        "type" : "sw",
        "id" : 1001,
        "cmd": "all"
    }
}

返回: [{"id":1002, "data":data}, {"id":1003, "data":data}]

