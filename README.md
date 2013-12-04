# Ever Sns Rank Server #

### 概述 ##

为手游提供好友积分排行等社交交互服务和存档服务。单节点版本。      
虽然叫rank server但并没有去做排序的工作，而是通过社交平台开放的好友关系链，构建游戏的社交关系，在好友之间分享数据，例如积分。有了数据，客户端就可轻松排序。    

公开数据是指开放给好友访问的数据（例如分数）。  
私有数据是指只能玩家自己访问的数据（例如存档）。  

项目依赖：              
cowboy: https://github.com/extend/cowboy                        
jsx: https://github.com/talentdeficit/jsx                       

### 用法 ##

环境要求，erlang otp + linux                    
git clone https://github.com/liangjingyang/everrank.git                 
在everrank目录下运行./rebar get-deps && ./rebar compile进行编译         
进入script目录，运行./start.sh          

### 测试 ##
以sina微博为例          

初始化互为好友的两个用户：1001， 1002           

    curl -i -d content="{\"init\":{\"type\":\"sw\", \"id\":1001, \"fdl\":[1002]}}" http://localhost:8080
    curl -i -d content="{\"init\":{\"type\":\"sw\", \"id\":1002, \"fdl\":[1001]}}" http://localhost:8080

更新两个人的游戏分数：          

    curl -i -d content="{\"update_data\"type\":\"sw\", \"id\":1001, \"data"\:8888}}" http://localhost:8080
    curl -i -d content="{\"update_data\"type\":\"sw\", \"id\":1002, \"data"\:9999}}" http://localhost:8080

获取自己的分数：                

    curl -i -d content="{\"get_data\":{\"type\":\"sw\", \"id\":1001}}" http://localhost:8080

获取好友的分数：                

    curl -i -d content="{\"get_friend_data\":{\"type\":\"sw\", \"id\":1001}}" http://localhost:8080             


### 协议 ##
http协议，post方法，json格式。    

- "init"                    初始化一个用户
- "update_friend"           更新用户的好友，支持增减两种操作
- "set_data"                设置用户公开数据，可以是任意json结构
- "get_data"                获取用户自己的公开数据
- "get_friend_data"         获取好友（们）的公开数据
- "set_private_data"        设置自己的私有数据，可以是任意json结构
- "get_private_data"        获取自己的私有数据        

### 协议字段 ##
- "type"                    社交平台的类型，支持新浪微博，腾讯微博，人人网
- "id"                      社交平台的用户唯一id
- "fdl"                     好友唯一id的列表
- "data"                    用户数据
- "cmd"                     操作命令
- "time"                    秒为单位的时间戳

### 社交平台 ##
- "sw"                      新浪微博
- "tw"                      腾讯微博
- "rr"                      人人网

### cmd ##
- "add"                     增加（update_friend协议里用到）
- "del"                     删除（update_friend协议里用到）

### 返回结果 ##
- "succ"
- "error system"
- "error method"
- "error body"
- "error content"
- "error protocol"
- "error field"
- "error type"
- "error json"
- 用户自己的公开数据
- 用户自己的私有数据
- 好友的公开数据

### 协议示例 ##
新浪微博为例

#### init ###
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

#### update_friend ###
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

#### set_data ###
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

#### get_data ###
请求:        

    {    
        "get_data" :    
        {    
            "type" : "sw",    
            "id" :1001     
        }    
    }    

返回:       

    {
        "id":1001, 
        "data":data, 
        "time":1385088233
    }    

#### get_friend_userdata ###
请求:          

    {    
        "get_friend_data" :    
        {        
            "type" : "sw",    
            "id" : 1001,    
            "time" : 0    
        }    
    }    

返回:       

    [
        {
            "id":1002, 
            "data":data,
            "time":1385088233
        }, 
        {
            "id":1003, 
            "data":data, 
            "time":1385088233
        }
    ]    

#### set_private_data ###
请求:         

    {    
        "set_private_data" :    
        {    
            "type" : "sw",    
            "id" : 1001,    
            "data" : data    
        }    
    }    

返回: "succ"

#### get_private_data ###
请求:         

    {    
        "get_private_data" :    
        {    
            "type" : "sw",    
            "id" : 1001    
        }    
    }    

返回:       

    {
        "id":1001, 
        "data":data, 
        "time":1385088233
    }    

