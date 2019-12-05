#### 协议

#### 目录
- [协议规范](#协议规范)
- [生成协议号](#生成协议号)
- [生成其他映射文件](#生成其他映射文件)
- [生成protobuf文件](#生成protobuf文件)
- [使用gpb编译protobuf](#使用gpb编译protobuf)

#### TODO
- [ ] 分开多个protobuf文件
- [ ] 处理协议编码解码逻辑

#### 协议规范
- 文件`./proto/game_proto.txt`
- 格式：
```
proto_list：
#{
name => Name,
router => gateway | {role, Mod},
note => Note
fields => [{Field, DataType, Note}],
err_code => [{Code, Note}],
ack => 0 | 1
},
public_error_code : [{Code, Note, Type}]
```

#### 生成协议号
- 根据`game_proto.txt`的`proto_list`生成`proto_id.txt`该文件只增不减只加不删

#### 生成其他映射文件
- `gateway_proto_router.erl`用于标记协议路由的模块，`gateway`路由到`gateway_tcp_client, {role, Mod}`路由到角色进程的模块
- `gateway_proto_id.erl`用于映射协议的协议号和反映射协议号与协议用于解码
- `gateway_proto_ack.erl`用于标识协议是否需要ack
- `./include/all_pb.hrl`用于include所有的{router}.hrl文件
- `./include/{router}.hrl` 根据router生成的协议结构头文件包含协议字段和错误码用于方便编码
- `./include/common_pb.hrl `router为gateway的协议字段、
- `./include/all_error_no.hrl ` 所有的错误码

#### 生成protobuf文件
- 数据类型映射
```
get_protobuf_type(int) -> "int32";
get_protobuf_type(long) -> "int64";
get_protobuf_type(string) -> "bytes";
get_protobuf_type(bool) -> "bool";
get_protobuf_type([int]) -> "repeated int32";
get_protobuf_type([long]) -> "repeated int64";
get_protobuf_type([string]) -> "repeated bytes";
get_protobuf_type(binary) -> "bytes";
get_protobuf_type([P]) -> lists:concat(["repeated ", P]);
get_protobuf_type(P) -> P.
```

#### 使用gpb编译protobuf
- 进入`gpb`目录`make doc`生成`gpb_scan.erl`和`gpb_parse.erl`
- 修改`gpb_complie`的`get_priv_dir`函数，使之可以获取到gpb下的priv目录
- 修改`gpb_gen_translation`的生成`id`、`v_ok`等描述，删除`compile inline`的代码，干掉编译的`old inliner`
- 使用easy_make编译`gpb`的源码
- 复制`gpb/bin/protoc-erl`的脚本到`script/gen_proto.es`
- 修改`gen_proto.es`的`gen_gpb`函数，获取`./proto`下的所有`.proto`文件去编译
- 运行`gen_proto.es`
- 生成`./src/proto/game_proto.erl`和`./include/proto/game_proto.hrl`

```bash
./proto/game_proto.proto # protobuf 文件
./proto/proto_id.txt # 协议id号（只增不减）
./include/{router}.hrl # 根据router生成的协议结构头文件
./include/all_error_no.hrl # 所有的错误码
./include/all_pb.hrl # include所有的{router}.hrl文件
./include/common_pb.hrl # 公有的协议字段
./src/proto/gateway_proto_router.erl # 用于代码路由模块
./src/proto/gateway_proto_ack.erl # 
./src/proto/gateway_proto_id.erl # 协议和协议号映射
```
- `gpb`编译出来的`game_proto.erl`文件太大，后续考虑弄成分文件的形式。