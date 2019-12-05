### 游戏引擎

#### 目录
- [编译引擎](#编译引擎)
- [函数库](#函数库)
- [时间服务](#时间服务)

#### TODO
- [x] make engine after make_lib
- [x] time server
- [ ] log app
- [ ] node app
- [ ] tcp app
- [ ] db app
- [ ] base tool

#### 编译引擎
- 修改`emakefile`把引擎的匹配放在前面

#### 函数库
- `./dkge/dkge_misc/src/mlib_tool.erl`
- ip转换
- 数据类型转换
- MD5哈希
- 取整操作
- 随机数
- 列表操作：取值，去重，排序等

#### 时间服务
- `./dkge/dkge_time/src/mtime.erl & ./dkge/dkge_time/src/mtime_server.erl`
- 时间戳转换
- 订阅时间消息
- 时间输出格式转换
- 启动时间服务等