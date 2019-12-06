%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author QingliangCn <>
%%% @copyright (C) 2010, QingliangCn
%%% @doc
%%%
%%% @end
%%% Created : 27 Jun 2010 by QingliangCn <>
%%%-------------------------------------------------------------------
-module(common_config).

-include("global.hrl").
-include("mnode.hrl").

%% 必须提供的函数列表 请勿修改
-export([
    get_basic_configs/0,        %% 基础配置列表
    get_selfload_configs/0,     %% 自加载配置列表
    get_activity_configs/0,     %% 活动配置列表
    reload/1,                   %% 重新加载配置
    is_debug/0,                 %% 是否测试环境
    is_allow_gm_cmd/0,          %% 是否允许GM命令
    get_allow_platforms/1,
    is_word_filter/0,           %% 是否过滤敏感字符
    is_robot_move/0,            %% 机器人移动不检测
    get_version/0,              %% 获取版本号
    get_game_version/0,         %% 获取大版本号
    get_open_day/0,             %% 获取开服日期
    get_opened_days/0,          %% 获取已开服天数
    get_open_time/0,            %% 获取开服时间
    get_agent_code/0,           %% 获取代理商代号 例如:tx
    get_agent_id/0,             %% 获取代理商id
    get_game_agent_id/0,        %% 获取游戏服代理商id
    get_server_id/0,            %% 获取游戏服id
    get_server_ip/0,
    get_game_name/0,            %% 获取游戏名
    get_game_code/0,            %% 获取游戏代号
    get_game_id/0,              %% 获取游戏项目编号
    get_cookie_extend/0,        %% 获取cookie后缀
    get_fcm_validation_key/0,   %% 获取防沉迷验证秘钥
    get_fcm_validation_url/0,   %% 获取防沉迷验证链接
    get_log_level/0,            %% 获取日志等级
    get_log_dir/0,              %% 获取日志路径
    merged_times/0,             %% 获取合服次数
    is_merge/0,                 %% 是否已经合服
    get_dge_root/0,             %% 获取游戏运行代码目录
    get_mnesia_dir/0,           %% 获取mnesia数据库存储路径
    get_max_online/0,           %% 获取最高在线值
    get_queue_num/0,            %% 获取排队人数
    is_lite/0,                  %% 是否轻量级模式,即仅启动db
    is_lite_server_mode/0,      %% 是否启用内测机的轻进程池模式
    is_no_db/0,                 %% 不启动mnesia
    is_no_mysql/0,              %% 不连接mysql
    get_server_type/0,          %% 获取本节点server_type编号
    get_server_type_code/0,     %% 获取本节点server_type
    get_server_type_code/1,     %% 获取本节点server_type
    get_lang/0,
    get_apps/1,                 %% 根据server_type获取启动app列表
    get_apps/0,                 %% 获取当前游戏节点启动app列表
    get_apps_lite/0,            %% 获取lite模式启动app列表
%%     get_topo_nodes/1,           %% 根据server_type获取组信息
%%     get_topo_nodes/0,           %% 获取当前游戏节点组信息
    get_topo_mode/0,            %% 获取拓扑模式
    get_web_port/0,             %% 获取mochiweb端口
    get_mcweb_reg_ip/0,         %% 获取注册到mcweb的IP
    get_gateway_port/0,         %% 获取本节点网关端口
    get_gateway_ip/0,           %% 获取本节点网关IP,
    get_gateway_ip_port/0,      %% 获取本节点网关IP
    is_remove/0,                %% 获取本节点是否要移除，
    get_bgp_ip_port/0,
    login_ip_whitelist/0,       %% 登录ip白名单
%%    is_ip_whitelist/1,          %% 是否白名单
    login_ip_limit_num/0,       %% 同ip登录限制人数
    login_ip_reg_limit_num/0,   %% 同IP注册人数
    is_cluster/0,               %% 是否跨服服务
    is_gctrl/0,                 %% 是否游戏节点
    is_group_dist/0,            %% 是否分布式服务
    is_notice_edit_ban/1       %% 当前是否敏感时间，可能要屏蔽某些公告修改
]).


%% 各项目自定函数列表
-export([
    get_server_line_id/0, %% 获取分线的ServerID（LineID）,非分线为0
    get_stop_prepare_second/0,
    get_stop_prepare_msg/0,
    get_upf/1,  %% 把PF转为UPF
    get_all_upf/0,  %% 当前代理下的所有UPF
    is_fcm_open/0,
    get_node_type/0,
    get_node_type/1,
    is_server_close/0,
    is_icon_ban/1,
    get_gm_add_pay_percent/1,
    config_all/1,
    get_mnode_service_masters/0,
    get_mnode_service_masters/3,
    get_mnode_service_slaves/0,
    get_mnode_service_slaves/3,
    get_mnode_service_routers/0,
    get_mnode_service_routers/3,
    get_mysql_config/0, %% MySQL服务器配置
    get_topo_mysql_config/0,
    get_log_prefix/0, %% 日志文件名的前缀,
    get_server_start_time/0,
    can_stop_fast/0 %% 根据topo_mode判断是否可以快速停止
]).

%% =======================================================
%% 游戏服必须提供的函数
%% =======================================================

%% 支持2种文件类型: record_consult,key_value_consult
get_basic_configs() ->
    [
        {mgee_topo, "mgee_topo.config", key_value_consult}
%%        {mchatmon_config, "mchatmon_config.config", key_value_consult},
%%        {cfg_mnode_service, "cfg_mnode_service.config", key_value_consult},
%%        {sdk, "sdk.config", key_value_consult},
%%        {mcweb_pay, "mcweb_pay.config", key_value_consult}
    ].
get_selfload_configs() ->
    [
        %%启动自加载
%%        {trace, "trace.config", key_value_consult},
%%        {robot, "robot.config", key_value_consult},
%%        {agent, "agent.config", key_value_consult},
%%        {servers, "servers.config", key_value_consult},
%%        {switching, "switching.config", key_value_consult},
%%        {switch, "switch.config", key_value_consult},
        {common, "common.config", key_value_consult}
%%        {portal, "portal.config", key_value_consult}
    ].
get_activity_configs() ->
    [].

%% reload(spec_activity)->
%%     mact:reload();
%%reload(mews) ->
%%    mconf_dyn:reload(mews),
%%    mews:reload_config();
reload(FileT) ->
    File = mlib_tool:to_list(FileT),
    case File of
        "cfg" ++ _ -> %% cfg开头的文件
            ErlCfgFileDir = mconf_dyn:get_config_path() ++ "erl/",
            FileName = ErlCfgFileDir ++ File ++ ".erl",
            compile:file(FileName, [{i, ErlCfgFileDir}, {outdir, common_config:get_dge_root() ++ "ebin"}]),
            c:l(FileT),
            ok;
        _ ->
            mconf_dyn:reload(FileT)
    end.


is_debug() ->
    [Val] = mconf_dyn:find(common, is_debug),
    Val.

%% @doc 允许通过特殊配置启用GM命令, 用于外服测试
is_allow_gm_cmd() ->
    case mconf_dyn:find(common, is_allow_gm_cmd) of
        [Val] -> Val;
        _ -> is_debug()
    end.

%% @doc 允许登录的platform
%% @returns [PFID]
get_allow_platforms(AgentID) ->
    case mconf_dyn:find(portal, AgentID) of
        [AllowPlatformList] ->
            AllowPlatformList;
        _ ->
            []
    end.


is_word_filter() ->
    case mconf_dyn:find(common, word_filter) of
        [ValT] when erlang:is_boolean(ValT) ->
            ValT;
        _ ->
            true
    end.

is_robot_move() ->
    case mconf_dyn:find(robot, move_switch) of
        [ValT] when erlang:is_boolean(ValT) ->
            ValT;
        _ ->
            false
    end.

%% @doc 服务端大版本号
%%      当前版本若是：B_beta0.2.4.0-20151121.32885.32885，此函数的返回值为：B_beta0.2.4.0
%% @returns VersionString
get_game_version() ->
    ensure_version_mod(cfg_server_version),
    cfg_server_version:get_version().
get_version() ->
    [V] = mconf_dyn:find(cfg_version_aux, svn_version),
    BV = get_game_version(),
    <<BV/binary, <<"_">>/binary, V/binary>>.
ensure_version_mod(Mod) ->
    case code:is_loaded(Mod) of
        {'file', _} -> ok;
        _ ->
            VersionInfo = get_dge_root() ++ "/version_server.txt",
            case file:read_file(VersionInfo) of
                {ok, Version} ->
                    [Major | _] = string:tokens(binary_to_list(Version), "-"),
                    Major;
                _ ->
                    Major = "undefined"
            end,
            gen_version_mod(Mod, Major)
    end.
gen_version_mod(Mod, Major) ->
    Src = lists:concat([
        "-module(", Mod, ").\n"
        "-export([get_version/0]).\n"
        "get_version() -> <<\"", Major, "\">>.\n"]),
    {Mod, Code} = dynamic_compile:from_string(Src),
    code:load_binary(Mod, lists:concat([Mod, ".erl"]), Code),
    ok.


%% 获取开服日期 {{Year, Month, Day}, {Hour, Min, Sec}}
get_open_day() ->
    [Val] = mconf_dyn:find(common, server_start_datetime),
    Val.

%% 获得当前为开服第几天,如果今天是6月28日,开服日期为6月28日,则今天为开服第一天,返回1
get_opened_days() ->
    [{Date, _}] = mconf_dyn:find(common, server_start_datetime),
    {Date2, _} = erlang:localtime(),
    A = calendar:date_to_gregorian_days(Date2) - calendar:date_to_gregorian_days(Date) + 1,
    case A < 1 of
        true ->
            1;
        false ->
            A
    end.
get_open_time() ->
    DateTime = get_open_day(),
    mtime:timestamp(DateTime).


%% 获取代理商名字
get_agent_code() ->
    [Val] = mconf_dyn:find(common, agent_code),
    Val.
get_agent_id() ->
    [Val] = mconf_dyn:find(common, agent_id),
    Val.
get_game_agent_id() ->
    case mconf_dyn:find(common, log_agent_id) of
        [Val] -> Val;
        [] -> get_agent_id()
    end.
%% 获取游戏服ID
get_server_id() ->
    [Val] = mconf_dyn:find(common, server_id),
    Val.

get_log_prefix() ->
    case mconf_dyn:find(common, log_prefix) of
        [Val] -> Val;
        _ -> ""
    end.

get_game_name() ->
    [GameName] = mconf_dyn:find(common, game_name),
    GameName.

get_game_code() ->
    [GameCode] = mconf_dyn:find(common, game_code),
    GameCode.

get_game_id() ->
    [GameID] = mconf_dyn:find(common, game_id),
    GameID.

get_cookie_extend() ->
    [Val] = mconf_dyn:find(common, cookie_extend),
    Val.

%% 获取防沉迷验证的地址
get_fcm_validation_url() ->
    [Val] = mconf_dyn:find(common, fcm_validation_url),
    Val.

get_fcm_validation_key() ->
    [Val] = mconf_dyn:find(common, fcm_validation_key),
    Val.

get_log_level() ->
    [Val] = mconf_dyn:find(common, log_level),
    Val.

get_log_dir() ->
    [Val] = mconf_dyn:find(common, log_dir),
    Val.

merged_times() ->
    case mconf_dyn:find(common, merged_times) of
        [Times] when erlang:is_integer(Times) -> Times;
        _ -> 0
    end.
%% 本服务器是否是合服后的服务器
is_merge() ->
    merged_times() > 0.

%% @doc 获取节点运行的代码目录
%% 此参数在系统启动时由启动脚本决定
%% @end
get_dge_root() ->
    {ok, [[MgeRoot]]} = init:get_argument(dge_root),
    MgeRoot.

get_mnesia_dir() ->
    AgentCode = get_agent_code(),
    ServerID = get_server_id(),
    GameCode = get_game_code(),
    ServerType = get_server_type(),
    "/data/database/mnesia/" ++ GameCode ++ "_" ++ AgentCode ++ "_" ++ mlib_tool:to_list(ServerID) ++ "/" ++ mlib_tool:to_list(ServerType) ++ "/".

%% @doc 获取最高在线限制
get_max_online() ->
    [MaxOnline] = mconf_dyn:find(common, max_online),
    MaxOnline.

%% @doc 获取排队人数
get_queue_num() ->
    [QueueNum] = mconf_dyn:find(common, queue_num),
    QueueNum.

%% @doc
is_lite() ->
    case init:get_argument(lite) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

%% @doc 强制不启动mdb_app
is_no_db() ->
    case erlang:get({?MODULE, is_no_db}) of
        undefined ->
            Ret =
                case init:get_argument(no_db) of
                    {ok, _} -> true;
                    _ -> false
                end,
            erlang:put({?MODULE, is_no_db}, Ret),
            Ret;
        Ret -> Ret
    end.

%% @doc 强制不连接mysql
is_no_mysql() ->
    case erlang:get({?MODULE, is_no_mysql}) of
        undefined ->
            Ret =
                case init:get_argument(no_mysql) of
                    {ok, _} -> true;
                    _ -> false
                end,
            erlang:put({?MODULE, is_no_mysql}, Ret),
            Ret;
        Ret -> Ret
    end.

%% @returns ServerType :: integer()
get_server_type() ->
    case erlang:get({?MODULE, server_type}) of
        undefined ->
            {ok, [[ServerType]]} = init:get_argument(server_type),
            Ret = mlib_tool:to_integer(ServerType),
            erlang:put({?MODULE, server_type}, Ret),
            Ret;
        Ret -> Ret
    end.

%% ServerType 服务器类型  0:gctrl  1-69:gate  71:gdb  72:gworld  73:gmap  81:cdb  82:cworld  83:cmap
%% @returns ServerType :: atom()
get_server_type_code() ->
    get_server_type_code(get_server_type()).
get_server_type_code(ServerType)->
    if
        ServerType =:= ?MNODE_CCTRL_SUBID ->cctrl;
        ServerType =:= ?MNODE_CDB_SUBID ->cdb;
        ServerType =:= ?MNODE_CWORLD_SUBID ->cworld;
        ServerType =:= ?MNODE_CMAP_SUBID ->cmap;

        ServerType =:= ?MNODE_GCTRL_SUBID ->gctrl;
        ServerType =:= ?MNODE_GDB_SUBID ->gdb;
        ServerType =:= ?MNODE_GWORLD_SUBID ->gworld;
        ServerType =:= ?MNODE_GMAP_SUBID ->gmap;
        ServerType >=?MNODE_GATE_MIN_SUBID  andalso ServerType =<?MNODE_GATE_MAX_SUBID  -> gate
    end.

%% @doc 服务器IPv4地址
get_server_ip() ->
    case os:getenv("CUSTOM_IP") of
        false ->
            {ok, IPInfo} = inet:getifaddrs(),
            {"eth0", IPInfo2} = lists:keyfind("eth0", 1, IPInfo),
            [{IPN1, IPN2, IPN3, IPN4} | _] = [IPTuple || {addr, IPTuple} <- IPInfo2, erlang:size(IPTuple) =:= 4],
            lists:concat([IPN1, ".", IPN2, ".", IPN3, ".", IPN4]);
        IPStr -> IPStr
    end.

%% @doc 当前运行的语言版本
%% @returns Lang :: atom()
get_lang() ->
    case init:get_argument(lang) of
        {ok, [[Lang]]} -> mlib_tool:to_atom(Lang);
        _ -> default
    end.
%%     case mconf_dyn:find(common, lang) of
%%         [Lang] -> Lang;
%%         _ -> default
%%     end.

%% @doc 对于内测机, 启动较少的进程池减轻压力, 注意和lite启动模式不是一回事
is_lite_server_mode() ->
    [true] =:= mconf_dyn:find(switching, is_lite_server_mode).


get_apps() ->
    get_apps(get_server_type()).
get_apps(ServerType) ->
    TopoMode = get_topo_mode(),
    [TopoAppList] = mconf_dyn:find(mgee_topo, {topo_mode, TopoMode}),
    ServerTypeCode = get_server_type_code(ServerType),
    case lists:keyfind(ServerTypeCode, 1, TopoAppList) of
        {_ServerTypeCode, Apps} ->
            Apps;
        _ -> []
    end.

can_stop_fast() ->
    TopoMode = get_topo_mode(),
    BanList =
        case mconf_dyn:find(mgee_topo, {ban_stop_fast, TopoMode}) of
            [List] -> List;
            _ -> [List] = mconf_dyn:find(mgee_topo, {ban_stop_fast, default}), List
        end,
    ServerID = get_server_id(),
    not lists:any(fun({StartID, EndID})-> StartID=<ServerID andalso ServerID=<EndID; (BanID) -> ServerID=:=BanID  end, BanList).

get_apps_lite() ->
    [LiteApps] = mconf_dyn:find(mgee_topo, apps_lite),
    LiteApps.

%% @doc 本节点需要启动的分布式服务master组件
%% @returns [ServiceName]
get_mnode_service_masters() ->
    TopoMode = get_topo_mode(),
    ServerType = get_server_type_code(),
    ServerID = get_server_id(),
    get_mnode_service_masters(TopoMode, ServerType, ServerID).
%% @returns [ServiceName]
get_mnode_service_masters(TopoMode, ServerType, ServerID) ->
    AddServiceNames =
        case mconf_dyn:find(mgee_topo, {service_masters, TopoMode, ServerType, ServerID}) of
            [TmpAddServiceNames] ->
                TmpAddServiceNames;
            [] ->
                []
        end,
    BaseServiceNames =
        case mconf_dyn:find(mgee_topo, {service_masters, TopoMode, ServerType}) of
            [] -> [];
            [TmpBaseServiceNames] -> TmpBaseServiceNames
        end,
    ListServiceNames =
        case mconf_dyn:find(mgee_topo, {service_masters_list, TopoMode, ServerType}) of
            [] -> [];
            [TmpListServiceNames] ->
                lists:foldl(fun({NeedServer, List}, Acc) ->
                    case NeedServer of
                        {StartServer, EndServer} when StartServer =< ServerID andalso ServerID =< EndServer ->
                            List ++ Acc;
                        NeedServerID when NeedServerID =:= ServerID ->
                            List ++ Acc;
                        _ ->
                            Acc
                    end
                end, [], TmpListServiceNames)
        end,
    lists:usort(ListServiceNames ++ AddServiceNames ++ BaseServiceNames).

%% @doc 本节点需要启动的分布式服务slave组件
%% @returns [ServiceName]
get_mnode_service_slaves() ->
    TopoMode = get_topo_mode(),
    ServerType = get_server_type_code(),
    ServerID = get_server_id(),
    get_mnode_service_slaves(TopoMode, ServerType, ServerID).
%% @returns [ServiceName]
get_mnode_service_slaves(TopoMode, ServerType, ServerID) ->
    AddServiceNames =
        case mconf_dyn:find(mgee_topo, {service_slaves, TopoMode, ServerType, ServerID}) of
            [TmpAddServiceNames] ->
                TmpAddServiceNames;
            [] ->
                []
        end,
    BaseServiceNames =
        case mconf_dyn:find(mgee_topo, {service_slaves, TopoMode, ServerType}) of
            [] -> [];
            [TmpBaseServiceNames] -> TmpBaseServiceNames
        end,
    ListServiceNames =
        case mconf_dyn:find(mgee_topo, {service_slaves_list, TopoMode, ServerType}) of
            [] -> [];
            [TmpListServiceNames] ->
                lists:foldl(fun({NeedServer, List}, Acc) ->
                    case NeedServer of
                        {StartServer, EndServer} when StartServer =< ServerID andalso ServerID =< EndServer ->
                            List ++ Acc;
                        NeedServerID when NeedServerID =:= ServerID ->
                            List ++ Acc;
                        _ ->
                            Acc
                    end
                end, [], TmpListServiceNames)
        end,
    lists:usort(ListServiceNames ++ AddServiceNames ++ BaseServiceNames).

%% @doc 本节点需要启动的分布式服务router组件
%% @returns [ServiceName | {ServiceName, MinIdx, MaxIdx}]
get_mnode_service_routers() ->
    TopoMode = get_topo_mode(),
    ServerType = get_server_type_code(),
    ServerID = get_server_id(),
    get_mnode_service_routers(TopoMode, ServerType, ServerID).
%% @returns [ServiceName | {ServiceName, MinIdx, MaxIdx}]
get_mnode_service_routers(TopoMode, ServerType, ServerID) ->
    AddServiceNames =
        case mconf_dyn:find(mgee_topo, {service_routers, TopoMode, ServerType, ServerID}) of
            [TmpAddServiceNames] ->
                TmpAddServiceNames;
            [] ->
                []
        end,
    BaseServiceNames =
        case mconf_dyn:find(mgee_topo, {service_routers, TopoMode, ServerType}) of
            [] -> [];
            [TmpBaseServiceNames] -> TmpBaseServiceNames
        end,
    ListServiceNames =
        case mconf_dyn:find(mgee_topo, {service_routers_list, TopoMode, ServerType}) of
            [] -> [];
            [TmpListServiceNames] ->
                lists:foldl(fun({NeedServer, List}, Acc) ->
                    case NeedServer of
                        {StartServer, EndServer} when StartServer =< ServerID andalso ServerID =< EndServer ->
                            List ++ Acc;
                        NeedServerID when NeedServerID =:= ServerID ->
                            List ++ Acc;
                        _ ->
                            Acc
                    end
                end, [], TmpListServiceNames)
        end,
    lists:usort(ListServiceNames ++ AddServiceNames ++ BaseServiceNames).

%% get_topo_nodes(ServerType) ->
%%     [TopoNodes] = mconf_dyn:find(common, topo_nodes),
%%     case lists:keyfind(ServerType, 1, TopoNodes) of
%%         false -> {error, not_found};
%%         TopoNode -> {ok, TopoNode}
%%     end.
%%
%% get_topo_nodes() ->
%%     [TopoNodes] = mconf_dyn:find(common, topo_nodes),
%%     TopoNodes.


get_topo_mode() ->
    [TopoMode] = mconf_dyn:find(common, topo_mode),
    TopoMode.

get_web_port() ->
    [WebPort] = mconf_dyn:find(common, web_port),
    WebPort.

get_gateway_port() ->
    case mconf_dyn:find(common, gateway_port) of
        [Port] -> {ok, Port};
        _ -> {error, not_found}
    end.
%%     ServerType = get_server_type(),
%%     case get_topo_nodes(ServerType) of
%%         {ok, {_ServerType, _IP, Port}} ->
%%             {ok, Port};
%%         _ ->
%%             {error, not_found}
%%     end.

%% @doc 注册到mcweb的ip  没有配置的话会取get_gateway_ip
get_mcweb_reg_ip()->
    case mconf_dyn:find(common, mcweb_reg_ip) of
        [IP] -> {ok, IP};
        _ ->
            get_gateway_ip()
    end.

get_gateway_ip() ->
    case mconf_dyn:find(common, gateway_ip) of
        [IP] -> {ok, IP};
        _ ->
            case mconf_dyn:find(common, server_ip) of
                [IP] -> {ok, maybe_use_ip_alias(IP)};
                _ -> {error, not_found}
            end
    end.
%%     ServerType = get_server_type(),
%%     case get_topo_nodes(ServerType) of
%%         {ok, {_ServerType, IP, _Port}} ->
%%             {ok, maybe_use_ip_alias(IP)};
%%         _ ->
%%             {error, not_found}
%%     end.

get_gateway_ip_port() ->
    case get_gateway_ip() of
        {ok, IP} ->
            case get_gateway_port() of
                {ok, Port} -> {ok, {IP, Port}};
                _ -> {error, not_found}
            end;
        _ -> {error, not_found}
    end.
%%     ServerType = get_server_type(),
%%     case get_topo_nodes(ServerType) of
%%         {ok, {_ServerType, IP, Port}} ->
%%             {ok, {maybe_use_ip_alias(IP), Port}};
%%         _ ->
%%             {error, not_found}
%%     end.

%% @doc 获取节点启动时的时间戳
get_server_start_time() ->
    case is_debug() of
        true ->
            case mconf_dyn:find(switching, server_start_time) of
                [Time] ->
                    Time;
                _ ->
                    0
            end;
        _ ->
            [Time] = mconf_dyn:find(switching, server_start_time),
            Time
    end.

is_remove() ->
    case mconf_dyn:find(switching, ?IS_REMOVE) of
        [IsRemove] -> IsRemove;
        [] -> false
    end.

%% @doc 给前端测试包(不访问入口机)的BGP信息, 正常应该是入口机(web)提供的
get_bgp_ip_port() ->
    case mconf_dyn:find(common, bgp) of
        [{IP, Port}] ->
            {ok, {IP, Port}};
        _ ->
            {ok, {"", ""}}
    end.

maybe_use_ip_alias(IP) ->
    case mconf_dyn:find(common, ip_alias) of
        [{IP, Alias}] -> Alias;
        _ -> IP
    end.

login_ip_whitelist() ->
    case mconf_dyn:find(common, office_ip) of
        [IPStrings] ->
            IPStrings;
        _ ->
            []
    end.

%%is_ip_whitelist(IP) ->
%%    world_ip_server:is_sz_ip(IP) orelse lists:member(IP, login_ip_whitelist()).

login_ip_limit_num() ->
    case mconf_dyn:find(switching, roles_per_ip) of
        [Num] when is_number(Num) -> Num;
        _ -> ?IP_MAX_ONLINE_ROLE_NUM
    end.

login_ip_reg_limit_num() ->
    case mconf_dyn:find(switching, regs_per_ip) of
        [Num] when is_number(Num) -> Num;
        _ -> ?IP_MAX_REG_ROLE_NUM
    end.

%% =======================================================
%% 游戏服自定义函数
%% =======================================================

get_node_type() ->
    AgentID = common_config:get_agent_id(),
    get_node_type(AgentID).
get_node_type(AgentID) ->
    case AgentID > 0 andalso AgentID < 800 of
        true ->
            ?CROSS_GGROUP;
        false ->
            ?CROSS_CCLUSTER
    end.

is_server_close() ->
    case mconf_dyn:find(switching, ?SERVER_CLOSE_FLAG) of
        [true] -> true;
        _ -> false
    end.

%% 获得停止分线之前的广播时间,单位为秒
get_stop_prepare_second() ->
    [Val] = mconf_dyn:find(cfg_etc, stop_prepare_second),
    Val.

get_stop_prepare_msg() ->
    [Val] = mconf_dyn:find(cfg_etc, stop_prepare_msg),
    Val.

get_upf(PF) ->
    case mconf_dyn:find(agent, {upf, PF}) of
        [UPF] -> UPF;
        _ -> PF
    end.

get_all_upf() ->
    case mconf_dyn:find(agent, {agent_pf, get_agent_id()}) of
        [UPFList] -> UPFList;
        _ -> []
    end.

%% 不能在角色进程调用
get_server_line_id() ->
    case get_node_type() of
        ?CROSS_CCLUSTER ->
            get_server_id();
        _ ->
            0
    end.

%% 判断防沉迷是否打开
is_fcm_open() ->
    case mconf_dyn:find(switching, fcm) of
        [true] -> true;
        _ -> false
    end.


is_cluster() ->
    get_agent_id() >= ?MNODE_CCLUSTER_MIN.

is_gctrl() ->
    get_agent_id() =:= get_game_agent_id().

is_group_dist() ->
    [TopoMode] = mconf_dyn:find(common, topo_mode),
    [List] = mconf_dyn:find(mgee_topo, {topo_mode, TopoMode}),
    case List of
        [_A, _B | _] ->
            true;
        _ ->
            false
    end.

is_icon_ban(Icon) ->
    [true] =:= mconf_dyn:find(cfg_ban_icon, Icon).

is_notice_edit_ban(Time) ->
    case mconf_dyn:find(switching, notice_edit_ban) of
        [{StartTime, EndTime}] when erlang:is_integer(StartTime) andalso erlang:is_integer(EndTime) ->
            StartTime=<Time andalso Time=<EndTime;
        [{{{_,_,_},{_,_,_}}=StartTime, {{_,_,_},{_,_,_}}=EndTime}] ->
            DateTime = mtime:timestamp_to_datetime(Time),
            StartTime=<DateTime andalso DateTime=<EndTime;
        _ ->
            false
    end.

get_gm_add_pay_percent(IsBind) ->
    AgentCode = get_agent_code(),
    case mconf_dyn:find(cfg_platform, {agent_gm_pay_percent, AgentCode, IsBind}) of
        [Percent] when erlang:is_integer(Percent)
            andalso Percent >= 0 andalso Percent =< 100 -> Percent;
        _ -> 100
    end.

config_all(ConfigName) ->
    ConfigName:all().


%% @doc MySQL服务器配置
%% @returns #{host := Host, port := Port, user := Username, pass := Password, db := Database, pool := PoolOptions} | undefined
get_mysql_config() ->
    case (not is_no_mysql()) andalso mconf_dyn:find(common, mysql) of
        [#{host := _Host,
            port := _Port,
            user := _Username,
            pass := _Password,
            db := _Database,
            pool := _PoolOptions} = Config] ->
            TopoMysqlConfig = get_topo_mysql_config(),
            maps:merge(Config, TopoMysqlConfig);
        _ ->
            undefined
    end.

get_topo_mysql_config() ->
    TopoMode = get_topo_mode(),
    ServerType = get_server_type_code(),
    ServerID = get_server_id(),
    case mconf_dyn:find(mgee_topo, {mysql, TopoMode, ServerType}) of
        [ConfigList] ->
            lists:foldl(fun({NeedServer, Return}, Acc) ->
                case NeedServer of
                    {StartServerID, EndServerID} when StartServerID =< ServerID andalso ServerID =< EndServerID ->
                        Return;
                    ServerID ->
                        Return;
                    _ ->
                        Acc
                end
            end, maps:new(), ConfigList);
        [] ->
            maps:new()
    end.
