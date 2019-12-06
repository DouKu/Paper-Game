%%%-------------------------------------------------------------------
%%% @doc 执行程序和启动获取函数
%%%-------------------------------------------------------------------
-module(dkmgr_misc).
-include("dkge.hrl").

%% 命令
-export([
    get_rpc_timeout/1,
    print_usage/0
]).

%% 数据接口
-export([
    get_mgr_log/0,
    get_log_header/1,
    get_starting_flag/0,
    get_succ_flag/0,
    init_conf/0,
    load_beams/0,
    start_server/0,
    get_start_apps/0,
    get_stop_apps/0
]).

%% ===========================

%% @doc RPC命令的timeout设置, 运维的工具可以发现超时的情况以便于报警
get_rpc_timeout(["reload_beam"|_]) -> 10000;
get_rpc_timeout(["reload_config"|_]) -> 10000;
get_rpc_timeout(["gstatus"|_]) -> 10000;
get_rpc_timeout(["status"|_]) -> 10000;
get_rpc_timeout(["mulog_size"|_]) -> 10000;
get_rpc_timeout(["get_port"|_]) -> 10000;
get_rpc_timeout(["get_game_info"|_]) -> 10000;
get_rpc_timeout(["set_remove"|_]) -> 20000;
get_rpc_timeout(["get_remove_state"|_]) -> 10000;
get_rpc_timeout(["get_game_stat"|_]) -> 10000;
get_rpc_timeout(["get_pid"|_]) -> 10000;
get_rpc_timeout(["get_common_info"|_]) -> 10000;
get_rpc_timeout(_) -> infinity.

%% @doc help命令
print_usage() ->
    CmdDescs =
        [
            {"status", "get node status"},
            {"start", "start node"},
            {"stop", "stop node"},
            {"reload_beam", "relad beam files"},
            {"reload_config", "relad config files"},
            {"exprs","execute the express"}
        ] ,
    MaxCmdLen =
        lists:max(lists:map(
            fun({Cmd, _Desc}) ->
                erlang:length(Cmd)
            end, CmdDescs)),
    NewLine = io_lib:format("~n", []),
    FmtCmdDescs =
        lists:map(
            fun({Cmd, Desc}) ->
                ["  ", Cmd, string:chars($\s, MaxCmdLen - erlang:length(Cmd) + 2),
                    Desc, NewLine]
            end, CmdDescs),
    ?PRINT(
        "Usage: mgectl command [argus]~n"
        "~n"
        "Available commands in this node node:~n"
        "~s"
        "~n"
        "Examples:~n"
        "  mgectl start~n",
        [FmtCmdDescs]).

%% ===========================
is_lite() ->
    common_config:is_lite().

get_game_code() ->
    common_config:get_game_code().
get_agent_code() ->
    common_config:get_agent_code().
get_server_id() ->
    common_config:get_server_id().
get_server_type() ->
    common_config:get_server_type().

get_apps() ->
    common_config:get_apps().
get_apps_lite() ->
    common_config:get_apps_lite().
get_dge_root()->
    common_config:get_dge_root().
%% ===========================
get_mgr_log() ->
    io_lib:format("/data/logs/~s_manager_~s_~p.log", [get_game_code(),get_agent_code(), get_server_id()]).

get_log_header({{Y,Mo,D},{H,Mi,S}}) ->
    AgentCode = get_agent_code(),
    ServerID  = get_server_id(),
    ServerType = get_server_type(),
    io_lib:format("system_info ~p S~p ~p ==== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ===",
        [AgentCode, ServerID,ServerType, Y, Mo, D, H, Mi, S]).

get_starting_flag()->
    MgeRoot = get_dge_root(),
    io_lib:format(" ~s/starting_flag",[MgeRoot]).

get_succ_flag()->
    MgeRoot = get_dge_root(),
    io_lib:format(" ~s/start_succ",[MgeRoot]).

init_conf() ->
    mconf_dyn:init().

load_beams() ->
    MgeRoot = get_dge_root(),
    FileList = filelib:wildcard(MgeRoot ++ "ebin/*.beam"),
    ModuleList = [begin mlib_tool:list_to_atom(filename:basename(File, ".beam")) end || File <- FileList],
    code:ensure_modules_loaded(ModuleList),
    ok.

start_server()->
    mtime:start_server(common, mmgr_sup, [1000]).

get_start_apps()->
    case is_lite() of
        true ->
            [App || App <- get_apps_lite(), is_allowed(App)];
        _ ->
            [App || App <- get_apps(), is_allowed(App)]
    end.

is_allowed(mdb_app) ->
    not common_config:is_no_db();
is_allowed(_) -> true.

get_stop_apps()->
    lists:reverse(get_apps()).

%% ===========================