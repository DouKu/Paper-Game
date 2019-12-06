%%%----------------------------------------------------------------------
%%% @doc 负责脚本控制游戏服的主要模块
%%% 本模块会启动一个控制节点，通过rpc的方式对游戏服进行控制
%%% @end
%%%----------------------------------------------------------------------
-module(dkmgr).

-export([
    start/0,
    stop/0,
    stop/1,
    start_apps/0,
    stop_apps/0
]).


-export([
    print_processes/0,
    cmd/0
]).

-include("dkge.hrl").
%% 执行远程命令后返回给脚本的状态
-define(STATUS_SUCCESS, 0). %%成功
-define(STATUS_ERROR,   1). %%错误
-define(STATUS_USAGE,   2). %%打印使用方法
-define(STATUS_BADRPC,  3). %%执行失败

print(Format, Args) ->
    {{Y,Mo,D},{H,Mi,S}} = erlang:localtime(),
    catch io:format("==== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ===(~w) : " ++ Format,
        [Y, Mo, D, H, Mi, S, node()] ++ Args).

-define(SYSTEM_LOG(Format, Args), erlang:send(mmgr_log_server, {system_log, erlang:localtime(), Format, Args})).


%% @doc mmgr_ctl节点的启动执行函数
%% 此函数负责远程操作机游戏服
%% @end
-spec cmd()->any().
cmd() ->
    case init:get_plain_arguments() of
        [SNode | Args]->
            %io:format("plain arguments is:~n~p", [AArgs]),
            SNode1 = case string:tokens(SNode, "@") of
                [_Node, _Server] ->
                    SNode;
                _ ->
                    case net_kernel:longnames() of
                        true ->
                            SNode ++ "@" ++ inet_db:gethostname() ++
                                "." ++ inet_db:res_option(domain);
                        false ->
                            SNode ++ "@" ++ inet_db:gethostname();
                        _ ->
                            SNode
                    end
            end,
            Node = erlang:list_to_atom(SNode1),
            case dkmgr_misc:get_rpc_timeout(Args) of
                infinity ->
                    Status = cmd(Node, Args);
                Timeout ->
                    Status = cmd(Node, Args, Timeout)
            end,
            halt(Status);
        _ ->
            dkmgr_misc:print_usage(),
            halt(?STATUS_USAGE)
    end.

cmd(Node, Args) ->
    case rpc:call(Node, dkmgr_misc, process, [Args]) of
        {badrpc, Reason} ->
            print("RPC failed on the node ~w: ~w~n", [Node, Reason]),
            ?STATUS_BADRPC;
        ok ->
            ?STATUS_SUCCESS;
        ErrorMsg->
            print("RPC failed on the node ~w: ~w~n", [Node, ErrorMsg]),
            ?STATUS_ERROR
    end.

cmd(Node, Args, Timeout) ->
    case rpc:call(Node, dkmgr_misc, process, [Args], Timeout) of
        {badrpc, Reason} ->
            print("RPC failed on the node ~w: ~w~n", [Node, Reason]),
            ?STATUS_BADRPC;
        ok ->
            ?STATUS_SUCCESS;
        error ->
            ?STATUS_ERROR;
        ErrorMsg->
            print("RPC failed on the node ~w: ~w~n", [Node, ErrorMsg]),
            ?STATUS_ERROR
    end.


start()->
    try
        dkmgr_app:start(),
        AppList = dkmgr_misc:get_start_apps(),
        [begin
            ?SYSTEM_LOG("~ts:~p~n", ["准备启动", App]),
            App:start(),
            ?SYSTEM_LOG("~ts:~p~n", ["启动成功", App])
        end || App <- AppList],
        ?SYSTEM_LOG("~ts~n", ["游戏启动成功"]),
        timer:sleep(1000),
        RmStarting = io_lib:format("rm ~s",[dkmgr_misc:get_starting_flag()]),
        os:cmd(RmStarting),
        TouchSucc = io_lib:format("touch ~s",[dkmgr_misc:get_succ_flag()]),
        os:cmd(TouchSucc),
        mlib_sys:gc(102400),
        ?SYSTEM_LOG("~ts ~n", ["按ctl-c退出"]),
        kill_tail(),
        ok
    catch
        Class:Error ->
            StackTrace = erlang:get_stacktrace(),
            ?SYSTEM_LOG("start error, class:~p, reason:~p, stacktrace:~p  ~n", [Class, Error, StackTrace]),
            kill_tail(),
            erlang:raise(Class, Error, StackTrace)
    end.

kill_tail() ->
    Cmd = lists:flatten(io_lib:format("ps awx | grep 'tail -f ~s' | grep -v 'grep' | awk '{print $1}' | xargs kill -9", [dkmgr_misc:get_mgr_log()])),
    os:cmd(Cmd).


stop()->
    catch print("shutdown...~n", []),
    stop(dkmgr_misc:get_stop_apps()),
    catch print("stopping mmgr_app...~n", []),
    dkmgr_app:stop(),
    catch print("stopped mmgr_app~n", []),
    init:stop().
stop_apps() ->
    catch print("shutdown...~n", []),
    stop(dkmgr_misc:get_stop_apps()),
    catch print("stopping mmgr_app...~n", []),
    dkmgr_app:stop(),
    catch print("stopped mmgr_app~n", []),
    ok.
start_apps() ->
    try
        dkmgr_app:start(),
        AppList = dkmgr_misc:get_start_apps(),
        [begin
            ?SYSTEM_LOG("~ts:~p~n", ["准备启动", App]),
            print("~ts:~p~n", [mlib_tool:to_list(<<"准备启动"/utf8>>), App]),
            App:start(),
            ?SYSTEM_LOG("~ts:~p~n", ["启动成功", App]),
            print("~ts:~p~n", [mlib_tool:to_list(<<"启动成功"/utf8>>), App])
        end || App <- AppList],
        ?SYSTEM_LOG("~ts~n", ["游戏启动成功"]),
        print("~ts~n", [mlib_tool:to_list(<<"游戏启动成功"/utf8>>)]),
        timer:sleep(1000),
        RmStarting = io_lib:format("rm ~s",[dkmgr_misc:get_starting_flag()]),
        os:cmd(RmStarting),
        TouchSucc = io_lib:format("touch ~s",[dkmgr_misc:get_succ_flag()]),
        os:cmd(TouchSucc),
        mlib_sys:gc(102400),
        ?SYSTEM_LOG("~ts ~n", ["按ctl-c退出"]),
        kill_tail(),
        ok
    catch
        Class:Error ->
            StackTrace = erlang:get_stacktrace(),
            ?SYSTEM_LOG("start error, class:~p, reason:~p, stacktrace:~p  ~n", [Class, Error, StackTrace]),
            kill_tail(),
            erlang:raise(Class, Error, StackTrace)
    end.

stop(AppList) ->
    [begin
        catch print("stopping ~w...~n", [App]),
        App:stop(),
        catch print("stopped ~w~n", [App])
    end || App <- AppList].


print_processes()->
    timer:sleep(5000),
    ProcessInfo = [{P,erlang:process_info(P)}||P<- erlang:processes()],
    AgentCode = common_config:get_agent_code(),
    ServerID = common_config:get_server_id(),
    {Y,M,D}=erlang:date(),
    {HH,MM,_SS} = erlang:time(),
    file:write_file(
        io_lib:format("/data/tmp/~s_~w.~w-~w-~w_~w:~w.processes.log",[AgentCode,ServerID,Y,M,D,HH,MM]),
        io_lib:format("~p", [ProcessInfo]),
        [write]),
%%    PortList = [{P, recon:port_info(P)}||P<- erlang:ports()],
    file:write_file(
        io_lib:format("/data/tmp/~s_~w.~w-~w-~w_~w:~w.port.log",[AgentCode,ServerID,Y,M,D,HH,MM]),
%%        io_lib:format("~p", [PortList]),
        [write]),
    timer:sleep(1000).


