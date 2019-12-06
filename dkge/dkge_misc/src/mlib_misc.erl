%%%-------------------------------------------------------------------
%%% @doc
%%%     不好归类的一些接口
%%% @end
%%%-------------------------------------------------------------------
-module(mlib_misc).


%% 启动帮助函数
-export([
    start_applications/1,
    stop_applications/1,
    critical_section/2
]).


manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    Iterate(fun (App, Acc) ->
        case Do(App) of
            ok -> [App | Acc];
            {error, {SkipError, _}} -> Acc;
            {error, Reason} ->
                lists:foreach(Undo, Acc),
                erlang:throw({error, {ErrorTag, App, Reason}})
        end
    end, [], Apps),
    ok.

%% @doc start applications
%% @throws {error, {ErrorTag, App, Reason}}
-spec start_applications([atom()]) -> ok.
start_applications(Apps) ->
    manage_applications(fun lists:foldl/3,
        fun application:start/1,
        fun application:stop/1,
        already_started,
        cannot_start_application,
        Apps).

%% @doc stop applications
%% @throws {error, {ErrorTag, App, Reason}}
-spec stop_applications([atom()]) -> ok.
stop_applications(Apps) ->
    manage_applications(fun lists:foldr/3,
        fun application:stop/1,
        fun application:start/1,
        not_started,
        cannot_stop_application,
        Apps).


%% @doc 节点内非重入方式执行函数, 即任意时刻只能有一个进程在执行此函数
%% @returns Ret | error
critical_section(TaskName, Fun) when is_atom(TaskName) ->
    Parent = self(),
    {WPID, MRef} =
        erlang:spawn_monitor(
            fun() ->
                Ret = do_critical_section(TaskName, Fun),
                Parent ! {self(), Ret}
            end),
    wait_for_reply(WPID, MRef).
do_critical_section(TaskName, Fun) ->
    %% 注册名字以防止并发冲突
    case catch erlang:register(TaskName, self()) of
        true ->
            (catch Fun());
        {'EXIT', _} ->
            case erlang:whereis(TaskName) of
                OPID when is_pid(OPID) ->
                    erlang:monitor(process, OPID),
                    receive
                        {'DOWN', _, _, OPID, _} -> ok
                    end,
                    do_critical_section(TaskName, Fun)
            end
    end.
wait_for_reply(WPID, MRef) ->
    receive
        {WPID, Ret} ->
            erlang:demonitor(MRef, [flush]),
            Ret;
        {'DOWN', _, _, WPID, _} ->
            error
    end.

