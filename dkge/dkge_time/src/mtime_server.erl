%%%-------------------------------------------------------------------
%%% 时间服务进程
%%%-------------------------------------------------------------------
-module(mtime_server).

-include_lib("stdlib/include/ms_transform.hrl").
-behaviour(gen_server).
-export([
    start/3,
    start_link/2,
    dispatcher/3,
    dispatcher_loop/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state, {last_wheel_timer = 0}).

-define(DAY_SECOND, 86400).
-define(MAX_LOOP_MS, 2000).
-define(LOOP_SECOND, 1000).

-define(MIN_INTERVAL, 30000). %% 最少多少MS需要一个脉冲, 以避免超长interval出现较大的误差

-define(MIN_DISPATCHERS, 5).
-define(MAX_DISPATCHERS, 5).
-define(DISPATCHER_CAPACITY, 200). %% 每个dispatcher最多分配多少注册进程

-type child() ::
'undefined' | pid().
-type startchild_err() ::
'already_present'
| {'already_started', Child :: child()}
| term().
-type startchild_ret() ::
{'ok', Child :: child()}
| {'ok', Child :: child(), Info :: term()}
| {'error', startchild_err()}.


-spec start(SupName, PName, LoopMSecs) -> startchild_ret() when
    SupName :: atom(),
    PName :: atom(),
    LoopMSecs :: [pos_integer()].
%% @doc 启动秒循环的mtime_XXX_server
start(SupName, [PName|PNames], LoopMSecs) ->
    [case erlang:is_integer(LoopMSec) andalso LoopMSec =< ?MAX_LOOP_MS andalso LoopMSec > 0 of
        true->ok;
        false->erlang:throw({mtime_server, illogical_loopms, PName, LoopMSecs})
    end || LoopMSec <- LoopMSecs],
    {ok, _} = supervisor:start_child(
        SupName,
        {PName, {?MODULE, start_link, [PName, LoopMSecs]}, transient, 10000, worker, [?MODULE]}),
    start(SupName, PNames, LoopMSecs);
start(_, [], _) ->
    ok.

%%--------------------------------------------------------------------
start_link(PName, LoopMSecs) ->
    gen_server:start_link({local, PName}, ?MODULE, [LoopMSecs], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
init([LoopMSs]) ->
    erlang:process_flag(trap_exit, true),
    init_register(),
    init_wheel(LoopMSs),
    add_dispatcher(),
%%     maybe_adjust_dispatchers(),
    {ok, #state{last_wheel_timer = mtime:now_ms()}}.

handle_call(detail, _From, State) ->
    {reply, erlang:get(), State};
handle_call({get_reg_pids, LoopMSec}, _From, State) ->
    {reply, get_registered_pids(LoopMSec), State};
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc 脉冲
handle_info(wheel_timer, State) ->
    State2 = do_wheel_timers(State),
    {noreply, State2};

handle_info({reg, Interval, PID}, State) ->
    do_reg(Interval, PID),
    {noreply, State};
handle_info({dereg, Interval, PID}, State) ->
    do_dereg(Interval, PID),
    {noreply, State};

handle_info({reg_timer, DailySec, Msg, PID}, #state{last_wheel_timer = Last} = State) ->
    reg_timer(DailySec, Msg, PID, Last),
    {noreply, State};
handle_info({dereg_timer, DailySec, Msg, PID}, State) ->
    dereg_timer(DailySec, PID, Msg),
    {noreply, State};
handle_info({dereg_timer, {daily, DailySec}, PID}, State) ->
    dereg_timer(DailySec, PID),
    {noreply, State};

handle_info({'DOWN', _, _, PID, _}, State) ->
    do_dispatcher_down(PID),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_wheel(Intervals) ->
    NowMS = mtime:now() * 1000,
    Timers = [{NowMS + Interval, Interval} || Interval <- Intervals],
    Wheel = lists:usort(Timers),
    set_wheel(Wheel),
    schedule_wheel_timer(NowMS).

%% @returns [{TriggerTime, Interval}], Interval :: integer() | {daily, DailySec}
get_wheel() ->
    case erlang:get(wheel) of
        undefined -> [];
        Ls -> Ls
    end.
set_wheel(Wheel) ->
    erlang:put(wheel, lists:usort(Wheel)).

%% @doc 如果没有定时就启动定时
maybe_schedule_wheel_timer() ->
    case erlang:get(loop_timer) of
        undefined ->
            schedule_wheel_timer(mtime:now_ms());
        _MRef ->
            %% 已经定时了
            ignore
    end.

schedule_wheel_timer(NowMS) ->
    Wheel = get_wheel(),
    case Wheel of
        [{NextMS, _Interval}|_] ->
            WaitMS = min(NextMS - NowMS, ?MIN_INTERVAL),
            case erlang:get(loop_timer_for) of
                OldMS when is_integer(OldMS), NowMS + WaitMS >= OldMS ->
                    ignore;
                _ ->
                    case erlang:get(loop_timer) of
                        undefined -> ignore;
                        OldMRef -> erlang:cancel_timer(OldMRef)
                    end,
                    MRef = erlang:send_after(WaitMS, self(), wheel_timer),
                    erlang:put(loop_timer, MRef),
                    erlang:put(next_trigger_time, NowMS + WaitMS),
                    ok
            end;
        [] ->
            case erlang:erase(loop_timer) of
                undefined -> ignore;
                MRef -> erlang:cancel_timer(MRef)
            end
    end.

%% @doc 触发时间轮上的所有计时器
do_wheel_timers(State) ->
    NowMS = mtime:now_ms(),
    Wheel = get_wheel(),
    erlang:erase(loop_timer),
    erlang:erase(next_trigger_time),
    do_wheel_timers(Wheel, [], NowMS),
    State#state{last_wheel_timer = NowMS}.
do_wheel_timers([], Pending, NowMS) ->
    set_wheel(Pending),
    schedule_wheel_timer(NowMS);
do_wheel_timers([{TriggerMS, Interval}|Wheel], Pending, NowMS) when NowMS >= TriggerMS ->
    trigger_wheel_timer(Interval, TriggerMS),
    NextTriggerMS = calc_next_trigger_time(TriggerMS, Interval),
    case NowMS >= NextTriggerMS of
        true -> do_wheel_timers([{NextTriggerMS, Interval}|Wheel], Pending, NowMS);
        _ -> do_wheel_timers(Wheel, [{NextTriggerMS, Interval}|Pending], NowMS)
    end;
do_wheel_timers([_|_] = Wheel, Pending, NowMS) ->
    set_wheel(Pending ++ Wheel),
    schedule_wheel_timer(NowMS).

trigger_wheel_timer({daily, DailySec} = Interval, _TimeMS) ->
    reset_next_daily_timestamp(DailySec),
    send_job(Interval);
trigger_wheel_timer(Interval, TimeMS) when Interval =:= 1000 ->
    send_job(Interval, {loop_sec, TimeMS div 1000});
trigger_wheel_timer(Interval, _TimeMS) ->
    send_job(Interval, {loop_msec, Interval}).

calc_next_trigger_time(CurrentMS, {daily, DailySec}) ->
    %% 每日固定点触发的
    get_next_daily_timestamp(DailySec * 1000, CurrentMS);
calc_next_trigger_time(CurrentMS, IntervalMS) when is_integer(IntervalMS) ->
    %% 周期触发的
    CurrentMS + IntervalMS.


maybe_add_wheel_gear(Interval) ->
    Wheel = get_wheel(),
    case lists:keyfind(Interval, 2, Wheel) of
        false ->
            NextTriggerMS = mtime:now_ms() + Interval,
            Wheel2 = lists:keystore(Interval, 2, Wheel, {NextTriggerMS, Interval}),
            set_wheel(Wheel2),
            ok;
        _ ->
            ignore
    end.
maybe_add_wheel_gear(NextTriggerMS, Interval) ->
    Wheel = get_wheel(),
    case lists:keyfind(Interval, 2, Wheel) of
        false ->
            Wheel2 = lists:keystore(Interval, 2, Wheel, {NextTriggerMS, Interval}),
            set_wheel(Wheel2),
            ok;
        _ ->
            ignore
    end.

%% @doc 不再有注册进程的时间点可以删除
maybe_del_wheel_gear(1000) ->
    ignore;
maybe_del_wheel_gear(Interval) ->
    case get_registered_count(Interval) of
        0 -> del_wheel_gear(Interval);
        _ -> ignore
    end.
del_wheel_gear(Interval) ->
    ETS1 = erlang:get(state_ets),
    ETS2 = erlang:get(client_ets),
    Wheel = get_wheel(),
    case lists:keytake(Interval, 2, Wheel) of
        {value, {_, _}, Wheel2} ->
            set_wheel(Wheel2),
            PIDs = get_registered_pids(Interval),
            [ets:delete(ETS1, {reg, Interval, PID}) || PID <- PIDs],
            ets:delete(ETS2, {reg, Interval}),
            case Interval of
                {daily, DailySec} -> reset_next_daily_timestamp(DailySec);
                _ -> ignore
            end,
            ok;
        _ ->
            ignore
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_register() ->
    ETS1 = ets:new(state_ets, [set, protected]),
    erlang:put(state_ets, ETS1),
    ETS2 = ets:new(clients, [bag, protected]),
    erlang:put(client_ets, ETS2),
    ok.

add_register(Interval, PID) ->
    ETS1 = erlang:get(state_ets),
    ETS2 = erlang:get(client_ets),
    case ets:lookup(ETS1, {reg, Interval, PID}) of
        [] ->
            maybe_add_wheel_gear(Interval),
            _Count = ets:update_counter(ETS1, {count, Interval}, 1, {{count, Interval}, 0}),
            ets:insert(ETS1, {{reg, Interval, PID}, 1}),
            ets:insert(ETS2, {{reg, Interval}, PID}),
            maybe_schedule_wheel_timer(),
            ok;
        _ -> ignore
    end.

del_register(Interval, PID) ->
    ETS1 = erlang:get(state_ets),
    ETS2 = erlang:get(client_ets),
    case ets:lookup(ETS1, {reg, Interval, PID}) of
        [] -> ignore;
        [O] ->
            ets:delete_object(ETS2, {{reg, Interval}, PID}),
            ets:delete_object(ETS1, O),
            _Count = ets:update_counter(ETS1, {count, Interval}, -1, {{count, Interval}, 1}),
            maybe_del_wheel_gear(Interval),
            ok
    end.

get_registered_count(Interval) ->
    ETS1 = erlang:get(state_ets),
    case ets:lookup(ETS1, {count, Interval}) of
        [] -> 0;
        [{_, C}] -> C
    end.

get_registered_pids(Interval) ->
    ETS2 = erlang:get(client_ets),
    [PID || {_, PID} <- ets:lookup(ETS2, {reg, Interval})].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_reg(Intervals, PID) when is_list(Intervals) ->
    [add_register(Interval, PID) || Interval <- Intervals],
    ok;
do_reg(Interval, PID) when is_integer(Interval) ->
    add_register(Interval, PID),
    ok.

do_dereg(Intervals, PID) when is_list(Intervals) ->
    [del_register(Interval, PID) || Interval <- Intervals],
    ok;
do_dereg(Interval, PID) ->
    del_register(Interval, PID),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc 每日指定时间(sec)触发
%% todo mjh
%% 两个进程都可以决定做某一个事情 但做决定时看到的条件(当前的时间)是变化的
%% 时间是缓存的话  角色的消息与跨天的消息的时序会导致问题
%% 1 注册消息过去刚好变成下一天的消息之后  没触发
%% 2 已跨天 服务未跨天 在跨天消息之前到  跨两次
%% 真实时间
%% 角色未跨天 消息在跨天后到达 没跨天
reg_timer(DailySec, Msg, PID, Last) ->
    NextTriggerMS = get_next_daily_timestamp(DailySec * 1000, Last),
    Interval = {daily, DailySec},
    maybe_add_wheel_gear(NextTriggerMS, Interval),
    add_register(Interval, PID),
    add_custom_msg(Interval, PID, Msg),
    ok.

%% @doc 取消每日指定时间(sec)触发
dereg_timer(DailySec, PID, Msg)->
    Interval = {daily, DailySec},
    case del_custom_msg(Interval, PID, Msg) of
        empty ->
            del_register(Interval, PID),
            maybe_del_wheel_gear(Interval);
        _ -> ignore
    end,
    ok.

%% @doc 取消每日指定时间(sec)触发
dereg_timer(DailySec, PID)->
    Interval = {daily, DailySec},
    del_custom_msg(Interval, PID),
    del_register(Interval, PID),
    maybe_del_wheel_gear(Interval),
    ok.

%% @doc 每日固定时间(第x秒)触发
%% @returns ms
get_next_daily_timestamp(DailySecMS, CurrentMS) ->
    case erlang:get({next_daily_timestamp, DailySecMS}) of
        undefined ->
            MidNightMS = mtime:midnight(CurrentMS div 1000) * 1000,
            TimestampMS = MidNightMS + DailySecMS,
            case TimestampMS > CurrentMS of
                true -> TimeMS = TimestampMS;
                _ -> TimeMS = TimestampMS + ?DAY_SECOND * 1000
            end,
            erlang:put({next_daily_timestamp, DailySecMS}, TimeMS);
        TimeMS ->
            ok
    end,
    TimeMS.

reset_next_daily_timestamp(DailySec) ->
    erlang:erase({next_daily_timestamp, DailySec * 1000}).


add_custom_msg(Interval, PID, Msg) ->
    ETS1 = erlang:get(state_ets),
    Key = {msg, Interval, PID},
    case ets:lookup(ETS1, Key) of
        [{_, MsgRefs}] -> ok;
        _ -> MsgRefs = []
    end,
    case is_need_ref(Msg) of
        true -> MRef = maybe_add_msg_ref(ETS1, Msg);
        _ -> MRef = Msg
    end,
    case lists:member(MRef, MsgRefs) of
        true -> ignore;
        _ -> ets:insert(ETS1, {Key, [MRef|MsgRefs]})
    end.
%% @returns empty | ok
del_custom_msg(Interval, PID, Msg) ->
    ETS1 = erlang:get(state_ets),
    case is_need_ref(Msg) of
        true ->
            case ets:lookup(ETS1, {msg, Msg}) of
                [{_, MRef}] -> del_msg_refs(ETS1, [MRef]);
                _ -> MRef = Msg
            end;
        _ ->
            MRef = Msg
    end,
    Key = {msg, Interval, PID},
    case ets:lookup(ETS1, Key) of
        [{_, MsgRefs}] -> ok;
        _ -> MsgRefs = []
    end,
    MsgRefs2 = lists:delete(MRef, MsgRefs),
    case MsgRefs2 of
        [] ->
            ets:delete(ETS1, Key),
            empty;
        _ ->
            ets:insert(ETS1, {Key, MsgRefs2}),
            ok
    end.
del_custom_msg(Interval, PID) ->
    ETS1 = erlang:get(state_ets),
    Key = {msg, Interval, PID},
    case ets:lookup(ETS1, Key) of
        [{_, MsgRefs}] -> ok;
        _ -> MsgRefs = []
    end,
    del_msg_refs(ETS1, MsgRefs),
    ets:delete(ETS1, Key).
%% @returns [Msg]
get_custom_msg(Interval, PID) ->
    ETS1 = erlang:get(state_ets),
    case ets:lookup(ETS1, {msg, Interval, PID}) of
        [] -> [];
        [{_, MsgRefs}] ->
            [begin
                case erlang:is_reference(MRef) of
                    true ->
                        [{_, Msg}] = ets:lookup(ETS1, {msg, MRef});
                    _ ->
                        Msg = MRef
                end,
                Msg
            end|| MRef <- MsgRefs]
    end.

%% @returns MRef
maybe_add_msg_ref(ETS1, Msg) ->
    case ets:lookup(ETS1, {msg, Msg}) of
        [{_, MRef}] -> ok;
        _ -> MRef = erlang:make_ref()
    end,
    ets:update_counter(ETS1, {msg_refc, MRef}, 1, {{msg_refc, MRef}, 0}),
    ets:insert(ETS1, [{{msg, MRef}, Msg}, {{msg, Msg}, MRef}]),
    MRef.
del_msg_refs(_, []) -> ok;
del_msg_refs(ETS1, [MRef|MsgRefs]) when is_reference(MRef) ->
    case ets:update_counter(ETS1, {msg_refc, MRef}, -1, {{msg_refc, MRef}, 1}) of
        0 ->
            [{_, Msg}] = ets:lookup(ETS1, {msg, MRef}),
            ets:delete(ETS1, {msg_refc, MRef}),
            ets:delete(ETS1, {msg, MRef}),
            ets:delete(ETS1, {msg, Msg}),
            ok;
        _ ->
            ok
    end,
    del_msg_refs(ETS1, MsgRefs);
del_msg_refs(ETS1, [_|MsgRefs]) ->
    del_msg_refs(ETS1, MsgRefs).

%% @doc 块头比较大的消息, 通过ref方式存放已减少内存开销
is_need_ref(Atom) when is_atom(Atom) -> false;
is_need_ref(Number) when is_number(Number) -> false;
is_need_ref({Atom1, Atom2}) when is_atom(Atom1), is_atom(Atom2) -> false;
is_need_ref({Atom, Number}) when is_atom(Atom), is_number(Number) -> false;
is_need_ref({Atom, Number1, Number2}) when is_atom(Atom), is_number(Number1), is_number(Number2) -> false;
is_need_ref(_Msg) -> true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc 把广播任务下放给dispatcher池, 需要逐个确认消息内容
send_job(Interval) ->
    Ds = get_dispatchers(),
    dispatch_job(Ds, {'$msg', Interval}).

%% @doc 把广播任务下放给dispatcher池, 统一广播消息内容
send_job(Interval, Msg) ->
    Ds = get_dispatchers(),
    dispatch_job(Ds, {'$msg', Interval, Msg}).

dispatch_job([{Dispatcher, _MRef}|_Ds], Job) ->
    erlang:send(Dispatcher, {job, Job}),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @returns [{PID, MRef}]
get_dispatchers() ->
    case erlang:get(dispatchers) of
        undefined -> [];
        L -> L
    end.
set_dispatchers(L) ->
    erlang:put(dispatchers, L).

%% @doc 新开dispatcher
add_dispatcher() ->
    Parent = self(),
    ETS2 = erlang:get(client_ets),
    ETS1 = erlang:get(state_ets),
    case erlang:spawn_monitor(?MODULE, dispatcher, [Parent, ETS1, ETS2]) of
        {PID, MRef} ->
            L = get_dispatchers(),
            set_dispatchers([{PID, MRef}|L]),
            ok;
        _ ->
            error
    end.


%% @doc dispatcher死亡, 可能需要重启
do_dispatcher_down(PID) ->
    L = get_dispatchers(),
    L2 = lists:keydelete(PID, 1, L),
    set_dispatchers(L2),
    add_dispatcher(),
    ok.


%% @doc dispatcher子进程的入口
dispatcher(Parent, ETS1, ETS2) ->
    erlang:monitor(process, Parent),
    erlang:put(client_ets, ETS2),
    erlang:put(state_ets, ETS1),
    dispatcher_loop(Parent).
dispatcher_loop(Parent) ->
    receive
        stop -> stop;
        {'DOWN', _, _, _, _} -> stop;
        {job, {'$msg', Interval, Msg}} ->
            PIDs = get_registered_pids(Interval),
            %% 固定的广播消息
            dispatcher_job(Interval, Msg, PIDs, Parent),
            ?MODULE:dispatcher_loop(Parent);
        {job, {'$msg', {daily, _} = Interval}} ->
            PIDs = get_registered_pids(Interval),
            %% 自定义的定时消息
            dispatcher_job(Interval, PIDs, Parent),
            ?MODULE:dispatcher_loop(Parent);
        _Other ->
            ignore
    end.
%% @doc 自定义定时消息
dispatcher_job({daily, DailySec} = Interval, PIDs, Parent) ->
    try
        [case erlang:is_process_alive(PID) of
            true ->
                Msgs = get_custom_msg(Interval, PID),
                [erlang:send(PID, Msg) || Msg <- Msgs];
            _ ->
                erlang:send(Parent, {dereg_timer, DailySec, PID})
        end || PID <- PIDs]
    catch
        _:_ -> ignore
    end.
%% @doc 统一定时广播消息
dispatcher_job(Interval, Msg, PIDs, Parent) ->
    try
        [case erlang:is_process_alive(PID) of
            true -> erlang:send(PID, Msg);
            _ -> erlang:send(Parent, {dereg, Interval, PID})
        end || PID <- PIDs]
    catch
        _:_ -> ignore
    end.


