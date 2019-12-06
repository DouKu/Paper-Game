%%%-------------------------------------------------------------------
%%% @doc dkmgr游戏入口
%%%-------------------------------------------------------------------
-module(dkmgr_app).

-behaviour(application).

-include("dkge.hrl").

-export([
    start/2,
    stop/1,
    start/0,
    stop/0
]).

-define(APPS, [sasl, asn1,inets, crypto, public_key, ssl,  mmgr_app]).

%% @doc
start() ->
    try
        ok = mlib_misc:start_applications(?APPS)
    after
        %%give the error loggers some time to catch up
        timer:sleep(100)
    end.

stop() ->
    ok = mlib_misc:stop_applications(?APPS).


%% --------------------------------------------------------------------
start(normal, []) ->
    {ok, SupPID} = dkmgr_sup:start_link(),
    lists:foreach(
        fun({Msg, Thunk}) ->
            io:format("starting ~-32s ...", [Msg]),
            Thunk(),
            io:format("done~n");
            ({Msg, M, F, A}) ->
                io:format("starting ~-20s ...", [Msg]),
                apply(M, F, A),
                io:format("done~n")
        end,
        [
            {"Init Config",
                fun() ->
                    dkmgr_misc:init_conf()
                end},
            {"Load Beams",
                fun() ->
                    dkmgr_misc:load_beams()
                end},
%%            {"Manager log",
%%                fun() ->
%%                    mmgr_log_server:start()
%%                end},
            {"Other Server",
                fun() ->
                    dkmgr_misc:start_server()
                end}
%%            {"Schedule Server",
%%                fun() ->
%%                    mmgr_sched_server:start()
%%                end},
%%            {"System Monitor",
%%                fun() ->
%%                    mmgr_monitor:start()
%%                end}
        ]),
    io:format("~nbroker running~n"),
    {ok, SupPID}.

stop(_State) ->
    ok.
