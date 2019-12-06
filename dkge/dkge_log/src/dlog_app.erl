%%%-------------------------------------------------------------------
%%% @doc  日志app
%%%-------------------------------------------------------------------
-module(dlog_app).

-behaviour(application).

%% Application callbacks
-export([
    start/0,
    stop/0,
    start/2,
    stop/1]).



-define(APPS, [dlog_app]).
%% --------------------------------------------------------------------
-spec start() -> ok.
%% @doc 启动dlog_app
start() ->
    try
        ok = mlib_misc:start_applications(?APPS)
    after
        %%give the error loggers some time to catch up
        timer:sleep(100)
    end.

stop() ->
    ok = mlib_misc:stop_applications(?APPS).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, PID} = dlog_sup:start_link(),
    gen_event:add_handler(error_logger, dlog_syserror, []),
    LogLevel = dlog_misc:get_log_level(),
    dlog_loglevel:set(LogLevel),
    {ok, _} = dlog_logger:start(),

    {ok, PID}.

stop(_State) ->
    ok.





