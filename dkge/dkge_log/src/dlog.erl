%%%-------------------------------------------------------------------
%%% @doc 日志app
%%%-------------------------------------------------------------------
-module(dlog).

%% API
-export([
    i/0,
    set_loglevel/1,
    set_callback/2,
    set_callbacks/1
]).

-define(INFO_LEVEL, 4).

-spec i() -> ok.
%% @doc 输出log_level和各logLevel的callback
i() ->
    io:format("mgee log level:~p ~n",[dlog_loglevel:get()]),
    io:format("mgee log callback:~p ~n",[dlog_logger:get_callback()]),
    ok.

-spec set_loglevel(LogLevel) -> {module, dlog_entry} | ok when
      LogLevel :: 1..5.
%% @doc 设置日志等级
%% @param ErrorLogLevel 日志级别 1-5
%%     {1, "Critical"}
%%     {2, "Error"}
%%     {3, "Warning"}
%%     {4, "Info"}
%%     {5, "Debug"}
%% @end
set_loglevel(Loglevel) ->
    dlog_loglevel:set(Loglevel).

-spec set_callback(LogLevel, MFAs) -> {set_callback, {LogLevel, MFAs}} | ignore when
      LogLevel :: integer(),
      MFAs :: [{atom(), atom(), [term()]}].
%% @doc 设置1-3级别的回调执行，会在每次写日志时执行回调函数
set_callback(LogLevel,MFAs) when LogLevel >=1 andalso LogLevel =<3->
    dlog_logger:set_callback(LogLevel,MFAs);
set_callback(_LogLevel,_MFAs) ->
    ignore.

-spec set_callbacks(LogLevelMFAsList) -> ok when
      LogLevelMFAsList :: [{LogLevel, MFAs}],
      LogLevel :: 1..5,
      MFAs :: [{atom(), atom(), [term()]}].
%% @doc 设置1-3级别的回调执行，会在每次写日志时执行回调函数
set_callbacks([]) ->
    ok;
set_callbacks([{LogLevel,MFAs}|TCallBacks]) ->
    [begin code:ensure_loaded(M) end || {M,_F,_A} <- MFAs],
    set_callback(LogLevel,MFAs),
    set_callbacks(TCallBacks).



