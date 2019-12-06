%%%-------------------------------------------------------------------
%%% @doc
%%%-------------------------------------------------------------------
-module(dlog_misc).


%% API
-export([
    get_log_level/0,
    get_log_dir/0,
    get_log_basename/0
]).

get_log_level() ->
    [LogLv] = mconf_dyn:find(common,log_level),
    LogLv.

get_log_dir() ->
    [LogDir] = mconf_dyn:find(common,log_dir),
    LogDir.

get_log_basename() ->
    AgentCode = common_config:get_agent_code(),
    ServerID  = common_config:get_server_id(),
    GameCode  = common_config:get_game_code(),
    Prefix = common_config:get_log_prefix(),
    lists:concat([Prefix, GameCode, "_", AgentCode, "_", ServerID]).

