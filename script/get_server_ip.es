#!/usr/bin/env escript
%% -*- erlang -*-

%%%----------------------------------------------------------------------
%%% @doc 获取节点的ip地址, 考虑别名
%%%----------------------------------------------------------------------

%% 入口
main([IPStr]) ->
    get_server_ip(IPStr).

get_server_ip(IPStr)->
    CommonConfigList = get_common_config(),
    case lists:keyfind(ip_alias, 1, CommonConfigList) of
        {_, {IPStr, IP}} -> next;
        _ -> IP = IPStr
    end,
    io:format("~s", [IP]).

get_common_config()->
    ScriptPath = escript:script_name(),
    ScriptDir = filename:dirname(filename:dirname(filename:dirname(ScriptPath))),
    {ok, L} = file:consult(ScriptDir ++ "/setting/common.config"),
    L.