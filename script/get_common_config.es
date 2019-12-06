#!/usr/bin/env escript
%% -*- erlang -*-

main([KeyStr]) ->
    Key = eval(KeyStr ++ "."),
    case catch get_value(Key) of
        {ok, Value} when not is_list(Value) ->
            io:format("~w", [Value]),
            init:stop(0);
        {ok, List} ->
            case is_printable(List) of
                true -> io:format("~ts", [List]);
                _ -> io:format("~w", [List])
            end,
            init:stop(0);
        {error, _Error} ->
            io:format("error", []),
            init:stop(1)
    end;
main(_) ->
    io:format("error", []),
    init:stop(1).

get_value(Key) ->
    CommonConfigList = get_common_config(),
    case lists:keyfind(Key, 1, CommonConfigList) of
        {_, Value} -> {ok, Value};
        _ -> erlang:throw({error, not_found})
    end.

get_common_config()->
    ScriptPath = filename:absname(escript:script_name()), %% ./script/run/xxx
    RootDir = filename:dirname(filename:dirname(filename:dirname(ScriptPath))),
    {ok, L} = file:consult(RootDir ++ "/setting/common.config"),
    L.

eval(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

is_printable(List) ->
    lists:all(fun(C) when is_integer(C), C > 0, C =< 255 -> true end, List).

