%%%-------------------------------------------------------------------
%%% @doc
%%%     常用且通用的一些工具函数
%%% @end
%%%-------------------------------------------------------------------
-module(mlib_tool).
-include("global.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    ip/1,
    get_domain_ip/1,
    ip_to_str/1,
    ip2hostlong/1,
    get_intranet_address/0,
    get_all_bind_address/0,
    get_tx_ip_address/0
]).

-export([
    to_integer/1,
    to_binary/1,
    to_tuple/1,
    to_float/1,
    to_list/1,
    to_atom/1,
    to_timestamp/1,
    list_to_atom/1,
    is_string/1,
    proplists_to_integer/2,
    float_to_str/1,
    md5/1,
    to_port/1,
    to_port/2,
    to_ref/3,
    to_ref/4
]).

-export([
    ceil/1,
    floor/1,
    magnify/1,
    random/1,
    random/2,
    random_seed/1,
    random_seed/3,
    rand_bytes/1,
    random_in_range/2,
    random_list/3,
    random_list/4 %% random_list(Sum,N,Min,Max), 把Sum分成随机大小的N份，每份最小min，最大Max
]).

-export([
    concat/1,
    page/3,
    list_element_index/2,
    combine_lists/2,
    random_element_from_list/1,
    random_elements_from_list/2,
    random_elements/2,
    random_elements_expand/2,
    random_reorder_list/1,
    has_duplicate_member/1,
    repeated_element_in_lists/2,
    list_filter_repeat/1,
    list_filter_repeat2/1,
    list_filter_repeat2/2,
    flatten_format/2,
    flatten_format_list/2,
    list_to_hex/1,
    binary_to_hex/1
]).

-export([
    utf8_len/1,
    sublist_utf8/3,
    code_point_len/1,
    trim/2
]).

-export([
    keymax/2,
    keymaxs/2,
    keymin/2,
    keymins/2,
    merge/1,
    merge/2,
    merge/4,
    update_merge/1,
    update_merge/2,
    update_merge/3,
    transpose/1,
    is_less/2,
    is_greater/2,
    is_less_or_equal/2,
    is_greater_or_equal/2,
    use_random_seed_do/2,
    list_replace/3
]).

-export([
    test/2,
    test_randoms/0
]).

test(N, Max) ->
    Min = 1,
    test2(N, Min, Max, []).
test2(N, Min, Max, Acc) when N>0 ->
    T = mlib_tool:random(Min, Max),
    {Count, NewAcc} = lists:foldl(fun({Tmp1, Tmp}, {Acc2, Acc3}) -> ?IF( (Tmp1-Tmp)=<T, {Acc2+1, [{Tmp1, Tmp}|Acc3]}, {Acc2, [{Tmp1, Tmp+1}|Acc3]}) end, {0, []}, Acc),
%%    ?INFO_MSG("~w", [{Acc, T, Count}]),
    test2(N-1, Min, Max-1, [{T+Count, Count}|NewAcc]);
test2(_N, _Min, _Max, Acc) ->
    [ T || {T, _}<- Acc].


%%%%%%%%%######IP relative######%%%%%%%%%
%% @doc Socket的IP地址字符串
-spec ip(port()) -> binary().
ip(Socket) ->
    {ok,{{Ip0,Ip1,Ip2,Ip3},_}} = inet:peername(Socket),
    list_to_binary(integer_to_list(Ip0)++"."++integer_to_list(Ip1)++"."++integer_to_list(Ip2)++"."++integer_to_list(Ip3)).

%% @doc 获取域名的IP
-spec get_domain_ip(atom() | string()) -> {ok, string()} | {error, term()}.
get_domain_ip(Name) ->
    case inet:gethostbyname(Name) of
        {ok, {_, _, _, _, _, [IPTuple]}} ->
            {ok, ?MODULE:ip_to_str(IPTuple)};
        {error, Error} ->
            {error, Error}
    end.

%% @doc IP(tuple) -> String
-spec ip_to_str(tuple()) -> string().
ip_to_str(IP) ->
    case IP of
        {A, B, C, D} ->
            lists:concat([A, ".", B, ".", C, ".", D]);
        {A, B, C, D, E, F, G, H} ->
            lists:concat([A, ":", B, ":", C, ":", D, ":", E, ":", F, ":", G, ":", H]);
        Str when is_list(Str) ->
            Str;
        _ ->
            []
    end.

%% @doc IPv4地址的32b编码值
%% @param IPStr := "A.B.C.D"
%% @returns IPInt(32bit): AABBCCDD
-spec ip2hostlong(string()) -> integer().
ip2hostlong(IPStr) ->
    [A,B,C,D] = string:tokens(IPStr,"."),
    (?MODULE:to_integer(A) bsl 24) + (?MODULE:to_integer(B) bsl 16) + (?MODULE:to_integer(C) bsl 8) + ?MODULE:to_integer(D).

%% @doc 获得内网IP地址, 只是适合内部的服务器
-spec get_intranet_address() -> [nonempty_string()].
get_intranet_address() ->
    Result = os:cmd("ifconfig -a | grep 'inet ' | egrep '192.168.|10.10' | awk '{print $2}' | cut -d ':' -f 2 | grep -v '^127'"),
    string:tokens(Result, "\n").

%% @doc 获得所有绑定的IP地址
-spec get_all_bind_address() -> [nonempty_string()].
get_all_bind_address() ->
    Result = os:cmd("ifconfig -a | grep 'inet ' | awk '{print $2}' | cut -d ':' -f 2 | grep -v '^127'"),
    string:tokens(Result, "\n").

%% @doc 获得腾讯环境的机器的IP地址
%% @returns IPInt(32bit): AABBCCDD
-spec get_tx_ip_address() -> integer().
get_tx_ip_address() ->
    Result =  os:cmd("ifconfig -a eth1 | grep 'inet ' | awk '{print $2}' | cut -d ':' -f 2"),
    [IPStr] = string:tokens(Result, "\n"),
    ip2hostlong(IPStr).

%%%%%%%%%######data type relative######%%%%%%%%%
%% @doc convert other type to integer
%% @throws other_value
-spec to_integer(integer() | binary() | list() | float() | term()) -> integer().
to_integer(Msg) when erlang:is_integer(Msg) ->
    Msg;
to_integer(Msg) when erlang:is_binary(Msg) ->
    Msg2 = erlang:binary_to_list(Msg),
    erlang:list_to_integer(Msg2);
to_integer(Msg) when erlang:is_list(Msg) ->
    erlang:list_to_integer(Msg);
to_integer(Msg) when erlang:is_float(Msg) ->
    erlang:round(Msg);
to_integer(_Msg) ->
    erlang:throw(other_value).

%% @doc convert other type to binary
%% @throws other_value
-spec to_binary(any()) -> binary().
to_binary(Msg) when erlang:is_binary(Msg) ->
    Msg;
to_binary(Msg) when erlang:is_atom(Msg) ->
    erlang:list_to_binary(erlang:atom_to_list(Msg));
%%atom_to_binary(Msg, utf8);
to_binary(Msg) when erlang:is_list(Msg) ->
    erlang:list_to_binary(Msg);
to_binary(Msg) when erlang:is_integer(Msg) ->
    erlang:list_to_binary(integer_to_list(Msg));
to_binary(Msg) when erlang:is_float(Msg) ->
    erlang:list_to_binary(float_to_str(Msg)).
%%to_binary(_Msg) ->
%%    erlang:throw({other_value, {_Msg, mlib_sys:get_stacktrace()}}).

%% @doc convert other type to tuple
-spec to_tuple(tuple() | term()) -> tuple().
to_tuple(T) when erlang:is_tuple(T) -> T;
to_tuple(T) -> {T}.

%% @doc convert other type to float
-spec to_float(term()) -> float().
to_float(Msg)->
    Msg2 = to_list(Msg),
    erlang:list_to_float(Msg2).

%% @doc convert other type to list
%% @throws other_value
-spec to_list(list() | atom() | binary() | tuple() | integer() | float() | term() | pid()) -> list().
to_list(Msg) when erlang:is_list(Msg) ->
    Msg;
to_list(Msg) when erlang:is_atom(Msg) ->
    erlang:atom_to_list(Msg);
to_list(Msg) when erlang:is_binary(Msg) ->
    erlang:binary_to_list(Msg);
to_list(Msg) when erlang:is_tuple(Msg) ->
    erlang:tuple_to_list(Msg);
to_list(Msg) when erlang:is_integer(Msg) ->
    erlang:integer_to_list(Msg);
to_list(Msg) when erlang:is_float(Msg) ->
    float_to_str(Msg);
to_list(PID) when erlang:is_pid(PID) ->
    pid_to_str(PID);
to_list(_) ->
    erlang:throw(other_value).

%% @doc convert other type to atom
%% @throws other_value
-spec to_atom(atom() | binary() | list() | integer() | term()) -> atom().
to_atom(Msg) when erlang:is_atom(Msg) ->
    Msg;
to_atom(Msg) when erlang:is_binary(Msg) ->
    ?MODULE:list_to_atom(erlang:binary_to_list(Msg));
to_atom(Msg) when erlang:is_list(Msg) ->
    ?MODULE:list_to_atom(Msg);
to_atom(Msg) when erlang:is_integer(Msg) ->
    ?MODULE:list_to_atom(erlang:integer_to_list(Msg));
to_atom(_) ->
    erlang:throw(other_value).  %%list_to_atom("").

%% {ok, ?TIME_MATCH_RE} = re:compile("\(\\d{4,4}\)[-/]\(\\d{1,2}\)[-/]\(\\d{1,2}\)[\\s]+\(\\d{1,2}\)[-:]\(\\d{1,2}\)[-:]\(\\d{1,2}\)").
-define(TIME_MATCH_RE, {re_pattern,6,0,0,<<69,82,67,80,58,1,0,0,0,0,0,0,1,0,0,0,255,255,255,255,255,255,255,255,0,0,0,0,0,0,6,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,125,0,246,127,0,9,0,1,93,0,4,7,114,0,9,106,0,0,0,0,0,160,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,127,0,10,0,2,7,91,0,1,7,114,0,10,106,0,0,0,0,0,160,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,127,0,10,0,3,7,91,0,1,7,114,0,10,106,0,54,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100,127,0,10,0,4,7,91,0,1,7,114,0,10,106,0,0,0,0,0,32,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,127,0,10,0,5,7,91,0,1,7,114,0,10,106,0,0,0,0,0,32,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,127,0,10,0,6,7,91,0,1,7,114,0,10,114,0,246,0>>}).
to_timestamp(T) when erlang:is_tuple(T) ->
    mtime:timestamp(T);
to_timestamp(T) when erlang:is_integer(T) ->
    T;
to_timestamp(T) ->
    case catch mlib_tool:to_integer(T) of
        I when erlang:is_integer(I) -> I;
        _ ->
            case re:split(T, ?TIME_MATCH_RE, [trim]) of
                [_, Year, Mon, Day, Hour, Min, Sec|_] ->
                    to_timestamp({{mlib_tool:to_integer(Year), mlib_tool:to_integer(Mon), mlib_tool:to_integer(Day)},
                        {mlib_tool:to_integer(Hour), mlib_tool:to_integer(Min), mlib_tool:to_integer(Sec)}});
                [_] ->
                    erlang:throw({error, T});
                _Err ->
                    erlang:throw({error, {T, _Err}})
            end
    end.




binary_to_hex(Bin) when is_binary(Bin) ->
    binary_to_hex(Bin, []).
binary_to_hex(<<>>, Acc) -> lists:reverse(Acc);
binary_to_hex(<<Byte, Bin/binary>>, Acc) ->
    [C1, C2] = int_to_hex(Byte),
    binary_to_hex(Bin, [C2, C1|Acc]).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

%% @doc 单个字节的16进制字符串表达
-spec int_to_hex(integer()) -> string().
int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a+(N-10).

%% @doc list_to_atom
-spec list_to_atom(list()) -> atom().
list_to_atom(List) when is_list(List) ->
    case catch(erlang:list_to_existing_atom(List)) of
        {'EXIT', _} -> erlang:list_to_atom(List);
        Atom when is_atom(Atom) -> Atom
    end.

%% @doc 是否是string
-spec is_string(term()) -> true | false.
is_string(List) ->
    case catch erlang:list_to_binary(List) of
        {'EXIT', _} ->
            false;
        _Binary ->
            true
    end.

%% @doc convert proplists to integer
-spec proplists_to_integer(term(), list()) -> integer().
proplists_to_integer(Key, List) ->
    to_integer(proplists:get_value(Key, List)).

%% @doc convert float to string: 1.5678->1.57
-spec float_to_str(integer() | float()) -> string().
float_to_str(N) when erlang:is_integer(N) ->
    integer_to_list(N) ++ ".00";
float_to_str(F) when erlang:is_float(F) ->
    [A] = io_lib:format("~.2f", [F]),
    A.

pid_to_str(PID) ->
    erlang:pid_to_list(PID).


%% @doc 构造port对象
to_port(N) when is_integer(N), N > 0 ->
    to_port(N, node()).
to_port(N, NodeName) when is_integer(N), N > 0, is_atom(NodeName) ->
    %% We rebuild the term from the int received:
    %% http://www.erlang.org/doc/apps/erts/erl_ext_dist.html#id86892
    Name = iolist_to_binary(atom_to_list(NodeName)),
    NameLen = iolist_size(Name),
    Vsn = binary:last(term_to_binary(self())),
    Bin = <<131, % term encoding value
        102, % tag
        100, % atom ext tag, used for node name
        NameLen:2/unit:8,
        Name:NameLen/binary,
        N:4/unit:8, % actual counter value
        Vsn:8 % creation
    >>,
    binary_to_term(Bin).


%% @doc 构造ref对象
to_ref(A, B, C) when is_integer(A), A >= 0,
    is_integer(B), C >= 0,
    is_integer(C), C >= 0 ->
    to_ref(A, B, C, node()).
to_ref(A, B, C, NodeName) when is_integer(A), A >= 0,
    is_integer(B), C >= 0,
    is_integer(C), C >= 0,
    is_atom(NodeName) ->
    Name = iolist_to_binary(atom_to_list(NodeName)),
    NameLen = iolist_size(Name),
    Vsn = binary:last(term_to_binary(self())),
    Bin = <<131, % term encoding value
        114, % tag
        0, 3,
        100, % atom ext tag, used for node name
        NameLen:2/unit:8,
        Name:NameLen/binary,
        Vsn:8, % creation
        C:4/unit:8,
        B:4/unit:8,
        A:4/unit:8>>,
    binary_to_term(Bin).


%% @doc md5
-spec md5(term()) -> string().
md5(S) ->
    Md5_bin =  erlang:md5(?MODULE:to_list(S)),
    lists:flatten(list_to_hex(binary_to_list(Md5_bin))).

%% @doc get the minimum number that is bigger than X
-spec ceil(number()) -> integer().
ceil(X) ->
    T = erlang:trunc(X),
    if X==T -> T;
        X>0 -> T+1;
        true -> T
    end.

%% @doc get the maximum number that is smaller than X
-spec floor(number()) -> integer().
floor(X) ->
    T = erlang:trunc(X),
    if X==T -> T;
        X>0 -> T;
        true -> T-1
    end.

-spec magnify(number()) -> integer().
magnify(X) when X>0 ->
    mlib_tool:ceil(X);
magnify(X) ->
    mlib_tool:floor(X).

%% random module is deprecated since releases 19 (ERTS >= 8.0)
%% R19的优化 大于等于8.0的版本处理
random(Max)->
    rand:uniform(Max).
random_seed(_Seed)->
    ignore.
rand_bytes(Len)->
    crypto:strong_rand_bytes(Len).

%% @doc get a random integer between Min and Max
-spec random(integer(), integer()) -> integer().
random(Min, Max)->
    Min2 = Min-1,
    random(Max-Min2)+Min2.


random_seed(A, B, C) ->
    random_seed({A, B, C}).

%% @doc 获取元素在列表中的index
-spec list_element_index(term(), list()) -> integer().
list_element_index(K, List) ->
    list_element_index(K, List, 1).
list_element_index(_, [], _) ->
    0;
list_element_index(K, [K|_], Index) ->
    Index;
list_element_index(K, [_|T], Index) ->
    list_element_index(K, T, Index+1).

%% @doc 合并2个列表, 过滤重复的
-spec combine_lists(list(), list()) -> list().
combine_lists(L1, L2) ->
    case erlang:length(L1) > erlang:length(L2) of
        true ->
            combine_lists1(L2, L1);
        _ ->
            combine_lists1(L1, L2)
    end.
combine_lists1([], ListB) ->
    ListB;
combine_lists1([A1|TListA], ListB) ->
    case lists:member(A1, ListB) of
        true -> combine_lists1(TListA, ListB);
        _ -> combine_lists1(TListA, [A1|ListB])
    end.

%% @doc 从一个List中随机取出N个元素
%% @end
-spec random_elements_from_list(pos_integer(), list()) -> {ok,list()}.
random_elements_from_list(N, List) when N > 0 ->
    case N=<200 andalso erlang:length(List)>= N*4 of
        true ->
            random_elements(N, List);
        _ ->
            random_elements_expand(N, List)
    end.

%% @doc 从一个List中随机取出N个元素，运算时间取决于List的大小，所以N ≈ length(List)可用，两者越接近越好
%% @end
random_elements_expand(N, List) when N > 0 ->
    TList = [{random(10000), I} || I <- List],
    SortTList = lists:sort(TList),
    TList2 = lists:sublist(SortTList, N),
    {ok, [I || {_Rand, I} <- TList2]}.
%% @doc 从一个List中随机取出N个元素,运算时间取决于N的大小，所以N << length(List) 时使用，实际使用中，N=<200 可用
%% @end
random_elements(N, List) when N > 0 ->
    IL = random_in_range(erlang:length(List), N),
    {_, IL2} = lists:foldl(fun(I, {Index, Acc})-> {Index+1, [{I, Index}|Acc]} end, {1, []}, IL),
    IL3 = lists:keysort(1, IL2),
    ResList = random_elements2(IL3, List, 1, []),
    {ok, [ R || {R, _}<- lists:keysort(2, ResList)]}.
random_elements2([{I, Index}|IL], [T|List], I, Acc) ->
    random_elements2(IL, List, I+1, [{T, Index}|Acc]);
random_elements2([_|_]=IL, [_T|List], C, Acc) ->
    random_elements2(IL, List, C+1, Acc);
random_elements2([], _List, _C, Acc) ->
    Acc.

%% @doc 随机取一个元素
%% @throws badarg
-spec random_element_from_list([term()]) -> term().
random_element_from_list([]) ->
    throw(badarg);
random_element_from_list(Inlist) ->
    Index  = random(length(Inlist)),
    lists:nth(Index, Inlist).

%% @doc 随机打乱List顺序
-spec random_reorder_list(list()) -> list().
random_reorder_list(List) ->
    List1 = [{random(10000),X} ||X <-List ],
    List2 = lists:keysort(1,List1),
    [E || {_,E} <-List2].

random_list(Sum, N, Min) ->
    random_list(Sum, N, Min, Sum).
random_list(Sum, N, Min, Max) when Max>Min andalso N>0 andalso Sum>N ->
    %% Average = Sum div N,
    %% true = (Max >= Average), true = (Average>=Min),
    %% Offset = erlang:max(Average-Min, Max-Average),
    %% RMin = erlang:max(Average-Offset, 0), RMax = erlang:min(Average+Offset, Sum),
    RMin = Min, RMax = Max,
    {LessNum, FullNum, OtherNum, RandomList, Rem} =
        lists:foldl(fun(_Index, {AccLess, AccFull, AccOther, AccAll, AccRem})->
            T = random(RMin, RMax),
            case T of
                RMin -> {AccLess+1,  AccFull, AccOther, [T|AccAll], AccRem-T};
                RMax -> {AccLess, AccFull+1, AccOther, [T|AccAll], AccRem-T};
                _ -> {AccLess, AccFull, AccOther+1, [T|AccAll], AccRem-T}
            end
        end, {0,0,0,[],Sum}, lists:seq(1, N)),
    random_list2(Rem, LessNum, FullNum, OtherNum, RandomList, RMin, RMax).
random_list2(0, _LessNum, _FullNum, _OtherNum, RandomList, _RMin, _RMax) ->
    RandomList;
random_list2(Rem, LessNum, FullNum, OtherNum, RandomList, RMin, RMax) ->
    {Func1, Func2, Length} =
        case Rem>0 of
            true -> {ceil, min, LessNum+OtherNum};
            false -> {floor, max, FullNum+OtherNum}
        end,
    Add = mlib_tool:Func1(Rem / Length),
    {NewLessNum, NewFullNum, NewOtherNum, NewRandomList, NewRem} =
        lists:foldl(fun(T0, {AccLess, AccFull, AccOther, AccAll, AccRem})->
            Add2 = erlang:Func2(AccRem, Add),
            T = T0+ Add2,
            if
                T=<RMin -> {AccLess+1, AccFull, AccOther, [RMin|AccAll], AccRem-(RMin-T0)};
                T>=RMax -> {AccLess, AccFull+1, AccOther, [RMax|AccAll], AccRem-(RMax-T0)};
                true -> {AccLess, AccFull, AccOther+1, [T|AccAll], AccRem-Add2}
            end
        end, {0,0,0,[], Rem}, RandomList),
    case NewRem=:=0 of
        true -> NewRandomList;
        false -> random_list2(NewRem, NewLessNum, NewFullNum, NewOtherNum, NewRandomList, RMin, RMax)
    end.

%% @doc 检查列表中是否有重复的元素
-spec has_duplicate_member(list()) -> true | false.
has_duplicate_member([]) ->
    false;
has_duplicate_member([E|L]) ->
    case lists:member(E, L) of
        true ->
            true;
        _ ->
            has_duplicate_member(L)
    end.

%% @doc 检查列表中是否存在重复元素
-spec repeated_element_in_lists(list(), list()) -> true | false.
repeated_element_in_lists(List1, List2) ->
    case erlang:length(List1) > erlang:length(List2) of
        true ->
            repeated_element_in_lists1(List2, List1);
        _ ->
            repeated_element_in_lists1(List1, List2)
    end.
repeated_element_in_lists1([], _ListB) ->
    false;
repeated_element_in_lists1(_ListA, []) ->
    false;
repeated_element_in_lists1([A1|TListA], ListB) ->
    case lists:member(A1, ListB) of
        true -> true;
        _ -> repeated_element_in_lists1(TListA, ListB)
    end.

%% @doc List去重, 顺序不乱
-spec list_filter_repeat(list()) -> list().
list_filter_repeat(List) ->
    lists:reverse(lists:foldl(fun(Elem,Acc) ->
        case lists:member(Elem,Acc) of
            true ->
                Acc;
            false ->
                [Elem|Acc]
        end
    end, [], List)).

%% @doc List去重
%%      可以替换第一种实现，数据越多效率越明显; 合并过程内存开销是前面一种的1~4倍，以空间换时间，合并10万条数据比尾递归多1~2M内存使用，但是使用后内存会被gc
%%      结果是排好序的，从小到大
-spec list_filter_repeat2(list()) -> list().
list_filter_repeat2(List) ->
    gb_sets:to_list(gb_sets:from_list(List)).

%% @doc List合并去重
%%      合并List、List2，去重；前面写了尾递归方式的合并去重，无论哪种情况，效率没这个高
%%      结果是排好序的，从小到大
-spec list_filter_repeat2(list(), list()) -> list().
list_filter_repeat2(List1, List2) ->
    gb_sets:to_list(gb_sets:union(gb_sets:from_list(List1), gb_sets:from_list(List2))).

%% @doc List中的元素做字符串衔接
-spec concat(list()) -> list().
concat(List) ->
    List2 = [?MODULE:to_list(E) || E <- List],
    lists:concat(List2).

%% @doc 从一个list中取一段(下标From到To之间)
%% @returns [Item]
page(List, From, To) when is_integer(From), is_integer(To), To >= 0, From =< To ->
    page(List, 1, From, To, []).
page([_|List], Nth, From, To, Acc) when Nth < From ->
    page(List, Nth + 1, From, To, Acc);
page([Item|List], Nth, From, To, Acc) when Nth =< To ->
    page(List, Nth + 1, From, To, [Item|Acc]);
page(_, Nth, _, To, Acc) when Nth > To -> lists:reverse(Acc);
page([], _, _, _, Acc) -> lists:reverse(Acc).

%% @doc 格式化字符串
-spec flatten_format(list() | binary(), list() | atom() | binary() | tuple() | integer() | float() | term() | pid()) -> list().
flatten_format(LangResources,ParamList) when erlang:is_list(ParamList)->
    lists:flatten(io_lib:format(LangResources,[?MODULE:to_list(PR)|| PR <- ParamList]));
flatten_format(LangResources,Param) ->
    lists:flatten(io_lib:format(LangResources,[?MODULE:to_list(Param)])).

flatten_format_list(LangResources,ParamList) when erlang:is_list(ParamList)->
    lists:flatten([io_lib:format(LangResources, [PR]) || PR <- ParamList]).

%% @doc get utf8 len
-spec utf8_len(list() | binary()) -> integer().
utf8_len(List) when erlang:is_list(List) ->
    len(List, 0);
utf8_len(Binary) when erlang:is_binary(Binary) ->
    len(erlang:binary_to_list(Binary), 0).


len([], N) ->
    N;
len([A, _, _, _, _, _ | T], N) when A =:= 252 orelse A =:= 253 ->
    len(T, N+1);
len([A, _, _, _, _ | T], N) when A >=248 andalso A =< 251 ->
    len(T, N+1);
len([A, _, _, _ |T], N) when A >= 240 andalso A =< 247 ->
    len(T, N+1);
len([A, _, _ | T], N) when A >= 224 ->
    len(T, N+1);
len([A, _ | T], N) when A >= 192 ->
    len(T, N+1);
len([_A | T], N) ->
    len(T, N+1).

%% @doc get utf8 len(sublist)
-spec sublist_utf8(list() | binary(), integer(), integer()) -> string().
sublist_utf8(List, Start, Length) when erlang:is_list(List) ->
    sublist_utf8_2(List, Start, Start + Length - 1, 0, []);
sublist_utf8(Binary, Start, Length) when erlang:is_binary(Binary) ->
    sublist_utf8_2(erlang:binary_to_list(Binary), Start, Start + Length - 1, 0, []).

sublist_utf8_2(List, Start, End, Cur, Result) ->
    if Cur =:= End ->
        lists:reverse(Result);
        true ->
            sublist_utf8_3(List, Start, End, Cur, Result)
    end.

sublist_utf8_3([], _Start, _End, _Cur, Result) ->
    lists:reverse(Result);
sublist_utf8_3([A, A2, A3, A4, A5, A6 | T], Start, End, Cur, Result) when A =:= 252 orelse A =:= 253 ->
    if Cur + 1 >= Start ->
        Result2 = [A6, A5, A4, A3, A2, A | Result];
        true ->
            Result2 = Result
    end,
    sublist_utf8_2(T, Start, End, Cur+1, Result2);
sublist_utf8_3([A, A2, A3, A4, A5 | T], Start, End, Cur, Result) when A >= 248 andalso A =< 251 ->
    if Cur + 1 >= Start ->
        Result2 = [A5, A4, A3, A2, A | Result];
        true ->
            Result2 = Result
    end,
    sublist_utf8_2(T, Start, End, Cur+1, Result2);
sublist_utf8_3([A, A2, A3, A4 | T], Start, End, Cur, Result) when A >= 240 andalso A =< 247 ->
    if Cur + 1 >= Start ->
        Result2 = [A4, A3, A2, A | Result];
        true ->
            Result2 = Result
    end,
    sublist_utf8_2(T, Start, End, Cur+1, Result2);
sublist_utf8_3([A, A2, A3 | T], Start, End, Cur, Result) when A >= 224 ->
    if Cur + 1 >= Start ->
        Result2 = [A3, A2, A | Result];
        true ->
            Result2 = Result
    end,
    sublist_utf8_2(T, Start, End, Cur+1, Result2);
sublist_utf8_3([A, A2 | T], Start, End, Cur, Result) when A >= 192 ->
    if Cur + 1 >= Start ->
        Result2 = [A2, A | Result];
        true ->
            Result2 = Result
    end,
    sublist_utf8_2(T, Start, End, Cur+1, Result2);
sublist_utf8_3([A | T], Start, End, Cur, Result) ->
    if Cur + 1 >= Start ->
        Result2 = [A | Result];
        true ->
            Result2 = Result
    end,
    sublist_utf8_2(T, Start, End, Cur+1, Result2).

keymax(Index, [T1|_]=List) ->
    keymax(Index, List, T1).
keymax(Index, [T|List], Max) ->
    case erlang:element(Index, T)> erlang:element(Index, Max) of
        true -> keymax(Index, List, T);
        false -> keymax(Index, List, Max)
    end;
keymax(_Index, [], Max) ->
    Max.

keymaxs(_Index, []) ->
    [];
keymaxs(Index, [T1|List]) ->
    keymaxs(Index, List, T1, [T1]).
keymaxs(Index, [T|List], Max, Maxs) ->
    TV = erlang:element(Index, T),
    MaxV = erlang:element(Index, Max),
    if
        TV > MaxV ->
            keymaxs(Index, List, T, [T]);
        TV < MaxV ->
            keymaxs(Index, List, Max, Maxs);
        true ->
            keymaxs(Index, List, Max, [T|Maxs])
    end;
keymaxs(_Index, [], _Max, Maxs) ->
    lists:reverse(Maxs).

keymin(Index, [T1|_]=List) ->
    keymin(Index, List, T1).
keymin(Index, [T|List], Min) ->
    case erlang:element(Index, T)< erlang:element(Index, Min) of
        true -> keymin(Index, List, T);
        false -> keymin(Index, List, Min)
    end;
keymin(_Index, [], Min) ->
    Min.

keymins(_Index, []) ->
    [];
keymins(Index, [T1|List]) ->
    keymins(Index, List, T1, [T1]).
keymins(Index, [T|List], Min, Mins) ->
    TV = erlang:element(Index, T),
    MinV = erlang:element(Index, Min),
    if
        TV < MinV ->
            keymins(Index, List, T, [T]);
        TV > MinV ->
            keymins(Index, List, Min, Mins);
        true ->
            keymins(Index, List, Min, [T|Mins])
    end;
keymins(_Index, [], _Min, Mins) ->
    lists:reverse(Mins).
%% 合并 KVList :: [{K :: any(), V :: number()| KVList }] 结构
merge(List) ->
    merge([], List).
merge(BaseList, List) ->
    merge(1, 2, BaseList, List).
merge(KeyIndex, ValIndex, BaseList, [Tuple|AddList]) when erlang:is_tuple(Tuple) ->
    K = erlang:element(KeyIndex, Tuple),
    V = erlang:element(ValIndex, Tuple),
    NewBaseList =
        case lists:keyfind(K, KeyIndex, BaseList) of
            OldTuple when erlang:is_tuple(OldTuple) ->
                OldV = erlang:element(ValIndex, OldTuple),
                if
                    erlang:is_number(OldV) andalso erlang:is_number(V) ->
                        lists:keyreplace(K, 1, BaseList, erlang:setelement(ValIndex, OldTuple, OldV+V));
                    erlang:is_list(OldV) andalso erlang:is_list(V) ->
                        lists:keyreplace(K, 1, BaseList, erlang:setelement(ValIndex, OldTuple, merge(OldV, V)));
                    true ->
                        erlang:throw({error, {Tuple, AddList}})
                end;
            false ->
                [Tuple|BaseList]
        end,
    merge(KeyIndex, ValIndex, NewBaseList, AddList);
merge(_KeyIndex, _ValIndex, BaseList, []) ->
    BaseList.
update_merge(List) ->
    update_merge([], List).
update_merge(BaseList, AddList) ->
    update_merge(1, BaseList, AddList).
update_merge(KeyIndex, BaseList, [Tuple|AddList]) ->
    NewBaseList = lists:keystore(erlang:element(KeyIndex, Tuple), KeyIndex, BaseList, Tuple),
    update_merge(KeyIndex, NewBaseList, AddList);
update_merge(_KeyIndex, BaseList, []) ->
    BaseList.
%% [[1,2],[3,4],[5,6]] => [[1,3,5], [2,4,6]]
transpose([[_|_]|_]=[HeadLine|OtherLines]) ->
    HeadLine2 =  [ [T] || T<-HeadLine],
    List =
        lists:foldl(fun(Tail, Acc)->
            transpose2(Tail, Acc)
        end, HeadLine2, OtherLines),
    [ lists:reverse(Line) || Line <- List].

transpose2([H|Heads], [Line|Lines]) ->
    [[H|Line] | transpose2(Heads, Lines)];
transpose2([], []) ->
    [].

is_greater(Vsn1, Vsn2) -> compare_version(Vsn1, Vsn2) == greater.
is_less(Vsn1, Vsn2) -> compare_version(Vsn1, Vsn2) == less.
is_greater_or_equal(Vsn1, Vsn2) -> not is_less(Vsn1, Vsn2).
is_less_or_equal(Vsn1, Vsn2) -> not is_greater(Vsn1, Vsn2).

compare_version(Vsn, Vsn) ->
    equal;
compare_version(Vsn1, Vsn2) ->
    compare_version1(string:tokens(Vsn1, "."), string:tokens(Vsn2, ".")).

compare_version1([], []) ->
    equal;
compare_version1([_X], []) ->
    greater;
compare_version1([], [_X]) ->
    less;
compare_version1([X|Rest1], [X|Rest2]) ->
    compare_version1(Rest1, Rest2);
compare_version1([X1], [X2]) ->
    %% For last digit ignore everything after the "-", if any
    Y1 = lists:takewhile(fun(X) -> X /= $- end, X1),
    Y2 = lists:takewhile(fun(X) -> X /= $- end, X2),
    compare_digit(Y1, Y2);
compare_version1([X1|Rest1], [X2|Rest2]) ->
    case compare_digit(X1, X2) of
        equal -> compare_version1(Rest1, Rest2);
        Else  -> Else
    end.

compare_digit(X, X) ->
    equal;
compare_digit(X1, X2) when length(X1) > length(X2) ->
    greater;
compare_digit(X1, X2) when length(X1) < length(X2) ->
    less;
compare_digit(X1, X2) ->
    case X1 > X2 of
        true  -> greater;
        false -> less
    end.

%% @doc  unicode代码点长度，一个字符对应一个代码点。
-spec code_point_len(list() | binary()) -> integer().
code_point_len(List) ->
    ListBin = mlib_tool:to_binary(List),
    erlang:length(unicode:characters_to_list(ListBin)).

%% @doc 去掉头部或尾部或两侧的空白字符
trim(String0, leading) ->
    RE = "^[\\s\\p{C}\\p{Z}]+",
    trim(String0, RE);
trim(String0, trailing) ->
    RE = "[\\s\\p{C}\\p{Z}]+$",
    trim(String0, RE);
trim(String0, both) ->
    RE = "^[\\s\\p{C}\\p{Z}]+|[\\s\\p{C}\\p{Z}]+$",
    trim(String0, RE);
trim(String0, RegEx) ->
    String = unicode:characters_to_list(mlib_tool:to_binary(String0)),
    case catch re:replace(String, RegEx, "", [unicode, global, {return, binary}]) of
        StringBin when is_binary(StringBin) ->
            mlib_tool:to_list(StringBin);
        Error ->
            {error, Error}
    end.

%% @doc 在[1, Range]中随机出Num个不相同的整数
%% @returns [Number]
random_in_range(Range, Num) ->
    random_in_range(Num, Range, 0, [], []).
random_in_range(0, _, _, _, Acc) -> Acc;
random_in_range(Need, Max, Has, PosAcc, Res) ->
    Pos = rand:uniform(Max - Has),
    {Val, PosAcc2} =
        lists:foldl(fun({P, V0}, {V, L}) when P =< Pos ->
            {V + 1, [{P, V0}|L]};
            ({P, V0}, {V, L}) ->
                {V, [{P - 1, V0}|L]}
        end, {Pos, []}, PosAcc),
    random_in_range(Need - 1, Max, Has + 1, [{Pos, Val}|PosAcc2], [Val|Res]).


-ifdef(TEST).
random_elements_from_list_test_() ->
    L1 = [],
    L2 = [3, 2, 4],
    [?_assertEqual([], random_elements_from_list(2, L1)),
        ?_assertEqual(2, erlang:length(lists:sort(random_elements_from_list(2, L2)))),
        ?_assertEqual([2, 3, 4], lists:sort(random_elements_from_list(3, L2))),
        ?_assertEqual([2, 3, 4], lists:sort(random_elements_from_list(4, L2))),
        ?_assertError(function_clause, random_elements_from_list(0, L2))].

-endif.


test_randoms() ->
    GenList = [{A, C}||A<-[1, 5, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000], C<-[100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000, 300000], A=<C],
    [begin
        L = lists:seq(1, C),
        erlang:garbage_collect(self()),
        {T1, _} = timer:tc(fun()->mlib_tool:random_elements(A, L), ok end),
        erlang:garbage_collect(self()),
        {T2, _} = timer:tc(fun()->mlib_tool:random_elements_expand(A, L), ok end),
        erlang:garbage_collect(self()),
        {T3, _} = timer:tc(fun()->mlib_tool:random_elements(A, L), ok end),
        erlang:garbage_collect(self()),
        {T4, _} = timer:tc(fun()->mlib_tool:random_elements_expand(A, L), ok end),
        erlang:garbage_collect(self()),
        {T5, _} = timer:tc(fun()->mlib_tool:random_elements(A, L), ok end),
        erlang:garbage_collect(self()),
        {T6, _} = timer:tc(fun()->mlib_tool:random_elements_expand(A, L), ok end),
        io:format("~w~n", [{{A, C}, {T1,T3,T5}, {T2,T4,T6}, (lists:sum([T1,T3,T5])/3) / (lists:sum([T2,T4,T6])/3)}]),
        {{A, C}, {T1,T3,T5}, {T2,T4,T6}, A*A, (lists:sum([T1,T3,T5])/3) / (lists:sum([T2,T4,T6])/3)}
    end|| {A, C}<- GenList].

%% @doc 设某个随机种子做事情  结束之后还原随机种子
use_random_seed_do({_, _, _} = Seed, Fun)->
    OldState = rand:export_seed(),
    rand:seed(exs64, Seed),
    Res = Fun(),
    erlang:erase(rand_seed),
    case OldState of
        undefined -> ignore;
        _ -> rand:seed(OldState)
    end,
    Res.

%% 小list
list_replace(Index, [_|List], New) when Index =:= 1 ->
    [New|List];
list_replace(Index, [T|List], New) when Index > 1 ->
    [T|list_replace(Index-1, List, New)].
