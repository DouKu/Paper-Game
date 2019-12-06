%%%-------------------------------------------------------------------
%%% @author mingchao
%%% @copyright (C) 2010, mingchao.com
%%% @doc
%%%     mgee config 的动态加载实现版本
%%%     目前只支持key-value或者record（首字段为key）的配置文件
%%% @end
%%% Created : 2010-12-2
%%%-------------------------------------------------------------------
-module(mconf_dyn).

-include("dkge.hrl").
-include_lib("kernel/include/file.hrl").


-export([
    get_version/0,
    get_config_path/0
]).

%% API
-export([
    gen_all_beam/1,
    gen_all_erl/2,
    gen_exist_erl/2,
    gen_all_beam_par/1,
    gen_changed_beam/1,
    gen_changed_beam_par/1
]).

-export([
    init/0,
    reload/1,
    reload_setting/0,
    reload_all/0
]).

-export([
    find/2,
    list/1
]).

-export([
    read_config/1,
    write_config/2,
    get_all_config/0,
    get_config_path/1,
    get_setting_path/0,
    gen_src_code/4,
    do_load_gen_src/3,
    change_config/3,
    remove_config/2
]).


-define(MAX_CONFIG_FILE_SIZE, 1024*1024). %%不接受超过1M的配置文件, 文件过大会导致编译开销巨大


%% @doc 修改配置文件, 注意不保留文件里的注释
%% @returns ok | error
change_config(ModName, Key, Value) ->
    PName = mlib_tool:to_atom(lists:concat([change_config, "_", ModName])),
    mlib_misc:critical_section(PName, fun() -> change_config_proc(ModName, Key, Value) end).
change_config_proc(ModName, Key, Value) ->
    ConfigPath = get_config_path(mlib_tool:to_list(ModName)),
    {ok, FileConsult} = file:consult(ConfigPath),
    NewFile = lists:keystore(Key, 1, FileConsult, {Key, Value}),
    NewConsult = [io_lib:format("~p.~n", [KVElem]) || KVElem <- NewFile],
    ok = file:write_file(ConfigPath, NewConsult, []),
    reload(ModName),
    ok.


%% @doc 修改配置文件, 注意不保留文件里的注释
%% @returns ok | error
remove_config(ModName, Key) ->
    PName = mlib_tool:to_atom(lists:concat([remove_config, "_", ModName])),
    mlib_misc:critical_section(PName, fun() -> remove_config_proc(ModName, Key) end).
remove_config_proc(ModName, Key) ->
    ConfigPath = get_config_path(mlib_tool:to_list(ModName)),
    {ok, FileConsult} = file:consult(ConfigPath),
    NewFile = lists:keydelete(Key, 1, FileConsult),
    NewConsult = [io_lib:format("~p.~n", [KVElem]) || KVElem <- NewFile],
    ok = file:write_file(ConfigPath, NewConsult, []),
    reload(ModName),
    ok.


%% ====================================================================
%% API Functions
%% ====================================================================
%% @doc 系统启动时将setting目录下的配置加载到内存中
init()->
    log_server_start_time(),
    reload_setting(),
    ok.

log_server_start_time()->
    ConfigPath = mconf_dyn:get_config_path("switching"),
    {ok,FileConsult} = file:consult(ConfigPath),
    KVList = lists:keystore(server_start_time, 1, FileConsult, {server_start_time, mtime:now()}),
    NewConsult = [io_lib:format("~p.~n", [KVElem]) || KVElem <- KVList],
    ok = file:write_file(ConfigPath, NewConsult, []),
    ok.

%% @doc 获取当前游戏服的版本号
%% 当前版本若是：B_beta0.2.4.0-20151121.32885.32885，此函数的返回值为：B_beta0.2.4.0
get_version() ->
    common_config:get_version().


%% @doc 获取配置文件目录
get_config_path() ->
    lists:concat([common_config:get_mge_root(), "config/"]).

get_setting_path() ->
    lists:concat([common_config:get_mge_root(), "setting/"]).

get_config_cfg_path()->
    get_config_path() ++ "cfg/".

get_config_cfg_path(FilePath) ->
    DefaultPath = get_config_cfg_path() ++ FilePath,
    case get_lang() of
        default -> DefaultPath;
        Lang ->
            LangPath = lists:concat([get_config_path(), "lang", Lang, "/", FilePath]),
            case file:read_file_info(LangPath) of
                {ok, _} -> LangPath;
                _ -> DefaultPath
            end
    end.

%% @doc 指定编译时间数据库放置的位置, 通过-meta_root命令行参数, 建议用源码目录(svn co目录)
get_meta_dir() ->
    case init:get_argument(meta_root) of
        {ok, [[Dir]]} -> Dir;
        _ -> ""
    end.

%% @doc 当前运行的语言版本
%% @returns Lang :: atom()
get_lang() ->
    case init:get_argument(lang) of
        {ok, [[Lang]]} -> mlib_tool:to_atom(Lang);
        _ -> default
    end.

%% @doc 重新加载setting目录下配置
reload_setting()->
    AllFileList =
        [begin
            {ModuleName,get_setting_path() ++ FilePath, FileType}
        end || {ModuleName,FilePath,FileType} <- common_config:get_selfload_configs()],
    do_load_config(AllFileList),
    ok.

%% @doc 重新加载全部配置
reload_all()->
    reload_setting(),
    AllFileList =
        [begin
            {ModuleName, get_config_cfg_path() ++ FilePath, FileType}
        end || {ModuleName, FilePath, FileType} <-  common_config:get_basic_configs()],
    do_load_config(AllFileList),
    ok.

%% @doc 重新加载配置文件
reload(ConfigName) when is_atom(ConfigName)->
    do_load_config(ConfigName);
reload(_ConfigName)  ->
    ignore.

%% @doc 根据键值查询配置
find(ConfigName,Key)->
    case ConfigName:find(Key) of
        undefined-> [];
        not_implement -> [];
        Val -> [Val]
    end.

%% @doc 获取配置文件全部键值
list(ConfigName)->
    case ConfigName:list() of
        undefined-> [];
        not_implement -> [];
        Val -> Val
    end.

%% @doc 获得所有的配置文件
%% @returns [[{name, Module}, {path, FilePath}]]
get_all_config() ->
    [begin
        [{name, ModuleName}, {path, get_setting_path() ++ FilePath}]
    end || {ModuleName,FilePath,_FileType} <-
        common_config:get_selfload_configs()] ++
    [begin
        [{name, ModuleName}, {path, get_config_cfg_path() ++ FilePath}]
    end || {ModuleName,FilePath,_FileType} <-
        common_config:get_basic_configs()].

%% @doc 直接读取配置文件内容
read_config(Name) ->
    case get_config_path(Name) of
        undefined -> undefined;
        FilePath -> file:read_file(FilePath)
    end.

%% @doc 直接写配置文件内容
%% @end
write_config(Name,Content) ->
    case get_config_path(Name) of
        undefined -> undefined;
        FilePath -> file:write_file(FilePath,Content,[binary])
    end.

%% @doc 获取配置文件路径
get_config_path(Name)->
    ConfigName = mlib_tool:to_atom(Name),
    case get_config_info(ConfigName) of
        {_ModName, FilePath, _FileType} -> FilePath;
        _ -> undefined
    end.


%% ====================================================================
%% Local Functions
%% ====================================================================
%% @doc 根据配置名加载配置文件
do_load_config(ConfigName) when erlang:is_atom(ConfigName) ->
    case get_config_info(ConfigName) of
        {_,_,_} = ConfigInfo ->
            do_load_config(ConfigInfo);
        _Other ->
            ?ERROR_MSG("mconf_dyn get_config_info error  ~p", [ConfigName]),
            clear_exit_info(),
            error
    end;
do_load_config({ModuleName,FilePath,FileType})  ->
    {ok,#file_info{size = FileSize}} = file:read_file_info(FilePath),
    case FileSize >= ?MAX_CONFIG_FILE_SIZE of
        true ->
            ?ERROR_MSG("mconf_dyn reload error: file_too_big ~p(~w)", [FilePath,FileSize]),
            clear_exit_info(),
            error;
        _ ->
            {ok,List} = file:consult(FilePath),
            case do_load_gen_src(ModuleName, FileType, List) of
                {ok,Code} ->
                    case ModuleName =:= common of
                        true ->ignore;
                        _ ->
                            file:write_file(lists:concat([common_config:get_mge_root(), "/ebin/", ModuleName, ".beam"]), Code, [write, binary])
                    end,
                    clear_exit_info(),
                    ok;
                _ ->
                    clear_exit_info(),
                    error
            end
    end;
do_load_config([]) ->
    ok;
do_load_config([Conf|T]) ->
    do_load_config(Conf),
    do_load_config(T).

%% @doc 获取config信息
%% @returns {ModuleName, FilePath， FileType} | false
get_config_info(ConfigName) ->
    AllFileList =
        [{ModuleName, get_setting_path() ++ FilePath, FileType} || {ModuleName, FilePath, FileType} <-  common_config:get_selfload_configs()]
        ++ get_config_filepaths(),
    lists:keyfind(ConfigName, 1, AllFileList).

get_config_filepaths() ->
    [{ModuleName, get_config_cfg_path(FilePath), FileType} || {ModuleName,FilePath,FileType} <-  common_config:get_basic_configs() ++ common_config:get_activity_configs()].


%% @doc 生成源代码，执行编译并加载到内存
do_load_gen_src(ModuleName,FileType,List)->
    case catch do_load_gen_src2(ModuleName,FileType,List) of
        {ok, Code} ->
            {ok, Code};
        Reason ->
            ?ERROR_MSG("Error compiling ~p Reason=~w ", [ModuleName, Reason]),
            error
    end.

do_load_gen_src2(ModuleName,FileType,List) ->
    if FileType =:= record_consult ->
        KeyValues =
            [ {element(2,Rec), Rec}  || Rec<- List],
        {_, ValList} = lists:unzip(KeyValues);
        true ->
            KeyValues = List,
            {_, ValList} = lists:unzip(KeyValues)
    end,
    Src = gen_src_code(ModuleName, set, KeyValues,ValList),
    {Mod, Code} = dynamic_compile:from_string( Src ),
    code:load_binary(Mod, mlib_tool:to_list(ModuleName) ++ ".erl", Code),
    {ok, Code}.

%% @doc
%% 项目*.config配置文件，生成对于的*.erl文件
%% erl -config_dir "" -output_dir ""
%% @end
gen_all_erl(ConfigDir, OutputDir) ->
    AllFileList =
        [{ConfigModuleName,lists:concat([ConfigDir,"/",FilePath]),FileType}
            || {ConfigModuleName,FilePath,FileType} <- common_config:get_basic_configs(),
            ConfigModuleName =/= common,
            is_atom(ConfigModuleName)],
    lists:foreach(
        fun({ConfigModuleName,FilePath,Type}) ->
            io:format("===> gen config file=~w~n", [ConfigModuleName]),
            OutputFilename = lists:concat([OutputDir, "/", ConfigModuleName, ".erl"]),
            ok = gen_all_erl_file(ConfigModuleName, FilePath, OutputFilename, Type)
        end, AllFileList),
    ok.
%% 只生成目录下的文件
gen_exist_erl(ConfigDir, OutputDir) ->
    LangPathList = filelib:wildcard(ConfigDir ++ "/*.config"),
    case LangPathList of
        [_|_] ->
            BaseConfigs = common_config:get_basic_configs(),
            AllFileList =
                [begin
                    ConfigModuleName = mlib_tool:to_atom(filename:rootname(filename:basename(FilePath))),
                    case lists:keyfind(ConfigModuleName, 1, BaseConfigs) of
                        {_, _, FileType} -> ok;
                        FileType -> erlang:throw({error, {'unknow file', FilePath}})
                    end,
                    {ConfigModuleName,FilePath,FileType}
                end || FilePath <- LangPathList ],
            lists:foreach(
                fun({ConfigModuleName,FilePath,Type}) ->
                    io:format("===> gen config file=~w~n", [ConfigModuleName]),
                    OutputFilename = lists:concat([OutputDir, "/", ConfigModuleName, ".erl"]),
                    ok = gen_all_erl_file(ConfigModuleName, FilePath, OutputFilename, Type)
                end, AllFileList),
            ok;
        _ ->
            ok
    end.


gen_all_erl_file(ConfigModuleName, FilePath, OutputFilename, Type) ->
    try
        {ok,#file_info{size = FileSize}} = file:read_file_info(FilePath),
        case FileSize >= ?MAX_CONFIG_FILE_SIZE of
            true ->
                io:format("ERROR file too big! ~p:~w bytes", [ConfigModuleName,FileSize]),
                file_too_big;
            _ ->
                {ok,RecList} = file:consult(FilePath),
                case Type of
                    record_consult ->
                        KeyValues = [{element(2, Rec),Rec} || Rec <- RecList];
                    key_value_consult ->
                        KeyValues = RecList
                end,
                {_, ValList} = lists:unzip(KeyValues),
                gen_src2file(OutputFilename, ConfigModuleName, set, KeyValues, ValList),
                ok
        end
    catch
        T:R ->
            io:format("file:~p format error! ~p:~p", [OutputFilename, T, R]),
            erlang:raise(T, R, [])
    end.

%% @doc 将配置文件生成对应的beam文件
gen_all_beam(Opts) ->
    load_meta_config(),
    AllFileList = get_all_config_files(),
    gen_config_beam(AllFileList, Opts),
    ok.

%% @doc 生成改动的配置对应的beam文件
gen_changed_beam(Opts) ->
    io:format("NOTE: ONLY RECOMPILE CHANGED CONFIGS."),
    load_meta_config(),
    AllFileList = get_changed_config_files(),
    gen_config_beam(AllFileList, Opts),
    ok.

gen_config_beam(AllFileList, Opts) ->
    io:format("TOTAL CONFIG FILES: ~w", [length(AllFileList)]),
    lists:foreach(
        fun({ConfigModuleName,FilePath,Type}) ->
            io:format("~w", [ConfigModuleName]),
            ok = gen_src_file(ConfigModuleName, FilePath, Type)
        end, AllFileList),
    do_compile_config_erls(Opts).

%% @doc 编译erl为beam
%% @returns ok|error
do_compile_config_erls(_Opts) ->
    ok = file:set_cwd(lists:concat([common_config:get_mge_root(), "ebin/"])),
    case easy_make:make_src() of
        up_to_date ->
%%            save_meta_config(),
            ok;
        error ->
            error
    end.

%% @doc 多进程并行，将配置文件生成对应beam文件
%% @returns ok|error|Error
gen_all_beam_par(Opts) ->
    load_meta_config(),
    AllFileList = get_all_config_files(),
    gen_config_beam_par(AllFileList, Opts).

%% @doc 多进程并行，将改动的配置文件生成对应beam文件
%% @returns ok|error|Error
gen_changed_beam_par(Opts) ->
    io:format("INFO: Changed Config Files ONLY"),
    load_meta_config(),
    AllFileList = get_changed_config_files(),
    gen_config_beam_par(AllFileList, Opts).

%% @returns ok|error|Error
gen_config_beam_par(AllFileList, Opts) ->
    io:format("total config files: ~w", [length(AllFileList)]),
    Ref = erlang:make_ref(),
    Works = do_split_list(AllFileList, erlang:system_info(schedulers_online)*2),
    Pid = self(),
    [gen_all_beam_worker(L, Pid, Ref) || L <- Works],
    case wait_for_workers(length(Works), Ref) of
        ok ->
            do_compile_config_erls(Opts);
        Error ->
            Error
    end.

get_all_config_files() ->
    [{ConfigModuleName,get_config_cfg_path()++FilePath,FileType} || {ConfigModuleName,FilePath,FileType} <- common_config:get_basic_configs(),
        ConfigModuleName =/= common,
        is_atom(ConfigModuleName)].

get_changed_config_files() ->
    filter_unchanged_files(get_all_config_files()).

filter_unchanged_files(L) ->
    lists:filter(fun(F) -> check_file_changed(F) end, L).

check_file_changed({ConfigModuleName,ConfigFile,Type}) ->
    check_meta_config(ConfigModuleName, ConfigFile, Type) orelse
        check_file_times(ConfigModuleName, ConfigFile).

check_meta_config(ConfigModuleName, ConfigFile, Type) ->
    case get_meta_config(ConfigModuleName) of
        {ok,{ConfigFile,Type,_,_}} ->
            false;
        _ ->
            true
    end.

check_file_times(ConfigModuleName, ConfigFile) ->
    {ok,{_,_,ConfigTime,BeamTime}} = get_meta_config(ConfigModuleName),
    case file:read_file_info(ConfigFile) of
        {ok,#file_info{mtime=ConfigTime}} when BeamTime>=ConfigTime ->
            case get_beam_time(ConfigModuleName) of
                BeamTime ->
                    false;
                _ ->
                    true
            end;
        _ ->
            true
    end.

%% @doc 获取beam文件上次变化时间
get_beam_time(ConfigModuleName) ->
    BeamFile = lists:concat([common_config:get_mge_root(), "ebin/", ConfigModuleName, ".beam"]),
    get_file_time(BeamFile).

get_file_time(File) ->
    case file:read_file_info(File) of
        {ok,#file_info{mtime=T}} ->
            T;
        _ ->
            0
    end.

%% @doc 将L分割成最多包含N个子列表的列表
do_split_list(L, N) ->
    Len = length(L),
    % 每个列表的元素数
    LLen = (Len + N - 1) div N,
    do_split_list(L, LLen, []).

do_split_list([], _N, Acc) ->
    lists:reverse(Acc);
do_split_list(L, N, Acc) ->
    {L2, L3} = lists:split(erlang:min(length(L), N), L),
    do_split_list(L3, N, [L2 | Acc]).

%% @doc 创建编译工作者节进程
gen_all_beam_worker(Files, Parent, Ref) ->
    Fun =
        fun() ->
            lists:foreach(
                fun({ConfigModuleName,FilePath,Type}) ->
                    io:format("~w.config", [ConfigModuleName]),
                    case gen_src_file(ConfigModuleName, FilePath, Type) of
                        ok -> ok;
                        Error -> throw(Error)
                    end
                end, Files),
            ok
        end,
    spawn_link(fun() ->
        case catch Fun() of
            ok ->
                Parent ! {ack,Ref};
            Err ->
                Parent ! {error,Err}
        end
    end).

%% @doc 等待编译进程工作完成返回数据
%% @returns ok|Error
wait_for_workers(0, _) ->
    ok;
wait_for_workers(N, Ref) ->
    receive
        {ack,Ref} ->
            wait_for_workers(N-1, Ref);
        {error,_} = Error ->
            Error;
        _ ->
            wait_for_workers(N, Ref)
    end.

%% @doc 生成config对应文件
%% @returns ok|file_too_big
%% @throws Error
gen_src_file(ConfigModuleName, FilePath, Type) ->
    try
        {ok,#file_info{size = FileSize}} = file:read_file_info(FilePath),
        case FileSize >= ?MAX_CONFIG_FILE_SIZE of
            true ->
                io:format("ERROR file too big! ~p:~w bytes", [ConfigModuleName,FileSize]),
                file_too_big;
            _ ->
                {ok,RecList} = file:consult(FilePath),
                case Type of
                    record_consult ->
                        KeyValues = [{element(2, Rec),Rec} || Rec <- RecList];
                    key_value_consult ->
                        KeyValues = RecList
                end,
                {_, ValList} = lists:unzip(KeyValues),
                Filename = lists:concat([common_config:get_mge_root(), "ebin/", ConfigModuleName, ".erl"]),
                gen_src2file(Filename, ConfigModuleName, set, KeyValues, ValList),
                ok
        end
    catch
        T:R ->
            io:format("file:~p format error! ~p:~p", [FilePath, T, R]),
            erlang:raise(T, R, [])
    end.


%% @doc 生成config对应的erl文件
gen_src2file(Filename, ConfModuleName, Type, KeyValues, ValList) ->
    {ok,Fd} = file:open(Filename, [write,binary,{encoding,utf8}]),
    KeyValues2 = get_src_key_values(Type, KeyValues),
    check_duplicate_keys(ConfModuleName, KeyValues2),
    gen_src2file_header(ConfModuleName, Fd),
    gen_src2file_list(ValList, Fd),
    gen_src2file_all(KeyValues2, Fd),
    gen_src2file_find(KeyValues2, Fd),
    file:close(Fd),
    ok.

gen_src2file_header(ConfModuleName, Fd) ->
    Code = "-module(" ++ mlib_tool:to_list(ConfModuleName) ++ ").\n-export([list/0,all/0,find/1]).\n",
    file:write(Fd, Code).

gen_src2file_list(ValList, Fd) ->
    Code = io_lib:format("list() -> ~w.\n", [ValList]),
    file:write(Fd, Code).

gen_src2file_all(KeyValues, Fd) ->
    case if_config_all() of
        true ->
            Code = io_lib:format("all() -> ~w.\n", [KeyValues]);
        _ ->
            Code = "all() -> []. \n"
    end,
    file:write(Fd, Code).

gen_src2file_find([], Fd) ->
    file:write(Fd, "find(_Key) -> undefined.");
gen_src2file_find([{K,V}|L], Fd) ->
    Code = io_lib:format("find(~w) -> ~w;\n", [K,V]),
    file:write(Fd, Code),
    gen_src2file_find(L, Fd).


gen_src_code(ConfModuleName,Type,KeyValues,ValList) ->
    KeyValues2 = get_src_key_values(Type, KeyValues),
    KeyValues3 = lists:reverse(KeyValues2),
    check_duplicate_keys(ConfModuleName, KeyValues3),
    Bin0 = gen_src_header(ConfModuleName, <<>>),
    Bin1 = gen_src_list(ValList, Bin0),
    Bin2 = gen_src_all(ConfModuleName, KeyValues, Bin1),
    Bin3 = gen_src_find(KeyValues3, Bin2),
    mlib_tool:to_list(Bin3).

gen_src_find([], BinAcc) ->
    <<BinAcc/binary, "find(_) -> undefined.\n">>;
gen_src_find([{Key,Value}|L], BinAcc) ->
    KeyBin = mlib_tool:to_binary(io_lib:format("~w", [Key])),
    ValueBin = mlib_tool:to_binary(io_lib:format("~w", [Value])),
    BinAcc2 = <<BinAcc/binary, <<"find(">>/binary, KeyBin/binary, <<") -> ">>/binary, ValueBin/binary, <<";">>/binary>>,
    gen_src_find(L, BinAcc2).

gen_src_all(ConfModuleName, KeyValues, BinAcc) ->
    case if_config_all() orelse ConfModuleName =:= common of
        true ->
            AllBin = mlib_tool:to_binary(io_lib:format("~w", [KeyValues]));
        _ ->
            AllBin = <<"[]">>
    end,
    <<BinAcc/binary, <<"all() -> ">>/binary, AllBin/binary, <<".\n">>/binary>>.

gen_src_list(ValList, BinAcc) ->
    StrList = io_lib:format("~w", [ValList]),
    ListBin = mlib_tool:to_binary(StrList),
    <<BinAcc/binary, <<"list() -> ">>/binary, ListBin/binary, <<".\n">>/binary>>.

gen_src_header(ConfModuleName, BinAcc) ->
    HeaderBin = mlib_tool:to_binary(mlib_tool:to_list(ConfModuleName)),
    <<BinAcc/binary, <<"-module('">>/binary, HeaderBin/binary, <<"').\n-export([list/0,all/0,find/1]).\n">>/binary>>.

get_src_key_values(bag, KeyValues) ->
    lists:foldl(fun({K, V}, Acc) ->
        case lists:keyfind(K, 1, Acc) of
            false ->
                [{K, [V]}|Acc];
            {K, VO} ->
                [{K, [V|VO]}|lists:keydelete(K, 1, Acc)]
        end
    end, [], KeyValues);
get_src_key_values(_, KeyValues) ->
    KeyValues.

check_duplicate_keys(ConfModName, KVs) ->
    ETS = ets:new(check_config_dup_keys, [private,set]),
    check_duplicate_keys(ConfModName, KVs, ETS),
    ets:delete(ETS),
    ok.
check_duplicate_keys(_, [], _) -> done;
check_duplicate_keys(ConfModuleName, [{K,_}|L], ETS) ->
    case ets:lookup(ETS, K) of
        [_] ->
            io:format("WARNING duplicate key! ~w:~w", [ConfModuleName,K]),
            check_duplicate_keys(ConfModuleName, L, ETS);
        _ ->
            ets:insert(ETS, {K,true}),
            check_duplicate_keys(ConfModuleName, L, ETS)
    end.

clear_exit_info() ->
    receive
        {'EXIT', _PID, _Reason} ->
            ignore
    after 0 ->
        ok
    end,
    ok.

if_config_all() ->
    case init:get_argument(conf) of
        {ok,[["all"]] } ->
            true;
        _ ->
            false
    end.

%% @doc 加载编译状态的文件到内存中
load_meta_config() ->
    Filename = get_meta_config_filename(),
    case exists(Filename) of
        true ->
            io:format("load meta config: ~w (~w entries)\n", [ets:file2tab(Filename),length(ets:tab2list(ets_meta_config))]);
        false ->
            io:format("init meta config: ok.\n", []),
            ets:new(ets_meta_config, [set,named_table,public,{read_concurrency,true},{keypos,1}])
    end.
%%
%%%% @doc 将编译状态的ets内存数据写入文件
%%save_meta_config() ->
%%    io:format("save meta config: ~s", [filename:absname(get_meta_config_filename())]),
%%    save_meta_config(get_all_config_files()).
%%save_meta_config(FileList) ->
%%    [ets:insert(ets_meta_config, {ConfigModuleName,{ConfigFile,Type,get_file_time(ConfigFile),get_beam_time(ConfigModuleName)}}) || {ConfigModuleName,ConfigFile,Type} <- FileList],
%%    ets:tab2file(ets_meta_config, get_meta_config_filename()),
%%    ok.

get_meta_config_filename() ->
    get_meta_dir() ++ "ets_meta_config".

%% @doc 检查文件是否存在
exists(Filename) ->
    case file:read_file_info(Filename) of
        {ok,_} ->
            true;
        _ ->
            false
    end.

%% @doc 根据配置文件获取编译状态信息
%% @returns {ok,{ConfigFile,Type,ConfigTime,BeamTime}}|{error,not_found}
get_meta_config(ConfigModuleName) ->
    case ets:lookup(ets_meta_config, ConfigModuleName) of
        [{_,D}] ->
            {ok,D};
        _ ->
            {error,not_found}
    end.

