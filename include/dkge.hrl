%%%----------------------------------------------------------------------
%%% @author  :
%%% @doc: 通用的游戏头文件
%%%----------------------------------------------------------------------
-ifndef(DKGE_HRL).
-define(DKGE_HRL, dkge_hrl).

%% 程序日志记录宏
-define(DEBUG(Format, Args),
    dlog_entry:debug_msg(node(), ?MODULE,?LINE,Format, Args)).
-define(DEBUG(D), ?DEBUG(D, [])).

-define(INFO_MSG(Format, Args),
    dlog_entry:info_msg(node(), ?MODULE,?LINE,Format, Args)).
-define(INFO_MSG(D), ?INFO_MSG(D, [])).

-define(WARNING_MSG(Format, Args),
    dlog_entry:warning_msg( node(), ?MODULE,?LINE,Format, Args)).
-define(WARNING_MSG(D), ?WARNING_MSG(D, [])).

-define(ERROR_MSG(Format, Args),
    dlog_entry:error_msg( node(), ?MODULE,?LINE,Format, Args)).
-define(ERROR_MSG(D), ?ERROR_MSG(D, [])).

-define(CRITICAL_MSG(Format, Args),
    dlog_entry:critical_msg( node(), ?MODULE,?LINE,Format, Args)).
-define(CRITICAL_MSG(D), ?CRITICAL_MSG(D, [])).

%%日志相关
-define(PRINT(Format, Args), io:format(Format, Args)).

-endif.