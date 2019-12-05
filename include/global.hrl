-ifndef(GLOBAL_HRL).
-define(GLOBAL_HRL, global_hrl).
%%包含其他文件
-include("proto/common_error_no.hrl").
-include("common_pb.hrl").

-define(WORD_SIZE, 8).

%% boolean 宏
-define(TRUE, 1).
-define(FALSE, 0).
-define(IF(C, T, F), (case (C) of true -> (T); false -> (F) end)).

%%日志相关
-define(PRINT(Format, Args), io:format(Format, Args)).
-define(NONE, none).

-define(ZERO_SN, <<"0">>).

%% 每个服只能创建最多9999W个角色
%% real_max_role_num:1000000
%% role_id_offset:1000

-define(MAX_ROLE_NUM, 1000000000).
-define(ROLE_ID_OFFSET, 1).
-define(MAX_ROLE_NUM_OFFSET, (?MAX_ROLE_NUM * ?ROLE_ID_OFFSET)). %% real_max_role_num*role_id_offset
-define(MIN_ROLE_ID, (?MAX_ROLE_NUM * ?ROLE_ID_OFFSET)). %%?MAX_ROLE_NUM*?MAX_SERVER_ID


-define(DAY_SECOND, 86400).
-define(HOUR_SECOND, 3600).
-define(MINUTE_SECOND, 60).

-define(DAY_RESET_SEC_SHIFT,  (0) ). %% 凌晨重置
-define(SHIFT_TIMESTAMP, ?SHIFT_TIMESTAMP(mtime:now()) ).
-define(SHIFT_TIMESTAMP(Ts), (Ts) - ?DAY_RESET_SEC_SHIFT).

%%简单类型转换
-define(BOOL2INT(Bool), ?IF((Bool), ?TRUE, ?FALSE)).
-define(INT2BOOL(INT), ?IF(((INT) =:= ?FALSE), false, true)).

%% 32位整数最大值/最小值
-define(INT32MAX, 2147483647).
-define(INT32MIN, -2147483648).

-define(SEC_PER_MIN,60).
-define(MIN_PER_HOUR,60).
-define(HOUR_PER_DAY, 24).
-define(ONE_MINUTE, ?SEC_PER_MIN).
-define(ONE_HOUR, (?MIN_PER_HOUR * ?ONE_MINUTE)).
-define(ONE_DAY, (?HOUR_PER_DAY * ?ONE_HOUR)).

%%缓存数据key
-define(IS_REMOVE, is_remove). %%当前节点是否要移除

-define(RETURN_OK(Result),erlang:throw({ok,Result})).
-define(RETURN_OK,erlang:throw(ok)).
-endif.

-ifndef(SHIFT_DATE).
-define(SHIFT_DATE, mtime:timestamp_to_date(?SHIFT_TIMESTAMP)).
-endif.

-ifndef(SHIFT_DATE_TIME).
-define(SHIFT_DATE_TIME, mtime:timestamp_to_datetime(?SHIFT_TIMESTAMP)).
-endif.
