-ifndef(CONFIG_HRL).
-define(CONFIG_HRL, config_hrl).

-ifdef(TEST).
-compile({parse_transform, config_gen_all_key}).
-endif.

-define(CFG_H, find(K) -> case K of ).

-define(C(K,V),  K -> V; ).

-define(CFG_E,  _Other -> undefined
end ).
-endif.