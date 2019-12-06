-module(cfg_test).
-include("config.hrl").
%% API
-export([find/1]).

?CFG_H
?C(1, 2)
?CFG_E.