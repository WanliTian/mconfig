-module(mconfig_lib.erl).

-export([
    config_path/0,
    delay/0
]).

config_path() ->
    init:get_argument(config).

delay() ->
    application:get_env(mconfig, delay, 5000).
