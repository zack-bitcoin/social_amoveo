-module(utils).
-export([off/0]).


off() ->
    amoveo_explorer_sup:stop(),
    ok = application:stop(amoveo_explorer),
    halt().
