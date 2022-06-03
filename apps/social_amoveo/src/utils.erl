-module(utils).
-export([off/0, talk/1]).


off() ->
    amoveo_explorer_sup:stop(),
    ok = application:stop(amoveo_explorer),
    halt().

talk(X) ->
    TM = test_mode(),
    Port = 
        if
            TM-> 3011;
            true -> 8081
        end,
    talker:talk(X, {{127,0,0,1}, Port}).
test_mode() ->
    case application:get_env(social_amoveo, test_mode) of
        {ok, B} -> B;
        _ -> false
    end.
