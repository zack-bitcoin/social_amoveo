%%%-------------------------------------------------------------------
%% @doc social_amoveo public API
%% @end
%%%-------------------------------------------------------------------

-module(social_amoveo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    inets:start(),
    start_http(),
    spawn(fun() -> accounts:block_cron() end),
    social_amoveo_sup:start_link().

stop(_State) ->
    ok.


start_http() ->
    Dispatch =
        cowboy_router:compile(
          [{'_', [
		  {"/signed/", signed_handler, []},
		  {"/:file", file_handler, []},
		  {"/", http_handler, []}
		 ]}]),
    Port = 8095,
    {ok, _} = cowboy:start_http(
                http, 100,
                [{ip, {0, 0, 0, 0}}, {port, Port}],
                [{env, [{dispatch, Dispatch}]}]),
    ok.

