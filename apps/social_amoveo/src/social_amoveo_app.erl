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
    spawn(fun() -> 
                  timer:sleep(1000),
                  accounts:block_scan(),
                  accounts:block_cron() 
          end),
    spawn(fun() ->
                  popular_posts_cache:cron()
          end),
    accounts:repossess_cron(),
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
%    {ok, _} = cowboy:start_http(
%                http, 100,
%                [{ip, {0, 0, 0, 0}}, {port, Port}],
%                [{env, [{dispatch, Dispatch}]}]),
    {ok, _} = cowboy:start_clear(
                http,
                [{ip, {0,0,0,0}}, {port, Port}],
                #{env => #{dispatch => Dispatch}}),
    ok.

