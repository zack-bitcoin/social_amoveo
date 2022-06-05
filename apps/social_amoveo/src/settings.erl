-module(settings).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-export([bytes/0, %hard drive bytes we can use for this social media website.
         coins/0, %approximate number of veo currently.
         half_life/0,%how long do coin-hours last
         cpu/0, %how many computations we can do per block
         dm_cost/0,
         post_cost/0,
         follow_cost/0,
         vote_cost/0,
         coins_per_byte/0,
         server_id/0,
         minimum_account_balance/0
         
]).

-record(d, {server_id}).
-define(LOC, "settings.db").

init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Y = if
            (X == "") -> 
                #d{server_id = 
                       crypto:strong_rand_bytes(
                         32)};
            true -> X
        end,
    {ok, Y}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, D) -> 
    db:save(?LOC, D),
    io:format("settings died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> 
    {reply, X#d.server_id, X};
handle_call(_, _From, X) -> {reply, X, X}.

server_id() ->
    gen_server:call(?MODULE, server_id).


bytes() ->
   %_123123123
    1000000000.%1 gigabytes
coins() -> %in satoshis
    %84.6k veo
   %_____12345678
    8460000000000.

coins_per_byte() ->
    %actually satoshis per byte.
    coins() div bytes().
    %8460 satoshis per byte -> 0.000084 veo-hours per byte -> 1 veo gives you 11k bytes.

half_life() -> %in blocks
    1008.
cpu() ->
    1000000.
dm_cost() ->
    30 * coins_per_byte().%in coin hours, to store a dm. + the cost per byte of the message.
post_cost() -> 38 * coins_per_byte().
follow_cost() -> 4 * coins_per_byte().
vote_cost() -> 20 * coins_per_byte().
%(coin-hours produced per block are the same as the market cap. So if you have (Market cap) many coin-hours, then you should be able to use nearly 100% of the server's CPU per hour)

minimum_account_balance() -> 66 * coins_per_byte().
