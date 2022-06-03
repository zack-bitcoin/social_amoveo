-module(scan_height).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        read/0, increment/1]).
-define(LOC, "scan_height.db").
init(ok) ->
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Y = if
            (X == "") -> 0;
            true -> X
        end,
    {ok, Y}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("scan_height died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({height, N}, X) -> 
    {noreply, max(N, X)};
handle_cast(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.

increment(N) ->
    gen_server:cast(?MODULE, {height, N}).
read() ->
    gen_server:call(?MODULE, read).
