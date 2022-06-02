%UNUSED.


-module(nonces).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
init(ok) -> 
    process_flag(trap_exit, true),
    ets_tools:load_ets(?MODULE),
    {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> 
    ets_tools:save_table(?MODULE),
    io:format("nonces died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({update, I, Nonce}, X) -> 
    Prev = lookup(I),
    if
        Nonce > Prev ->
            ets:insert(?MODULE, [{I, Nonce}]);
        true -> ok
    end,
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call({check, I}, _From, X) -> 
    Y = lookup(I),
    {reply, Y, X};
handle_call(_, _From, X) -> {reply, X, X}.

lookup(I) ->
    case ets:lookup(?MODULE, I) of
        [{I, N}|_] -> N;
        [] -> 0
    end.


update(I, Nonce) 
  when ((is_integer(Nonce)) 
        and (Nonce > 0)) ->
    gen_server:cast(?MODULE, {update, I, Nonce}).
check(I) ->
    gen_server:call(?MODULE, {check, I}).
    
