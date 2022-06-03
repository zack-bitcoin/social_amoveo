

%For each account, this is a relation from the pubkey of that account to it's id.
%This way we don't need to store the long pubkeys anywhere besides here.

% a dictionary where keys are pubkeys, and values are ids.

-module(pubkeys).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        new/2, read/1,
        test/0]).
init(ok) -> 
    process_flag(trap_exit, true),
    Top = ets_tools:load_ets(?MODULE),
    {ok, Top}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, Top) -> 
    ets:insert(?MODULE, [{top, Top}]),
    ets_tools:save_table(?MODULE),
    io:format("pubkeys died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({new, Pubkey, ID}, X) -> 
    ets:insert(?MODULE, [{Pubkey, ID}]),
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, Pubkey}, _From, X) -> 
    {reply, ets_tools:read(?MODULE, Pubkey), X};
handle_call(_, _From, X) -> {reply, X, X}.

new(Pubkey, ID) ->
    gen_server:cast(?MODULE, {new, Pubkey, ID}).
read(Pubkey) ->
    gen_server:call(?MODULE, {read, Pubkey}).

test() ->
    ID1 = <<"1">>,
    ID2 = <<"2">>,
    A1 = new(ID1, 1),
    A2 = new(ID2, 2),
    A1 = read(ID1),
    A2 = read(ID2),
    success.
    
    
