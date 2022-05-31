-module(accounts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        cron/0]).

-record(acc, 
        {id, pubkey, 
         name = "", description = "", 
         timestamp, %when coin-hours was last calculated.
         veo = 0, %how many veo your account owns in the Amoveo blockchain.
         delegated = 0, %positive or negative
         %net_coins = veo + delegated - coins_in_posts - coins_in_votes
         coin_hours = 0, 
         nonce = 0, %used for signing api requests.
         coins_in_posts = 0, 
         coins_in_votes = 0, 
         following = [], %account ids
         dms = [], %ids of direct messages
         delegate_to = [],%{account id, balance} 
         delegate_of = [], %{account id, balance} they delegate to us.
         posts = [], %{post_id, timestamp, upvotes, downvotes} posts that we authored, in chronological order, recent posts first.
         votes = [] %{post_id, amount, timestamp}
        }).

%formula for updating coin-hours.
%P = Previous balance of coin-hours.
%C = coins balance
%T = time passed since last update
%H = half-life. maybe 30 days or 720 hours?

%N = New Balance of coin-hours
%N = P + (T*(C - P)/H)



init(ok) -> 
    process_flag(trap_exit, true),
    Top = ets_tools:load_ets(?MODULE),
    {ok, Top}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, Top) -> 
    ets:delete(?MODULE, top),
    ets:insert(?MODULE, [{top, Top}]),
    ets_tools:save_table(?MODULE),
    io:format("accounts died!\n"), 
    ok.
    
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.


cron() ->
    %check if there is a new block in the full node on the same computer.
    %if there is a block, check if any account balances changed.
    %change "veo" to be their balance in amoveo.
    %maybe base it off the code from the explorer.
    ok.
