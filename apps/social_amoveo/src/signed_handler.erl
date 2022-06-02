-module(signed_handler).
-export([init/3, handle/2, terminate/3, doit/2, init/2]).
init(Req0, Opts) ->
    handle(Req0, Opts).	
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
handle(Req, State) ->
    {ok, Data0, Req2} = cowboy_req:body(Req),
    {IP, _} = cowboy_req:peer(Req2),
    Data1 = jiffy:decode(Data0),
    Stx = packer:unpack_helper(Data1),

    Tx = element(2, Stx),
    Pub = element(2, Tx),
    Nonce = element(3, Tx),
    AID = pubkeys:read(Pub),
    Acc =  accounts:read(AID),
    PrevNonce =  accounts:nonce(Acc),
    true = Nonce > PrevNonce,
    D = packer:pack(doit(Tx, AID)),
    accounts:update_nonce(AID, Nonce),

    Headers=[{<<"content-type">>,<<"application/octet-stream">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    Req4 = cowboy_req:reply(200, Headers, D, Req2),
    {ok, Req4, State}.
doit({test}, _) -> {ok, "success"};
doit({balance}, AID) ->
    %returns your balance in coins and in coin-hours. Also tells how many coins you have delegated, ho
    {ok, 0};

%todo
%delegate coins to another account. so you can use cold storage funds.
%* delegate coins to another account. (so you can use your cold storage funds).
%* choose an account name. (you get refunded coins for making your name shorter)
%* choose a description for your account.(you get refunded coins for making your description shorter.)
%* follow an account.
%* make a post.
%* upvote/downvote (vote) a post. post id, amount to vote with. (potentially changes the list of post ids ordered by popularity in the author's account).
%* scale back all votes. (is costs according to how many votes you have.)
%* delete all but your N biggest votes. (costs accourding to how many votes you have.) (refunds coins from those votes)
%* send a DM, with an optional coin-hour lockup. (costs more for longer messages, and if the expiration is further in the future.)
%* check DM. (costs coins, because now the data is yours to delete) (refunds coin-hours to sender based on how long until the refund)
%* unlock DM deposit

%API requests that use only coin-hours:
%* un-delegate coins.
%* check your balance of coins and coin-hours.
%* lookup post by id
%* lookup account by pubkey
%* spend coin-hours to another account. 
%* N recent chronological posts by accounts 
%* top N posts written by these accounts (costs by how many posts are loaded)
%* top posts, according to upvotes from these accounts. costs by (database lookup for each account) + (sum up sublists of each account)
%* list of accounts that an account follows.
%* given list of accounts L, and an account A, returns subset of L that are followers of A.
%* unfollow an account. (refunds coins)
%* list of who delegates to you.

%Free signed api
%* delete a DM.
%* unlock/burn deposit from DM
%* remove a vote.
%* delete a post.
%* undelegate.

%Free signed api, waits a couple seconds before responding. so that you don't need to already have coin-hours to be able to delete things and recuperate your coins to start generating coin-hours again.
%* DM ids you already read.
%* lookup DM by id.
%* list of who you delegated to.
%* list of your read DMs.


doit(X, _) ->
    io:fwrite("http handler doit fail"),
    io:fwrite(X),
    <<"error- unsupported http request.">>.
