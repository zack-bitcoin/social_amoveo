-module(http_handler).
-export([init/3, handle/2, terminate/3, doit/1, init/2]).
init(Req0, Opts) ->
    handle(Req0, Opts).	
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
handle(Req, State) ->
    {ok, Data0, Req2} = cowboy_req:body(Req),
    {IP, _} = cowboy_req:peer(Req2),
    Data1 = jiffy:decode(Data0),
    Data = packer:unpack_helper(Data1),
    D = packer:pack(doit(Data)),
    Headers=[{<<"content-type">>,<<"application/octet-stream">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    Req4 = cowboy_req:reply(200, Headers, D, Req2),
    {ok, Req4, State}.

sleeper() ->
    %timer:sleep(2000).
    timer:sleep(0).
    

doit({test}) -> {ok, "success"};

%todo
%top posts
doit({top}) -> 
    sleeper(),
    {ok, element(1, popular_posts_cache:read())};

%most hated posts
doit({top, 0, 2}) -> 
    sleeper(),
    {ok, element(2, popular_posts_cache:read())};

% top posts from an account
doit({top, AID}) -> 
    sleeper(),
    {ok, accounts:posts(AID)};

doit({repo, AID}) ->
    sleeper(),
    {ok, accounts:votes(AID)};

doit({sid}) ->
    sleeper(),
    {ok, settings:server_id()};

doit({x, 1, AID}) ->
    sleeper(),
    {ok, A} = accounts:balance_read(AID),
    {ok, accounts:nonce(A)};

doit({x, 2, Pub}) ->
    sleeper(),
    {ok, pubkeys:read(base64:encode(Pub))};

doit({x, 3, PID}) ->
    sleeper(),
    {ok, posts:read(PID)};

doit({x, 4, AID}) ->
    sleeper(),
    {ok, accounts:balance_read(AID)};

doit(X) ->
    io:fwrite("http handler doit fail"),
    io:fwrite(X).
