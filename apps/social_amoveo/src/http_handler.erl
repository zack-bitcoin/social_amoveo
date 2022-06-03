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
doit({test}) -> {ok, "success"};

%todo
%top posts
% top posts from an account
%recent posts from an account
%votes from an account
%* settings:server_id()

doit(X) ->
    io:fwrite("http handler doit fail"),
    io:fwrite(X).
