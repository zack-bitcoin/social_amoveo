-module(file_handler).
-export([init/3, handle/2, terminate/3, init/2]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:8095/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:8095

init(Req0, Opts) ->
    handle(Req0, Opts).	
handle(Req, _) ->
    %{F, _} = cowboy_req:path(Req),
    F = case cowboy_req:path(Req) of
            {This, _} -> This;
            That ->  That
        end,
    %io:fwrite(F),
    PrivDir0 = "../../../../js",
    PrivDir = list_to_binary(PrivDir0),
    true = case F of
	       <<"/utils.js">> -> true;
	       <<"/rpc.js">> -> true;
	       <<"/BigInteger.js">> -> true;
	       <<"/codecBytes.js">> -> true;
	       <<"/crypto.js">> -> true;
	       <<"/elliptic.min.js">> -> true;
	       <<"/favicon.ico">> -> true;
	       <<"/files.js">> -> true;
	       <<"/format.js">> -> true;
	       <<"/home.html">> -> true;
	       <<"/keys.js">> -> true;
	       <<"/main.js">> -> true;
	       <<"/server.js">> -> true;
	       <<"/sha256.js">> -> true;
	       <<"/signing.js">> -> true;
	       <<"/sjcl.js">> -> true;
	       <<"/test.html">> -> true;
	       <<"/cold.html">> -> true;
	       <<"/nonce.js">> -> true;
	       <<"/account_div_maker.js">> -> true;
	       <<"/account_loader.js">> -> true;
	       <<"/following.js">> -> true;
	       <<"/encryption.js">> -> true;
	       <<"/encryption_library.js">> -> true;
	       <<"/send_dm_div_maker.js">> -> true;
               
               X -> 
                   io:fwrite("ext file handler block access to: "),
                   io:fwrite(X),
                   io:fwrite("\n"),
                   false
           end,
    File = << PrivDir/binary, F/binary>>,
    {ok, _Data, _} = cowboy_req:read_body(Req),
    %Headers = [{<<"content-type">>, <<"text/html">>},
    %           {<<"Access-Control-Allow-Origin">>, 
    %            <<"*">>}],
    Headers = #{<<"content-type">> => <<"text/html">>,
                <<"Access-Control-Allow-Origin">> => <<"*">>},
    Text = read_file(File),
    %{ok, Req2} = cowboy_req:reply(200, Headers, Text, Req),
    Req2 = cowboy_req:reply(200, Headers, Text, Req),
    %io:fwrite(Req2),
    {ok, Req2, File}.
read_file(F) ->
    {ok, File } = file:open(F, [read, binary, raw]),
    {ok, O} =file:pread(File, 0, filelib:file_size(F)),
    file:close(File),
    O.
init(_Type, Req, _Opts) -> {ok, Req, []}.
terminate(_Reason, _Req, _State) -> ok.
