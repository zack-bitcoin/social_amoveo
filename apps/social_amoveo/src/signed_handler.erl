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
    ServerID = element(4, Tx),
    ServerID = settings:server_id(),
    {ok, AID} = pubkeys:read(base64:encode(Pub)),
    {ok, Acc} =  accounts:balance_read(AID),
    PrevNonce =  accounts:nonce(Acc),
    io:fwrite("nonce prev:"),
    io:fwrite(integer_to_list(PrevNonce)),
    io:fwrite(" "),
    io:fwrite(integer_to_list(Nonce)),
    io:fwrite("\n"),
    true = Nonce > PrevNonce,
    D = packer:pack(doit(Tx, AID)),
    accounts:update_nonce(AID, Nonce),

    Headers=[{<<"content-type">>,<<"application/octet-stream">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    Req4 = cowboy_req:reply(200, Headers, D, Req2),
    {ok, Req4, State}.
doit({test, _, _, _}, _) -> {ok, "success"};
doit({balance, _, _, _, AID}, From) 
  when is_integer(AID) ->
    %static sized info about an account.
    ok = accounts:charge(From, settings:api_cost()),
    {ok, A} = accounts:balance_read(AID),
    A2 = tuple_to_list(A),
    {A3, _} = lists:split(11, A2),
    R = list_to_tuple(A3),
    %io:fwrite(R),
    {ok, R};
doit({balance, _, _, _, Pub}, From) ->
    case pubkeys:read(Pub) of
        error -> {ok, <<"that pubkey is not assigned to any account.">>};
        {ok, AID} ->
            ok = accounts:charge(
              From, settings:api_cost()),
            A = accounts:balance_read(AID),
            A2 = tuple_to_list(A),
            {A3, _} = lists:split(11, A2),
            {ok, list_to_tuple([AID|A3])}
    end;

doit({send, _, _, _, Coins, To}, From) ->
    %send coins. NOT coin-hours
    ok = accounts:charge(From, settings:api_cost()),
    if
        is_integer(To) -> 
            accounts:delegate(From, To, Coins);
        true -> 
            case pubkeys:read(To) of
                error -> 
                    accounts:delegate_new(
                      From, Coins, 0);
                {ok, ID} -> 
                    accounts:delegate(
                      From, ID, Coins)
            end
    end;
doit({send, _, _, _, 2, CoinHours, To}, From) ->
    %send coin-hours. NOT coins.
    ok = accounts:charge(
           From, CoinHours + settings:api_cost()),
    if
        is_integer(To) -> 
            accounts:change_coin_hours(
              To, CoinHours);
        true ->
            case pubkeys:read(To) of
                error -> 
                    NewID = accounts:delegate_new(
                              From, 0, CoinHours),
                    {ok, NewID};
                {ok, ID} -> 
                    accounts:change_coin_hours(
                      ID, CoinHours),
                    {ok, 0}
            end
    end,
    {ok, 0};
doit({x, _, _, _, 1, NewName}, From) ->
%* choose an account name. (you get refunded coins for making your name shorter)
    true = is_binary(NewName),
    ok = accounts:charge(From, settings:api_cost()),
    accounts:change_name(From, NewName),
    {ok, 0};
doit({x, _, _, _, 2, NewDescription}, From) ->
%* choose a description for your account.(you get refunded coins for making your description shorter.)
    true = is_binary(NewDescription),
    ok = accounts:charge(From, settings:api_cost()),
    accounts:change_description(From, NewDescription),
    {ok, 0};
doit({x, _, _, _, 3, Leader}, From) ->
%* follow an account.
    true = is_integer(Leader),
    ok = accounts:charge(From, settings:api_cost()),
    accounts:follow(From, Leader),
    {ok, 0};
doit({x, _, _, _, 4, Text}, From) ->
%* make a post.
    true = is_binary(Text),
    ok = accounts:charge(From, settings:api_cost()),
    PID = posts:new(Text, From),
    accounts:make_post(From, PID),
    {ok, 0};
doit({x, _, _, _, 5, Text, Parent}, From) ->
%* make a comment
    true = is_binary(Text),
    true = is_integer(Parent),
    ok = accounts:charge(From, settings:api_cost()),
    case posts:comment(Text, From, Parent) of
        <<"invalid parent">> -> {error, <<"you tried to commented on a post that does not exist in the database.">>};
        PID ->
            accounts:make_post(From, PID),
            {ok, 0}
    end;
doit({x, _, _, _, 6, PID, Amount, Direction}, From) ->
%* upvote/downvote (vote) a post. post id, amount to vote with. (potentially changes the list of post ids ordered by popularity in the author's account).
    true = is_integer(Amount),
    ok = accounts:charge(From, settings:api_cost()),
    {ok, Post} = posts:read(PID),
    Author = posts:author(Post),
    case Direction of
        1 -> 
            accounts:make_vote(
              From, PID, Amount, up),
            accounts:post_voted(
              Author, PID, Amount, 0),
            posts:upvote(PID, Amount);
        -1 -> 
            accounts:make_vote(
              From, PID, Amount, down),
            accounts:post_voted(
              Author, PID, 0, Amount),
            posts:downvote(PID, Amount)
    end,
    {ok, 0};
doit({x, _, _, _, 7, PID}, From) ->
%* delete a vote
    %free.
    {Dhate, Dlove} = accounts:remove_vote(From, PID),
    posts:downvote(PID, Dhate),
    posts:upvote(PID, Dlove),
    {ok, 0};
doit({x, _, _, _, 8, N}, From) ->
%* scale back all votes. (is costs according to how many votes you have.)
    %N is between 0 and 1000000, to encode the rational value N/1000000.
    ok = accounts:charge(From, settings:api_cost()),
    accounts:scale_votes(From, N),
    {ok, 0};
doit({x, _, _, _, 9, Min}, From) ->
    %remove the votes that have less than Min value locked in them.
    ok = accounts:charge(From, settings:api_cost()),
    accounts:remove_small_votes(From, Min),
    {ok, 0};
doit({x, _, _, _, 10, To, Msg, CoinHours}, From) ->
%* send a DM, with an optional coin-hour lockup. (costs more for longer messages, and if the expiration is further in the future.)
    true = is_integer(To),
    ok = accounts:charge(From, settings:api_cost() + 
                        CoinHours),
    MID = dms:send(Msg, From, To, CoinHours),
    accounts:send_dm(From, To, MID),
    {ok, 0};
doit({x, _, _, _, 11, MID}, From) ->
%* check DM. (costs coins, because now the data is yours to delete) (refunds coin-hours to sender based on how long until the refund)
    ok = accounts:charge(From, settings:api_cost()),
    case dms:read(MID) of
        error ->
            {error, <<"no message with that id">>};
        DM ->
            case dms:from(DM) of
                From ->
                    dms:mark_as_read(MID),
                    accounts:mark_read_dm(
                      From, dms:to(DM), MID),
                    {ok, dms:content(DM)};
                _ -> {error, <<"not your message to delete">>}
            end
    end;
doit({x, _, _, _, 12, MID, 1}, From) ->
%* unlock DM deposit
    ok = accounts:charge(From, settings:api_cost()),
    DM = dms:read(MID),
    case dms:from(DM) of
        From ->
            L = dms:lockup(DM),
            TS = dms:timestamp(DM),
            L2 = accounts:new_balance(
                   L, TS, height_tracker:check(), 0),
            dms:unlock(MID),
            accounts:change_coin_hours(From, L2);
        _ -> {error, <<"not your message to unlock the deposit ">>}
    end;
doit({x, _, _, _, 12, MID, 0}, From) ->
%* burn DM deposit
    ok = accounts:charge(From, settings:api_cost()),
    DM = dms:read(MID),
    case dms:from(DM) of
        From ->
            dms:unlock(MID);
        _ -> {error, <<"not your message to burn the deposit">>}
    end;
doit({x, _, _, _, 13, AID}, From) ->
%* un-delegate coins.
    ok = accounts:charge(From, settings:api_cost()),
    accounts:undelegate(From, AID),
    {ok, 0};
doit({x, _, _, _, 14, PID}, From) ->
%* lookup post by id
    ok = accounts:charge(From, settings:api_cost()),
    posts:read(PID);
doit({x, _, _, _, 15, AID}, From) ->
%* chronological posts by accounts 
    ok = accounts:charge(From, settings:api_cost()),
    {ok, accounts:posts(AID)};
doit({x, _, _, _, 16, AIDs}, From) ->
%posts from all these accounts. (costs by how many posts are loaded)
    ok = accounts:charge(
           From, settings:api_cost()*(1 + length(AIDs))),
    Posts = lists:map(fun(X) -> accounts:posts(X) end,
                      AIDs),
    {ok, Posts};
doit({x, _, _, _, 17, AID}, From) ->
%* list of accounts that an account follows.
    ok = accounts:charge(
           From, settings:api_cost()),
    {ok, accounts:following(AID)};
doit({x, _, _, _, 18, L, AID}, From) ->
%* given list of accounts L, and an account A, returns subset of L that are followers of A.
    ok = accounts:charge(
      From, settings:api_cost() * length(L)),
    L2 = lists:filter(
           fun(X) ->
                   case accounts:following(X) of
                       {error, _} -> false;
                       A -> is_in(AID, A)
                   end
           end, L),
    {ok, L2};
doit({x, _, _, _, 19, AID}, From) 
  when is_integer(AID) ->
%* unfollow an account. (refunds coins)
    ok = accounts:charge(From, settings:api_cost()),
    accounts:unfollow(From, AID);
doit({x, _, _, _, 20, AID}, From) ->
%* list of who delegates to you.
    case accounts:balance_read(AID) of
        error -> {error, <<"account does not exist">>};
        {ok, A} ->
            {ok, accounts:delegated_by(A)}
    end;
doit({x, _, _, _, 21, AID}, From) ->
%* list of who you delegate to.
    case accounts:balance_read(AID) of
        error -> {error, <<"account does not exist">>};
        {ok, A} ->
            {ok, accounts:delegated_to(A)}
    end;
doit({x, _, _, _, 22, AID, MID}, From) ->
    %delete a dm. free.
    case accounts:balance_read(AID) of
        error -> {error, <<"account does not exist">>};
        {ok, A} ->
            case dms:read(MID) of
                {ok, M} ->
                    From = dms:from(M),
                    To = dms:to(M),
                    case AID of
                        From -> ok;
                        To -> %return the locked funds
                            L = dms:lockup(M),
                            TS = dms:timestamp(M),
                            L2 = accounts:new_balance(
                                   L, TS, 
                                   height_tracker:check(), 
                                   0),
                            accounts:change_coin_hours(
                              From, L2)
                    end,
                    dms:delete(MID),
                    case dms:was_read(M) of
                        1 -> 
                            accounts:remove_read_dm(
                              From, To, MID);
                        0 -> 
                            accounts:remove_unread_dm(
                              From, To, MID)
                    end;
                {error, E} ->
                    {error, E};
                _ -> error
            end
    end;
doit({x, _, _, _, 23, AID, MID}, From) ->
    %unlock deposit from DM
    case accounts:balance_read(AID) of
        error -> {error, <<"account does not exist">>};
        {ok, A} ->
            case dms:read(MID) of
                {ok, M} ->
                    From = dms:from(M),
                    AID = dms:to(M),
                    L = dms:lockup(M),
                    TS = dms:timestamp(M),
                    L2 = accounts:new_balance(
                           L, TS, 
                           height_tracker:check(), 
                           0),
                    accounts:change_coin_hours(
                      From, L2),
                    dms:unlock(MID),
                    {ok, 0};
                _ -> {error, <<"no message with that id">>}
            end
    end;
doit({x, _, _, _, 24, AID, MID}, From) ->
    %burn deposit from DM
    case accounts:balance_read(AID) of
        error -> {error, <<"account does not exist">>};
        {ok, A} ->
            case dms:read(MID) of
                {ok, M} ->
                    AID = dms:to(M),
                    dms:unlock(MID),
                    {ok, 0};
                _ -> {error, <<"no message with that id">>}
            end
    end;
doit({x, _, _, _, 25, PID}, From) ->
    %delete a post
    case accounts:balance_read(From) of
        error -> {error, <<"account does not exist">>};
        {ok, A} ->
            case posts:read(PID) of
                error -> {error, <<"that post does not exist">>};
                {ok, P}->
                    accounts:remove_post(From, PID),
                    posts:delete(PID),
                    {ok, 0}
            end
    end;

doit({x, _, _, _, 26, AID, Them}, From) ->
    %undelegate coins from someone.
    accounts:undelegate(AID, Them),
    {ok, 0};
doit({x, _, _, _, 27, AID}, From) ->
    %list of ids of dms related to you.
    case accounts:balance_read(AID) of
        error -> {error, <<"account does not exist">>};
        {ok, A} ->
            {accounts:sent_unread_dms(A),
             accounts:sent_read_dms(A),
             accounts:received_unread_dms(A),
             accounts:received_read_dms(A)}
    end;
doit(X, _) ->
    io:fwrite("signed handler doit fail"),
    io:fwrite(X),
    <<"error- unsupported http request.">>.

is_in(_, []) -> false;
is_in(A, [A|_]) -> true;
is_in(A, [_|T]) -> is_in(A, T).
