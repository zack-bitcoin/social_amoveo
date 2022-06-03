-module(accounts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,

         %paralisable reading
         read/1, read/2,
         following/1, dms/1,
         delegated_to/1,delegated_by/1,
         posts/1, votes/1,

         %asynchronous editing
         update_nonce/2,
         delegate/3,%delegate value to an existing account
         delegate_new/3,%delegate coins and/or coin-hours to create a new account
         delegate_new/4,
         pay_interest/1,%update the coin-hours balance after a period of time. Removing coins that expired, and paying out new coin-hours based on the number of coins held.
         make_post/2, remove_post/2,
         make_vote/3, remove_vote/2, 
         follow/2, unfollow/2,
         send_dm/3, mark_read_dm/3, 
         remove_unread_dm/3, remove_read_dm/3,
         change_coin_hours/2,
         change_name/2,
         change_description/2,
         scale_votes/2,
         remove_small_votes/2,
         update_veo_balance/2,
         new_account/0,
         test/0,

         block_cron/0]).

-record(acc, 
        {%id, pubkey, 
         name = <<"">>, description = <<"">>, 
         timestamp, %block when coin-hours was last calculated.
         veo = 0, %how many veo your account owns in the Amoveo blockchain.
         delegated = 0, %positive or negative amount that this account has delegated to others.
         %net_coins = veo + delegated - coins_in_votes
         coin_hours = 0, 
         nonce = 0, %used for signing api requests.
         coins_in_votes = 0, 
         coins_in_posts = 0,
         coins_in_dms = 0,
         following = [], %account ids
         sent_unread_dms = [], %mid
         sent_read_dms = [],
         received_unread_dms = [],
         received_read_dms = [],
         delegated_to = [],%{account id, balance} 
         delegated_by = [], %{account id, balance} they delegate to us.
         posts = [], %{post_id, timestamp, upvotes, downvotes} posts that we authored, in chronological order, recent posts first.
         votes = [] %{post_id, amount, timestamp}
        }).


init(ok) -> 
    process_flag(trap_exit, true),
    Top = ets_tools:load_ets(?MODULE),
    {ok, Top}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, Top) -> 
    ets:insert(?MODULE, [{top, Top}]),
    ets_tools:save_table(?MODULE),
    io:format("accounts died!\n"), 
    ok.
    
handle_info(_, X) -> {noreply, X}.

handle_cast({delegate, AID, To, Amount}, Top) -> 
    %the account we are delegating to already exists.
    true = Amount > 0,
    case ets_read(AID) of
        error -> ok;
        {ok, A1} ->
            case ets_read(To) of
                error -> ok;
                {ok, A2} ->
                    ets_write(
                      AID, 
                      A1#acc{
                        delegated = 
                            A1#acc.delegated +
                            Amount,
                        delegated_to = 
                            [{To, Amount}|
                             A1#acc.delegated_to]
                       }),
                    ets_write(
                      To, 
                      A2#acc{
                        delegated = 
                            A2#acc.delegated -
                            Amount,
                        delegated_by = 
                            [{AID, Amount}|
                             A2#acc.delegated_by]
                       })
            end
    end,
    {noreply, Top};
handle_cast({update_nonce, AID, Nonce}, X) -> 
    case ets_read(AID) of
        error -> ok;
        {ok, A} ->
            A2 = A#acc{
                   nonce = max(Nonce, 
                               A#acc.nonce)},
            ets_write(AID, A2)
    end,
    {noreply, X};
handle_cast({send_dm, From, To, MID}, X) ->
    io:fwrite("send dm internal\n"),
    case {ets_read(From), ets_read(To)} of
        {error, _} -> ok;
        {_, error} -> ok;
        {{ok, F}, {ok, T}} ->
            io:fwrite("send dm 2\n"),
            F2 = F#acc{
                   sent_unread_dms = 
                       [MID|F#acc.sent_unread_dms],
                   coins_in_dms =
                       F#acc.coins_in_dms +
                       settings:dm_cost()
                  },
            T2 = T#acc{
                   received_unread_dms = 
                       [MID|T#acc.received_unread_dms]
                  },
            ets_write(From, F2),
            ets_write(To, T2)
    end,
    {noreply, X};
handle_cast({mark_read_dm, From, To, MID}, X) ->
    case ets_read(From) of
        error -> ok;
        {ok, F} ->
            case remove_mid(
                   MID, F#acc.sent_unread_dms) of
                error -> ok;
                {ok, Unread} ->
                    F2 = F#acc{
                           sent_read_dms = 
                               [MID|F#acc.sent_read_dms],
                           sent_unread_dms = Unread,
                           coins_in_dms = 
                               F#acc.coins_in_dms -
                               settings:dm_cost()
                          },
                    ets_write(From, F2)
            end
    end,
    case ets_read(To) of
        error -> ok;
        {ok, T} ->
            case remove_mid(MID, T#acc.received_unread_dms) of
                error -> ok;
                {ok, Unread2} ->
                    T2 = T#acc{
                           received_unread_dms = Unread2,
                           received_read_dms = 
                               [MID|T#acc.received_read_dms],
                           coins_in_dms = 
                               T#acc.coins_in_dms +
                               settings:dm_cost()
                          },
                    ets_write(To, T2)
            end
    end,
    {noreply, X};
handle_cast({remove_unread_dm, From, To, MID}, X) ->
    case ets_read(From) of
        error -> ok;
        {ok, F} ->
            case remove_mid(
                   MID, F#acc.sent_unread_dms) of
                error -> ok;
                {ok, Unread} ->
                    F2 = F#acc{
                           sent_unread_dms = 
                               Unread,
                           coins_in_dms = 
                               F#acc.coins_in_dms -
                               settings:dm_cost()
                          },
                    ets_write(From, F2)
            end
    end,
    case ets_read(To) of
        error -> ok;
        {ok, T} ->
            case remove_mid(MID, T#acc.received_unread_dms) of
                error -> ok;
                {ok, Unread2} ->
                    T2 = T#acc{
                           received_unread_dms = 
                               Unread2
                          },
                    ets_write(To, T2)
            end
    end,
    {noreply, X};
handle_cast({remove_read_dm, From, To, MID}, X) ->
    case ets_read(From) of
        error -> ok;
        {ok, F} ->
            case remove_mid(
                   MID, F#acc.sent_read_dms) of
                error -> ok;
                {ok, Read} ->
                    F2 = F#acc{
                           sent_read_dms = Read
                          },
                    ets_write(From, F2)
            end
    end,
    case ets_read(To) of
        error -> ok;
        {ok, T} ->
            case remove_mid(MID, T#acc.received_read_dms) of
                error -> ok;
                {ok, Read2} ->
                    T2 = T#acc{
                           received_read_dms = 
                               Read2,
                           coins_in_dms =
                               T#acc.coins_in_dms +
                               settings:dm_cost()
                          },
                    ets_write(To, T2)
            end
    end,
    {noreply, X};
handle_cast({follow, AID, Leader}, X) ->
    case ets_read(AID) of
        error -> ok;
        {ok, A} ->
            A2 = A#acc{
                   following = 
                       [Leader|A#acc.following],
                   coins_in_posts = 
                       A#acc.coins_in_posts + 
                       settings:follow_cost()
                  },
            ets_write(AID, A2)
    end,
    {noreply, X};
handle_cast({unfollow, AID, Leader}, X) ->
    case ets_read(AID) of
        error -> ok;
        {ok, A} ->
            case remove_leader(Leader, A#acc.following) of
                error -> ok;
                {ok, F2} ->
                    A2 = 
                        A#acc{following = F2,
                              coins_in_posts = 
                                  A#acc.coins_in_posts -
                                  settings:follow_cost()},
                    ets_write(AID, A2)
            end
    end,
    {noreply, X};
handle_cast({vote, AID, PID, Amount}, X) ->
    case ets_read(AID) of
        error -> ok;
        {ok, A} ->
            {TSA, TSB, _} = 
                erlang:timestamp(),
            TS = {TSA, TSB, 0},
            A2 = A#acc{
                   votes = [{PID, Amount, TS}|A#acc.votes],
                   coins_in_votes = 
                       A#acc.coins_in_votes + 
                       Amount
                  },
            ets_write(AID, A2)
    end,
    {noreply, X};
handle_cast({remove_vote, AID, PID}, X) -> 
    case ets_read(AID) of
        error -> ok;
        {ok, A} ->
            case grab_vote(PID, A#acc.votes) of
                error -> ok;
                {{_, Amount, _}, Rest} ->
                    A2 = A#acc{
                           votes = Rest,
                           coins_in_votes =
                               A#acc.coins_in_votes - 
                               Amount
                          },
                    ets_write(AID, A2)
            end
    end,
    {noreply, X};
handle_cast({post, AID, PID}, X) -> 
    case ets_read(AID) of
        error -> ok;
        {ok, A} ->
            {TSA, TSB, _} = 
                erlang:timestamp(),
            TS = {TSA, TSB, 0},
            A2 = A#acc{
                   posts = [{PID, TS, 0, 0}|
                            A#acc.posts],
                   coins_in_posts = 
                       A#acc.coins_in_posts +
                       settings:post_cost()
                  },
            ets_write(AID, A2)
    end,
    {noreply, X};
handle_cast({remove_post, AID, PID}, X) -> 
    case ets_read(AID) of
        error -> ok;
        {ok, A} ->
            case remove_post_from_list(
                   PID, A#acc.posts) of
                error -> ok;
                {ok, Posts2} ->
                    A2 = A#acc{
                           posts = Posts2,
                           coins_in_posts = 
                               A#acc.coins_in_posts -
                               settings:post_cost()
                          },
                    ets_write(AID, A2)
            end
    end,
    {noreply, X};
handle_cast({interest, AID}, X) -> 
    case read(AID) of
        error -> ok;
        {ok, A} ->
            ets_write(AID, A)
    end,
    {noreply, X};
handle_cast({change_coin_hours, AID, D}, X) -> 
    case ets_read(AID) of
        error -> ok;
        {ok, A} ->
            A2 = A#acc{
                   coin_hours = 
                       A#acc.coin_hours + D
                  },
            ets_write(AID, A2)
    end,
    {noreply, X};
handle_cast({change_name, AID, Name}, X) -> 
    case ets_read(AID) of
        error -> ok;
        {ok, A} ->
            OldName = A#acc.name,
            D = size(Name) - size(OldName),
            Cost = settings:coins_per_byte() * D,
            A2 = A#acc{
                   name = Name,
                   coins_in_posts = 
                       A#acc.coins_in_posts + Cost
                  },
            ets_write(AID, A2)
    end,
    {noreply, X};
handle_cast({change_description, AID, Description},
            X) -> 
    case ets_read(AID) of
        error -> ok;
        {ok, A} ->
            OldDes = A#acc.description,
            D = size(Description) - size(OldDes),
            Cost = settings:coins_per_byte() * D,
            A2 = A#acc{
                   description = Description,
                   coins_in_posts = 
                       A#acc.coins_in_posts + Cost
                  },
            ets_write(AID, A2)
    end,
    {noreply, X};
handle_cast({scale_votes, AID, N}, X) -> 
         %votes = [] %{post_id, amount, timestamp}
    case ets_read(AID) of
        error -> ok;
        {ok, A} ->
            {NewVotes, RecoveredCoins} =
                scale_internal(A#acc.votes, N),
            A2 = A#acc{
                   votes = NewVotes,
                   coins_in_votes =
                       A#acc.coins_in_votes -
                       RecoveredCoins
                  },
            ets_write(AID, A2)
    end,
    {noreply, X};
handle_cast({remove_small_votes, AID, Min}, X) -> 
    case ets_read(AID) of
        error -> ok;
        {ok, A} ->
            {NewVotes, RecoveredCoins} =
                remove_small_internal(
                  A#acc.votes, Min),
            A2 = A#acc{
                   votes = NewVotes,
                   coins_in_votes = 
                       A#acc.coins_in_votes -
                       RecoveredCoins
                  },
            ets_write(AID, A2)
    end,
    {noreply, X};
handle_cast({update_veo_balance, AID, NewBalance},
            X) -> 
    case read(AID) of
        error -> ok;
        {ok, A} ->
            A2 = A#acc{
                   veo = NewBalance
                  },
            ets_write(AID, A2)
    end,
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call({new_account, Height}, _, X) -> 
    case ets_read(X) of
        {ok, _} -> 
            io:fwrite("this should never happen. in account, new_account."),
            ok;
        error ->
            A = #acc{
              timestamp = Height
             },
            ets_write(X, A)
    end,
    {reply, X, X+1};
handle_call({delegate_new, AID, Coins, 
             CoinHours, Height}, 
            _From, Top) -> 
    %for the case where we are creating an account that doesn't exist in our database yet.
    case ets_read(AID) of
        error -> {reply, {error, <<"your account does not exist">>}, 
                  Top};
        {ok, A1} -> 
            CoinsA1 = balance(A1),
            NewCoinHoursA1 = 
                A1#acc.coin_hours - 
                CoinHours,
            if
                NewCoinHoursA1 < 0 ->
                    {reply, {error, <<"you don't have that many coin-hours to spend">>}, 
                     Top};
                CoinsA1 < Coins ->
                    {reply, {error, <<"you don't have that many coins to delegate">>},
                     Top};
                true ->
                    ets_write(
                      AID, 
                      A1#acc{
                        coin_hours = 
                            NewCoinHoursA1,
                        delegated = 
                            A1#acc.delegated +
                            Coins,
                        delegated_to = 
                            [{Top, Coins}|
                             A1#acc.delegated_to]
                       }),
                    ets_write(
                      Top, #acc{
                        coin_hours = CoinHours,
                        delegated = -Coins,
                        delegated_by = 
                            [{AID, Coins}],
                        timestamp = Height
                       }),
                    {reply, Top, Top+1}
            end
    end;
handle_call(_, _From, X) -> {reply, X, X}.

read(AID) ->
    Height = height_tracker:check(),
    read(AID, Height).
read(AID, Height) ->
    %reads an account from the database, and updates the coin-hours to be current
    case ets_read(AID) of
        error -> error;
        {ok, A} ->
            #acc{
          coin_hours = CoinHours,
          timestamp = TS
         } = A,
            Coins = balance(A),
            TS2 = max(Height, TS),
            {ok, A#acc{
              coin_hours = 
                  new_balance(CoinHours, TS, TS2, Coins),
              timestamp = TS2}}
    end.
new_balance(CoinHours, TS1, TS2, Coins) ->
%formula for updating coin-hours.
    P = CoinHours,%previous balance of coins hours.
    C = Coins,%coins balance
    T = max(TS2 - TS1, 0),%time that passed.
    H = 1000,%Half Life of 1000 blocks is about a week.
    P + (T*(C - P) div H).
    
    

ets_read(AID) ->
    ets_tools:read(?MODULE, AID).
ets_write(AID, Val) ->
    ets_tools:write(?MODULE, AID, Val).
balance(Acc) ->
    #acc{
         veo = Veo,
         delegated = Delegated,
         coins_in_votes = CIV,
         coins_in_posts = CIP,
         coins_in_dms = CID
     } = Acc,
    Veo - Delegated - CIV - CIP - CID.
            
update_nonce(AID, Nonce) ->
    gen_server:cast(?MODULE, 
                    {update_nonce, AID, Nonce}).
delegate(AID, To, Amount) 
  when is_integer(To) and is_integer(Amount) ->
    gen_server:cast(
      ?MODULE, 
      {delegate, AID, To, Amount}).
delegate_new(AID, Coins, CoinHours) ->
    Height = height_tracker:check(),
    delegate_new(AID, Coins, CoinHours, Height).
delegate_new(AID, Coins, CoinHours, Height)
  when (is_integer(Coins) and
        is_integer(CoinHours)) ->
    gen_server:call(
      ?MODULE,
      {delegate_new, AID, Coins, CoinHours, Height}).
pay_interest(AID) ->
    gen_server:cast(?MODULE, {interest, AID}).
make_post(AID, PID) ->
    gen_server:cast(
      ?MODULE, {post, AID, PID}).
remove_post(AID, PID) ->
    gen_server:cast(
      ?MODULE, {remove_post, AID, PID}).
make_vote(AID, PID, Amount) ->
    gen_server:cast(
      ?MODULE, {vote, AID, PID, Amount}).
remove_vote(AID, PID) ->
    gen_server:cast(
      ?MODULE, {remove_vote, AID, PID}).
follow(AID, Leader) ->
    gen_server:cast(
      ?MODULE, {follow, AID, Leader}).
unfollow(AID, Leader) ->
    gen_server:cast(
      ?MODULE, {unfollow, AID, Leader}).
send_dm(From, To, MID) ->
    gen_server:cast(
      ?MODULE, {send_dm, From, To, MID}).
mark_read_dm(From, To, MID) ->
    gen_server:cast(
      ?MODULE, {mark_read_dm, From, To, MID}).
remove_unread_dm(From, To, MID) ->
    gen_server:cast(
      ?MODULE, {remove_unread_dm, From, To, MID}).
remove_read_dm(From, To, MID) ->
    gen_server:cast(
      ?MODULE, {remove_read_dm, From, To, MID}).
change_coin_hours(AID, D) when is_integer(D)->
    gen_server:cast(
      ?MODULE, {change_coin_hours, AID, D}).
change_name(AID, Name) when is_binary(Name) ->
    gen_server:cast(
      ?MODULE, {change_name, AID, Name}).
change_description(AID, D) when is_binary(D) ->
    gen_server:cast(
      ?MODULE, {change_description, AID, D}).
scale_votes(AID, N) when 
      is_integer(N) and 
      (N > -1) and 
      (N < 1000000) ->
    gen_server:cast(?MODULE, {scale_votes, AID,N}).
remove_small_votes(AID, Min) when
      is_integer(Min) and (Min>0) ->
    gen_server:cast(?MODULE, {remove_small_votes,
                              AID, Min}).
update_veo_balance(AID, NewBalance) when
      is_integer(NewBalance) ->
    gen_server:cast(?MODULE, {update_veo_balance,
                              AID,
                              NewBalance}).
new_account() ->
    Height = height_tracker:check(),
    gen_server:call(?MODULE, 
                    {new_account, Height}).

    
    
    

following(AID) ->
    case ets_read(AID) of
        error -> 
            {error, <<"account doesn't exist">>};
        {ok, A} -> A#acc.following
    end.
dms(AID) ->
    case ets_read(AID) of
        error -> 
            {error, <<"account doesn't exist">>};
        {ok, A} -> {A#acc.sent_unread_dms,
                    A#acc.sent_read_dms,
                    A#acc.received_unread_dms,
                    A#acc.received_read_dms}
    end.
delegated_to(AID) ->
    case ets_read(AID) of
        error -> 
            {error, <<"account doesn't exist">>};
        {ok, A} -> A#acc.delegated_to
    end.
delegated_by(AID) ->
    case ets_read(AID) of
        error -> 
            {error, <<"account doesn't exist">>};
        {ok, A} -> A#acc.delegated_by
    end.
posts(AID) ->
    case ets_read(AID) of
        error -> 
            {error, <<"account doesn't exist">>};
        {ok, A} -> A#acc.posts
    end.
votes(AID) ->
    case ets_read(AID) of
        error -> 
            {error, <<"account doesn't exist">>};
        {ok, A} -> A#acc.votes
    end.

remove_post_from_list(PID, L) ->
    rpfl2(PID, L, []).
rpfl2(I, [{I, _, _, _}|T], R) ->
    {ok, lists:reverse(R) ++ T};
rpfl2(_, [], _) ->
    error;
rpfl2(I, [H|T], R) ->
    rpfl2(I, T, [H|R]).

remove_leader(Leader, L) ->
    remove_leader2(Leader, L, []).
remove_leader2(X, [X|R], F) ->
    {ok, lists:reverse(F) ++ R};
remove_leader2(X, [A|R], F) ->
    remove_leader2(X, R, [A|F]);
remove_leader2(_, [], _) ->
    error.

grab_vote(PID, Votes) ->
    gv2(PID, Votes, []).
gv2(I, [{I, A, B}|T], R) ->
    {{I, A, B}, lists:reverse(R) ++ T};
gv2(I, [H|T], R) ->
    gv2(I, T, [H|R]);
gv2(_, [], _) ->
    error.

scale_internal(Votes, N) ->
    si2(Votes, N, [], 0).
si2([], _, R, C) ->
    {lists:reverse(R), C};
si2([{PID, Amount, TS}|Votes], N, R, C) ->
    Rest = Amount * N div 1000000,
    Recovered = Amount - Rest,
    si2(Votes, N, [{PID, Rest, TS}|R],C+Recovered).

remove_small_internal(Votes, Min) ->
    rsi2(Votes, Min, [], 0).
rsi2([], _, R, C) ->
    {lists:reverse(R), C};
rsi2([{PID, Amount, _}|Votes], Min, R, C) 
  when (Amount < Min) ->
    rsi2(Votes, Min, R, C+Amount);
rsi2([X|Votes], Min, R, C) ->
    rsi2(Votes, Min, [X|R], C).

remove_mid(MID, L) ->
    rm2(MID, L, []).
rm2(_MID, [], _R) ->
    error;
rm2(MID, [MID|T], R) ->
    {ok, lists:reverse(R) ++ T};
rm2(MID, [A|T], R) ->
    rm2(MID, T, [A|R]).


                          


block_cron() ->
    timer:sleep(5000),
    block_scan(),
    block_cron().
unused() ->
    spawn(fun() ->
   %               timer:sleep(5000),
                  timer:sleep(5000)
                  %block_cron()
          end),
    spawn(fun() ->
                  timer:sleep(5000),
                  block_scan(),
                  block_cron()
          end).
block_scan() ->
    X = utils:talk({height}),
    case X of
        {ok, Height} ->
            Start2 = max(0, scan_height:read()),
%    spawn(fun() ->
            scan_history(Start2, Height+1);
        _ -> ok
    end.
scan_history(N, M) when N >= M -> ok;
scan_history(Start, End) -> 
    E2 = min(End, Start+50),
    {ok, Blocks} = utils:talk({blocks, Start, E2}),
    case length(Blocks) of
        1 -> 
            %io:fwrite("done scanning tx history\n"),
            ok;
        _ ->
            io:fwrite("scanning blocks at \n"),
            io:fwrite(integer_to_list(Start)),
            io:fwrite(" - "),
            io:fwrite(integer_to_list(End)),
            io:fwrite("\n"),
            %load_blocks(Blocks),
            %load_txs(Blocks),
            load_blocks(Blocks),
            LastBlock = lists:nth(length(Blocks), Blocks),
            LastHeight = element(2, LastBlock),
            scan_history(LastHeight + 1, End)
            %scan_history(LastHeight, End)
    end.

   
load_blocks([]) -> ok;
load_blocks([Block|T]) -> 
    %J = element(15, Block) is meta. stored as a stringified JSON object.
    height_tracker:update(element(2, Block)),
    J1 = element(15, Block),
    case J1 of
        <<"">> -> ok;
        _ ->
            J = jiffy:decode(J1),
            load_txs(
              element(2, hd(element(1, J))))
    end,
    load_blocks(T).

load_txs([]) -> ok;
load_txs([{[{<<"type">>,<<"account">>},
            {<<"pubkey">>,P},
            {<<"balance">>, B}|
            _]}|T]) -> 
    AID = case pubkeys:read(P) of
             error -> 
                  ID = new_account(),
                  pubkeys:new(P, ID),
                  ID;
             {ok, N} -> N
         end,
    update_veo_balance(AID, B),
    load_txs(T);
load_txs([_|T]) -> 
    load_txs(T).

%height_check() ->
%    case utils:talk({height}) of
%        {ok, H} -> H;
%        _ -> 0
%    end.

test() ->
    AID1 = new_account(),
    update_veo_balance(AID1, 13300), 
    update_nonce(AID1, 2),
    change_coin_hours(AID1, 12000),
   
    AID2 = delegate_new(AID1, 150, 0),
    change_coin_hours(AID2, 15000),

    make_post(AID1, 4),
    make_post(AID1, 5),
    remove_post(AID1, 4),
    
    make_vote(AID2, 5, 25),
    remove_vote(AID2, 5),
    make_vote(AID2, 5, 20),

    follow(AID2, AID1),
    unfollow(AID2, AID1),
    follow(AID2, AID1),

    unfollow(AID1, AID2),
    follow(AID2, AID1),

    send_dm(AID2, AID1, 6),
    send_dm(AID2, AID1, 7),
    send_dm(AID2, AID1, 8),
    send_dm(AID2, AID1, 9),
    mark_read_dm(AID2, AID1, 6),
    mark_read_dm(AID2, AID1, 7),
    remove_read_dm(AID2, AID1, 6),
    remove_unread_dm(AID2, AID1, 8),


    change_name(AID1, <<"alice">>),
    change_description(AID1, <<"first user">>),
   
    
    make_vote(AID1, 11, 10),
    make_vote(AID1, 12, 15),
    make_vote(AID1, 13, 20),
    make_vote(AID1, 14, 25),
    scale_votes(AID1, 1000000 div 5),
    remove_small_votes(AID1, 3),
    timer:sleep(300),
    {AID1, AID2,
     read(AID1, 0),
     read(AID2, 0),
     following(AID2),
     dms(AID1),
     delegated_to(AID1),
     delegated_by(AID2),
     posts(AID1),
     votes(AID2)}.
