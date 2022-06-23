-module(popular_posts_cache).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
read/0, cron/0]).

%-define(period, 60000).%once per minute
-define(period, 2000).%once per 2 seconds.
-define(size, 256).

init(ok) -> {ok, {[], []}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({store, L, H}, _) -> 
    {noreply, {L, H}};
handle_cast(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.

store(L, H) when is_list(L) and is_list(H) ->
    gen_server:cast(?MODULE, {store, L, H}).
read() ->
    gen_server:call(?MODULE, {read}).


cron() ->
    timer:sleep(?period),
    spawn(fun() ->
                  {x, T} = 
                      gen_server:call(posts, ok),
                  {Loved, Hated} = 
                      cron2(1, T, 0, [], 0, []),
                  store(Loved, Hated)
          end),
    cron().
cron2(ID, ID, _, Loved, _, Hated) -> 
    {Loved, Hated};
cron2(ID, Limit,
      CutoffL, Loved, 
      CutoffH, Hated) ->
    case posts:read(ID) of
        error ->
            cron2(ID+1, Limit, 
                  CutoffL, Loved,
                  CutoffH, Hated);
        {ok, Post} ->
            UV = posts:upvotes(Post),
            DV = posts:downvotes(Post),
            Loved2 = 
                if
                    (UV >= CutoffL) ->
                       insert(loved, Post, Loved);
                    true -> Loved
                end,
            CutoffL2 = 
                if
                    (length(Loved2) >= ?size) ->
                        posts:upvotes(
                          lists:last(Loved2));
                    true -> CutoffL
                end,
            Hated2 = 
                if
                    (DV >= CutoffH) ->
                        insert(hated, Post, Hated);
                    true -> Hated
                end,
            CutoffH2 = 
                if
                    (length(Hated2) >= ?size) ->
                        posts:downvotes(
                          lists:last(Hated2));
                    true -> CutoffH
                end,
            cron2(ID+1, Limit, 
                  CutoffL2, Loved2,
                  CutoffH2, Hated2)
    end.

insert(Type, Post, L) ->
    L2 = remove_post_from_list(posts:id(Post), L),
    L3 = if
             (length(L2) == ?size) ->
                 tl(L2);
              true -> L2
         end,
    insert2(Type, Post, L3).
remove_post_from_list(_, []) ->
    [];
remove_post_from_list(ID, [Post|T]) ->
    ID2 = posts:id(Post),
    if
        (ID == ID2) -> T;
        true -> [Post|remove_post_from_list(ID, T)]
    end.

insert2(_, P, []) -> [P];
insert2(loved, Post, 
        [SP|T]) ->
    SUV = posts:upvotes(SP),
    UV = posts:upvotes(Post),
    if
        (SUV >= UV) ->
            [Post, SP|T];
        true -> [SP|insert2(loved, Post, T)]
    end;
insert2(hated, Post,
       [SP|T]) ->
    SDV = posts:downvotes(SP),
    DV = posts:downvotes(Post),
    if
        (SDV >= DV) ->
            [Post, SP|T];
        true -> [SP|insert2(hated, Post, T)]
    end.
            
                                 
                    
