-module(posts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         new/2, comment/3, upvote/2, downvote/2, 
         delete/1, read/1, test/0,
         cost/1]).

-record(post, 
        {id, %number bigger than 0
         text = <<"">>, 
         author, %account id
         timestamp,%when posted
         upvotes = 0,
         downvotes = 0,
         comments = [], %ids of other posts
         parent = 0 %id of parent, or 0 if it is top level
        }).

cost(P) ->
    settings:post_cost() +
        (settings:coins_per_byte() *
             size(P#post.text)).

init(ok) -> 
    process_flag(trap_exit, true),
    Top = ets_tools:load_ets(?MODULE),
    {ok, Top}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, Top) -> 
    ets:insert(?MODULE, [{top, Top}]),
    ets_tools:save_table(?MODULE),
    io:format("died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({delete, PID}, X) -> 
    ets:delete(?MODULE, PID),
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call({post, Text, Author}, _From, Top) -> 
    {T1, T2, _} = erlang:timestamp(),
    P = #post{
      id = Top,
      text = Text,
      author = Author,
      timestamp = {T1, T2, 0}
     },
    ets:insert(?MODULE, [{Top, P}]),
    {reply, Top, Top+1};
handle_call({comment, Text, Author, ParentID}, 
            _From, Top) -> 
    {T1, T2, _} = erlang:timestamp(),
    case ets:lookup(?MODULE, ParentID) of
        [] -> {reply, <<"invalid parent">>, Top};
        [{ParentID, Parent}|_] ->
            Parent2 = 
                Parent#post{
                  comments = 
                      [Top|Parent#post.comments]},
                       
            P = #post{
              id = Top,
              text = Text,
              author = Author,
              timestamp = {T1, T2, 0},
              parent = ParentID
             },
            ets:insert(?MODULE, [{Top, P}]),
            ets:insert(
              ?MODULE, [{ParentID, Parent2}]),
            {reply, Top, Top+1}
    end;
handle_call({vote, Type, PID, Amount}, _, Top) -> 
    case ets:lookup(?MODULE, PID) of
        [] -> 
            %error, vote on post that does not exist.
            {reply, <<"error, post does not exist">>, Top};
        [{PID, Post}|_] ->
            Post2 = 
                case Type of
                    1 -> Post#post{
                           upvotes = Post#post.upvotes + Amount
                          };
                    -1 -> Post#post{
                            downvotes = Post#post.downvotes + Amount
                           }
                end,
            ets:insert(?MODULE, [{PID, Post2}]),
            {reply, success, Top}
    end;
handle_call(_, _From, X) -> {reply, X, X}.

new(Text, Author) when is_binary(Text) ->
    gen_server:call(?MODULE, {post, Text, Author}).
comment(Text, Author, Parent) when is_binary(Text) ->
    gen_server:call(
      ?MODULE, {comment, Text, Author, Parent}).

upvote(PID, Amount) ->
    gen_server:call(
      ?MODULE, {vote, 1, PID, Amount}).
downvote(PID, Amount) ->
    gen_server:call(
      ?MODULE, {vote, -1, PID, Amount}).
delete(PID) ->
    gen_server:cast(?MODULE, {delete, PID}).
read(PID) ->
    case ets:lookup(?MODULE, PID) of
        [] -> error;
        [{PID, Post}|_] -> {ok, Post}
    end.

test() ->
    Acc1 = 1,
    P1 = new(<<"first post">>, Acc1),
    C1 = comment(<<"a comment">>, Acc1, P1),
    C2 = comment(<<"a second comment">>, Acc1, P1),
    lists:map(fun(X) -> {ok, Y} = read(X), Y end,
              [P1, C1, C2]).
