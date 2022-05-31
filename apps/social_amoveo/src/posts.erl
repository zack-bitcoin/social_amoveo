-module(posts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

-record(post, 
        {id, %number bigger than 0
         text = "", 
         author, %account id
         timestamp,%when posted
         upvotes = 0,
         downvotes = 0,
         comments = [], %ids of other posts
         parent = 0 %id of parent, or 0 if it is top level
        }).

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
    io:format("died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.
