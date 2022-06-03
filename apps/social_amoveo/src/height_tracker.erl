-module(height_tracker).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        check/0, update/1]).
-define(LOC, "height_tracker.db").
db_save(F, X) -> file:write_file(F, term_to_binary(X)).
db_read(F) ->
    case file:read_file(F) of
        {ok, <<>>} -> <<>>;
        {ok, Out} -> binary_to_term(Out);
        {error, enoent} -> 
            %io:fwrite("file does not exist\n"),
            "";
        {error, Reason} -> Reason
    end.
init(ok) -> 
    process_flag(trap_exit, true),
    X = db_read(?LOC),
    Y = if
            (X == "") -> 0;
            true -> X
        end,
    {ok, Y}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, N) -> 
    db_save(?LOC, N),
    io:format("died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({update, N}, X) -> 
    {noreply, max(N, X)};
handle_cast(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.

check() ->
    gen_server:call(?MODULE, check).
update(N) when is_integer(N) ->
    gen_server:cast(?MODULE, {update, N}).

