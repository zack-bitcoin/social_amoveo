-module(dms).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         read/1, mark_as_read/1, delete/1, 
         send/3, send/4, unlock/1, cost/1,

         lockup/1,to/1, timestamp/1,

         test/0]).

-record(x, {id, 
            content = <<>>, 
            read = 0,
            from, %account id
            to, %account id
            lockup,
            timestamp
            }).

lockup(X) ->
    X#x.lockup.
to(X) ->
    X#x.to.
timestamp(X) ->
    X#x.timestamp.

cost(X) ->
    settings:dm_cost() +
        (size(X#x.content) * 
             settings:coins_per_bytes()).
    

init(ok) -> 
    process_flag(trap_exit, true),
    Top = ets_tools:load_ets(?MODULE),
    {ok, Top}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, Top) -> 
    ets:insert(?MODULE, [{top, Top}]),
    ets_tools:save_table(?MODULE),
    io:format("DMs died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({unlock, ID}, X) -> 
    case ets:lookup(?MODULE, ID) of
        [] -> ok;
        [{ID, Msg}|_] ->
            Msg2 = Msg#x{lockup = 0},
            ets:insert(?MODULE, [{ID, Msg2}])
    end,
    {noreply, X};
handle_cast({delete, ID}, X) -> 
    ets:delete(?MODULE, ID),
    {noreply, X};
handle_cast({mark, ID}, X) -> 
    case ets:lookup(?MODULE, ID) of
        [] -> ok;
        [{ID, Msg}|_] ->
            Msg2 = Msg#x{read = 1},
            ets:insert(?MODULE, [{ID, Msg2}])
    end,
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call({send, Text, From, To, Lockup}, 
            _From, Top) -> 
    Msg = #x{id = Top, 
             content = Text,
             from = From,
             to = To,
             lockup = Lockup},
    ets:insert(?MODULE, [{Top, Msg}]),
    {reply, Top, Top+1};
handle_call({read, ID}, _From, X) -> 
    M = case ets:lookup(?MODULE, ID) of
            [] -> <<"no message with that id">>;
            [{ID, Msg}|_] ->
                Msg 
        end,
    {reply, M, X};
handle_call(_, _From, X) -> {reply, X, X}.


read(ID) ->
    gen_server:call(?MODULE, {read, ID}).
mark_as_read(ID) ->
    gen_server:cast(?MODULE, {mark, ID}).
delete(ID) ->
    gen_server:cast(?MODULE, {delete, ID}).
send(Msg, From, To) ->
    send(Msg, From, To, 0).
send(Msg, From, To, Lockup) 
  when (is_binary(Msg) and
        is_integer(Lockup)) ->
    gen_server:call(?MODULE, 
                    {send, Msg, From, To, Lockup}).
unlock(ID) ->
    gen_server:cast(?MODULE, {unlock, ID}).
    

test() ->
    Acc1 = 1,
    Acc2 = 2,
    MID = send(<<"hi">>, Acc1, Acc2, 0),
    M1 = read(MID),
    mark_as_read(MID),
    M2 = read(MID),
    delete(MID),
    M3 = read(MID),
    {M1, M2, M3}.
