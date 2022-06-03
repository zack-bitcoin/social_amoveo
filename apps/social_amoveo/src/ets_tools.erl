-module(ets_tools).
-export([location/1, 
         load_ets/2, load_ets/1,
         save_table/2, save_table/1,
         read/2, write/3]).

location(ID) ->
    atom_to_list(ID) ++ ".db".

load_ets(ID) ->
    load_ets(ID, location(ID)).

load_ets(ID, Loc) ->
    %Loc = location(ID),
    case ets:info(?MODULE) of
        undefined ->
            case ets:file2tab(Loc) of
                {ok, ID} -> ok;
                {error, R} ->
                    ets:new(ID, [set, named_table, {write_concurrency, false}, compressed])
            end;
        _ -> ok
    end,
    Top = case ets:lookup(ID, top) of
              [{top, N}|_] -> N;
              [] -> 1
          end,
    Top.

save_table(ID) ->
    save_table(ID, location(ID)).

save_table(ID, Loc) ->
    case ets:tab2file(ID, Loc, [{sync, true}]) of
        ok -> ok;
        {error, R} ->
            save_table(ID, Loc)
    end.

read(DB, X) ->
    case ets:lookup(DB, X) of
        [] -> error;
        [{X, A}] -> {ok, A}
    end.

write(DB, ID, V) ->
    ets:insert(DB, [{ID, V}]).
    
            
