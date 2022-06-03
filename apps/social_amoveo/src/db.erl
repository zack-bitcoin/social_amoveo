-module(db).
-export([save/2, read/1]).

save(F, X) -> 
    file:write_file(F, term_to_binary(X)).
read(F) ->
    case file:read_file(F) of
        {ok, <<>>} -> <<>>;
        {ok, Out} -> binary_to_term(Out);
        {error, enoent} -> 
            %io:fwrite("file does not exist\n"),
            "";
        {error, Reason} -> Reason
    end.
