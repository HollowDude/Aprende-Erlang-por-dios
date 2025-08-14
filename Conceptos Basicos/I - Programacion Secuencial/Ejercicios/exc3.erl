-module(exc3).
-export([pos/1]).

pos(N) ->
    if
        N > 0 ->
            io:format("Es mayor que 0 el ~w~n",[N]);
        N < 0 ->
            io:format("Es menor que 0 el ~w~n", [N]);
        true ->
            io:format("PAPA ES 0!!!!!!!!!!!!")
            
    end.

