-module(test1).
-export([start/0, say_some/2, say_some/1]).

say_some(0) ->
    done.
say_some(Some, Times) ->
    io:format("~w~n~w~n", [Some, Times]),
    if 
        Times-1==0 ->
            say_some(0);
        true ->
            say_some(Some, Times)
    end.

start() ->
    spawn(test1, say_some, [latranca, 4]),
    spawn(test1, say_some, [latrancota, 5]).