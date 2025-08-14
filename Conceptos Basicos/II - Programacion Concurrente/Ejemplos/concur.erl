-module(concur).
-export([start/0, say/2]).

say(What, 0) ->
done;
say(What, Times) ->
    io:format("Digo que ~w~n", [What]),
    say(What, Times - 1).


start() ->
    spawn(concur, say, [candela, 3]),
    spawn(concur, say, [ejlapinga, 999]).