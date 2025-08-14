-module(atoms).
-export([convert/2]).

convert(X, inches) ->
    io:format("Este es en pies:~w~n", [X / 2.54]);

convert(X, centimeter) ->
    io:format("Este es en cms:~w~n", [X * 2.54]).
