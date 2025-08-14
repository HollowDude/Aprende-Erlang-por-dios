-module(exc1).
-export([area/1, temp/2]).

area(R) ->
    io:format("El area de ese circulo es:~n~w~n",[math:pi()*R*R]).
temp(celc, C) ->
    C*9/5+32;
temp(far, F) ->
    (F-32)*5/9.