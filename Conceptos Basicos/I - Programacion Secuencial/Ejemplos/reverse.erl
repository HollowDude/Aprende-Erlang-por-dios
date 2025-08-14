-module(reverse).
-export([rev/1]).

rev(List)->
    rev(List, []).
rev([Head | Rest], R) ->
    %Solo para probar el throw:
    if 
        Head == 5 -> throw(no_se_puede)
    end,
    %** exception throw: no_se_puede
    %        in function  reverse:rev/2 (reverse.erl, line 9)
    rev(Rest, [Head | R]);
rev([], R)->
    R.