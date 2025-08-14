%I really need to refine this heaping pill of shit(Not now)
-module(exc2).
-export([sum/1, filter/2]).

sum([Head|Rest]) ->
    sum(Rest, Head).

sum([], L) ->
    io:format("La suma de to eso es: ~n~w~n", [L]),
    L;
sum([Head|Rest], L) ->
    New_L = Head + L,
    sum(Rest, New_L).


filter([Head|Rest], N) when Head > N->
    filter(Rest, [Head], N);
filter([Head|Rest], N) ->
    filter(Rest, [N], N).

filter([], List, N)->
    List;
filter([Head|Rest], List, N) when Head > N ->
    filter(Rest,[Head|List], N);
filter([Head|Rest],List, N) ->
    filter(Rest,[List],N).

