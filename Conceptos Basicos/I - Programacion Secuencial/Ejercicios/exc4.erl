-module(exc4).
-export([start/0, mns/2]).
mns(M,0)->
    done;
mns(M, N) ->
    io:format("El mensaje es:~w~n", [M]),
    mns(M, N-1).

start() ->
    spawn(exc4, mns, [candelaaaaaaaaa, 2]),
    spawn(exc4, mns, [papaaaaaaaaaa, 5]),
    spawn(exc4, mns, [dPEPDPEPE, 2]),
    spawn(fun()-> io:format("La ultima que hice~n") end ).
