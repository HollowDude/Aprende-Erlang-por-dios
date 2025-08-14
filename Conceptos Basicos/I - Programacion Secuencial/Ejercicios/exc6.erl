-module(exc6).
-export([ping/1,pong/0,start/0]).

ping(0) ->
    pong ! finished,
    io:format("Ping se acabo~n", []);
ping(N) ->
    pong ! {ping, self()},
    receive
        pong ->
            io:format("Ping recibio Pong~n", [])
    end,
    ping(N-1).

pong() ->
    receive 
        finished ->
            io:format("Se termino el Pong", []);
        {ping, Ping} ->
            io:format("Pong recibio Ping~n", []),
            Ping ! pong,
            pong()
    end.

start() ->
    register(pong, spawn(exc6, pong, [])),
    spawn(exc6, ping, [3]).