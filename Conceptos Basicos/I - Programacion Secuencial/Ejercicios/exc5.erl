-module(exc5).
-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("Se termino el PING");
ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive
        pong ->
            io:format("Ping recibio pong~n", [])
    end,
    ping(N-1, Pong_PID).

pong() ->
    receive 
        finished ->
            io:format("Se termino el PONG~n", []);
        {ping, Ping_PID} ->
            io:format("Pong recibio Ping~n", []),
            Ping_PID ! pong,
            pong()
    end. 

start() ->
    Pong_PID = spawn(exc5, pong, []),
    spawn(exc5, ping, [5, Pong_PID]).