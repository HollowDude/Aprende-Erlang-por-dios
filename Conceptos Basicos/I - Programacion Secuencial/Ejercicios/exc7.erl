-module(exc7).
-export([ping/2, pong/0, start_ping/1, start_pong/0]).

ping(0, Pong_Node) -> 
    {pong, Pong_Node} ! finished,
    io:format("Ping se acabo~n", []);

ping(N, Pong_Node) ->
    {pong, Pong_Node} ! {ping, self()},
    receive
      pong ->
        io:format("Ping recibio Pong~n", [])
    after 5000 ->
      io:format("Ping timed out~n", [])
    end,
    ping(N-1, Pong_Node).
    

pong() ->
    receive
      finished ->
        io:format("Pong se acabo~n", []);
      {ping, Ping_PID} ->
        io:format("Pong recibio Ping~n", []),
        Ping_PID ! pong,
        pong()
    after 5000 ->
        io:format("Pong timed out~n", [])
    end.

start_pong() ->
    register(pong, spawn(exc7, pong, [])).

start_ping(Pong_Node) ->
    spawn(exc7, ping, [3, Pong_Node]).

