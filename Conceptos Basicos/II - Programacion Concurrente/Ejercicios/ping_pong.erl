-module(ping_pong).
-export([start/1, ping/2, pong/0]).

ping(Times, Pong_PID) ->
    link(Pong_PID),
    ping1(Times, Pong_PID).

ping1(0, _) ->
    exit(ping);

ping1(Times, Pong_PID) ->
    Pong_PID ! {pong, self()},
    receive
      pong ->
        io:format("Ping", [])
    after 2000 ->
        io:format("Inactividad, desconectado luego de 2 secs de espera", []) 
    end,
    ping1(Times-1, Pong_PID).

pong() ->
    receive
      {ping, Ping_PID} ->
        io:format("Pong", []),
        Ping_PID ! pong,
        pong()
    end.

start(Ping_Node) ->
    Pong_PID = spawn(ping_pong, pong, []),
    spawn(Ping_Node, ping_pong, ping, [2, Pong_PID]).