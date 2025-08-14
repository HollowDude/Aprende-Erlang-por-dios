-module(eco).
-export([start/0, send_msg/1]).

eco_server() ->
    receive
      {echo, From, Msg} ->
        From ! {echo_replay, Msg},
        eco_server()
    end.

start() ->
  case whereis(eco_server) of
    undefined ->
      Pid = spawn(fun eco_server/0),
      register(eco_server, Pid),
      io:format("Server hechado a andar.~n");
    _ ->
      io:format("Ya existe ese server~n", [])
  end.

send_msg(Msg) ->
  case whereis(eco_server) of
    undefined ->
      io:format("El server no esta andando~n", []);
    Pid ->
      Pid ! {echo, self(), Msg},
      receive
        {echo_replay, Rsp} ->
          io:format("Respondio con: ~p~n", [Rsp])
      end 
  end.