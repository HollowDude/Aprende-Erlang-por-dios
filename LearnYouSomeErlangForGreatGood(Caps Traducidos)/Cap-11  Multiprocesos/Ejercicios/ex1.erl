-module(ex1).
-export([start/0, send/2]).

start() ->
    spawn(fun eco/0).

eco() ->
    receive
       {From, {echo, Text}} ->
            From ! {ok, Text},
            eco()
    end.

send(Pid ,Msg) ->
    Pid ! {self(), {echo, Msg}},
    receive
        {ok, Reply} ->
            Reply
    after
        2000 ->
            timeout
    end.

