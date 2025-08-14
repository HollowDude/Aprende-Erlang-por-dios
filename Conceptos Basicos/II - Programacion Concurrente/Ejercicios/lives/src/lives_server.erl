-module(lives_server).
-export([start/1, loop/1, send/1]).

-include("lives_common.hrl").

start(Initial_Lives) ->
    case whereis(vidas_erl) of
        undefined ->
            PID = spawn(fun() -> loop(#state{lives=Initial_Lives}) end),
            register(vidas_erl, PID);
        _ ->
            io:format("Ya esta creado el proceso~n", [])
    end.


loop(State = #state{lives=Lives, poison=Poison}) ->
    Timeout = case Poison of
                true -> ?TIMEOUT_POISON;
                false -> ?TIMEOUT_NORMAL
            end,
    receive

      ?MSG_STAT -> 
        io:format("Vida actual:~p~n", [Lives]),
        loop(State);
      
      ?MSG_POISON ->
        io:format("Se formo la guaracha, estas envenenado, pierdes 4 de vida cada 3 secs", []),
        loop(State#state{poison=true});
      
      {?MSG_LOSE, N} ->
        New_Lives = Lives - N,
        if
            New_Lives =< 0 ->
                io:format("Muerto michama~n", []);
            true ->
                io:format("Perdio vida, cantidad:~p~n", [New_Lives]),
                loop(State#state{lives=New_Lives})
        end;

      {?MSG_GAIN, N} ->
        New_Lives = Lives+ N,
        io:format("Ganaste ~p vidas, que talla!~n Ahora tienes: ~p~n", [N, New_Lives]),
        loop(State#state{lives=New_Lives})

    after Timeout ->
        case Poison of
            true ->
                New_Lives = Lives - 4,
                if
                    New_Lives =< 0 ->
                        io:format("Muerto michama, te jamo el veneno~n", []);
                    true ->
                        io:format("El veneno te esta jamando con 4 de vida cada 3 secs, cantidad actual:~p~n", [New_Lives]),
                        loop(State#state{lives=New_Lives})
                end;
            false ->
                io:format("Estas dormio papa, se te cerro esta historia!~n", []),
                exit(normal)
        end
    end.

send(Msg) ->
    case whereis(vidas_erl) of
        undefined ->
            io:format("Proceso no iniciado michama~n", []),
            exit(normal);
        PID ->
            PID ! Msg
    end.