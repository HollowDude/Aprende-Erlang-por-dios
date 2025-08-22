-module(ex3).
-compile(export_all).

start() -> spawn(?MODULE, buzon, []).
    
send(Pid, Msg) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Msg},
    receive
        {Ref, Msg} ->
            Msg
    after 2000 ->
        timeout
    end.

buzon() ->
    receive
        {From, Ref, {priority, Text}}->
            io:format("Procesando PRIORITY: ~p~n", [Text]),
            From ! {Ref, {priority, Text}},
            buzon()
        after 0 ->
            buzon2()
    end.

buzon2() ->
    receive
        {From, Ref, Msg}->
            io:format("Procesando PRIORITY: ~p~n", [Msg]),
            From ! {Ref, Msg},
            buzon()
    end.