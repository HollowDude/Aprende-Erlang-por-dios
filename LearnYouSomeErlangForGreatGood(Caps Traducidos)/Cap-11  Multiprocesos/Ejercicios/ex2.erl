-module(ex2).
-compile(export_all).

start() -> spawn(?MODULE, loop, []).

send(Pid, Msg) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Msg},
    receive
        {Ref, Replay} ->
            Replay
        after 2000 ->
            timeout
    end.

loop() ->
    receive
        {From, Ref, {msg, Text}} ->
            From ! {Ref, "Recibido:" ++ Text},
            loop();
        {_From, _Ref, {secret, _Text}} ->
            loop()
    end.