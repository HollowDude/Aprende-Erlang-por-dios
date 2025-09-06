-module(ex3).
-compile(export_all).

serv() ->
    process_flag(trap_exit, true),
    receive
        candel ->
            ok,
            serv();
        {'EXIT', _Pid, 'shutdown'} ->
            shutdown;
        {'EXIT', _Pid, _} ->
            io:format("Hubo problema ~n")
    end.