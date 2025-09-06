-module(ex2).
-compile(export_all).

init() ->
    spawn(?MODULE, worker, []).

sup(Pid) ->
    process_flag(trap_exit, true),
    link(Pid),
    receive
        {'EXIT', Pid, normal} ->
            oknormal;
        {'EXIT', Pid, _} ->
            io:format("Problemita aca michama"),
            Pid2 = init(),
            sup(Pid2)
    end.

worker() ->
    timer:sleep(5000),
    worker().