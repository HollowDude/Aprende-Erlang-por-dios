-module(ex7).
-compile(export_all).

sup() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, work, []),
    register(worker, Pid),
    receive
        {'EXIT', _Pid, 'normal'} ->
            ok;
        {'EXIT', _Pid, 'shutdown'} ->
            ok;
        {'EXIT', _Pid, Reason} ->
            io:format(Reason),
            sup()
    end.

work() ->
    Rand = rand:uniform(2),
    if
        Rand =:= 1 ->
            exit(crash);
        true ->
            ok
    end.

% Deberia loggear algo asi:
% 1> ex7:sup().
% crashok
% 2> ex7:sup().
% ok