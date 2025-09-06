-module(linkmon).
-compile(export_all).

chain(0) ->
    receive
      _ ->
        ok
      after
        2000 ->
          exit("Chain dies here")
    end;
chain(N) ->
    PID = spawn(fun() -> chain(N-1) end),
    link(PID),
    receive
      _ ->
        ok
    end.

start_critic() ->
    spawn(?MODULE, critic, []).

judge(PID, Band, Album) ->
    PID ! {self(), {Band, Album}},
    receive
        {PID, Criticism} -> Criticism
        after 2000 -> timeout
    end.

critic() ->
    receive
        %Pense en pensar en nombres en español graciosos pero que pereza papa
        {From, {"Rage Against the Turing Machine", "Unit Testify"}} ->
            From ! {"They are great!"};
        {From, {"System of a Downtime", "Memoize"}} ->
            From ! {"They're not Johnny Crash but they're good."};
        {From, {"Johnny Crash", "The Token Ring of Fire"}} ->
            From ! {"Simply incredible."};
        {From, {_Band, _Album}} ->
            From ! {"They are terrible!"}
    end,
    critic().

start_critic2() ->
    spawn(?MODULE, restarter, []).

restarter() ->
    process_flag(trap_exit, true),
    PID = spawn_link(?MODULE, critic, []),
    receive
        {'EXIT', PID, normal} -> %No es un error de crasheo
            ok;
        {'EXIT', PID, shutdown} -> %Finalizacion manual, tampoco es crasheo
            ok;
        {'EXIT', PID, _} ->
            restarter()
    end.

judge2(Band, Album) ->
    critic ! {self(), {Band, Album}},
    PID = whereis(critic),
    receive
        {PID, Criticism} -> Criticism
        after 2000 ->
            timeout
    end.

judge3(Band, Album) ->
    Ref = make_ref(),
    critic ! {self(), Ref, {Band, Album}},
    receive
        {Ref, Criticism} -> Criticism
        after 2000 ->
            timeout
    end.

critic2() ->
	receive
		{From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
			From ! {Ref, "They are great!"};
		{From, Ref, {"System of a Downtime", "Memoize"}} ->
			From ! {Ref, "They're not Johnny Crash but they're good."};
		{From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
			From ! {Ref, "Simply incredible."};
		{From, Ref, {_Band, _Album}} ->
			From ! {Ref, "They are terrible!"}
	end,
	critic2().