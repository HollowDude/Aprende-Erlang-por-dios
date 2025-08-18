-module(my_server).
-export([start/2, start_link/2, call/2, cast/2, reply/2]).

%APi publica:
start(Module, InitialState) ->
    spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
    spawn_link(fun() -> init(Module, InitialState) end).

call(Pid, Msg) ->
    Ref = monitor(process, Pid),
    Pid ! {sync, self(), Ref, Msg},
    receive
        {ref, Reply} ->
            demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            error(Reason)
        after 5000 ->
            error(timeout)
    end.

cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.

reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.

%Cosas privadas:
init(Module, InitialState) ->
	loop(Module, Module:init(InitialState)).
	loop(Module, State) ->
	receive
		{async, Msg} ->
			loop(Module, Module:handle_cast(Msg, State));
		{sync, Pid, Ref, Msg} ->
			loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
	end.

