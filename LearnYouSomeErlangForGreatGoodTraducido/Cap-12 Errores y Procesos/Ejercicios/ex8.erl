-module(ex8).
-compile(export_all).
-record(client, {pid, monitor}).

start() ->
    Pid = spawn(?MODULE, dispatcher, [[]]),
    register(dispatcher, Pid).

dispatcher(Clientes) ->
    receive
        {connect, Pid} ->
            Ref = monitor(process, Pid),
            Pid ! {self(), conectado},
            dispatcher([#client{pid = Pid, monitor = Ref}|Clientes]);
        {'DOWN', Ref, process, Pid, Reason} ->
            io:format("Se callo el proceso: ~w~n", [Pid]),
            io:format("Por razon de: ~w~n", [Reason]),
            demonitor(Ref, [flush]),
            dispatcher(lists:delete(#client{pid = Pid, monitor = Ref}, Clientes))
    end.

start_client() ->
    spawn(?MODULE, client, []).

client() ->
    Pid = whereis(dispatcher),
    Pid ! {connect, self()},
    receive
        die ->
            exit(muerte)
    end.

% 2> Pid5=ex8:start().
% true
% 3> Cl1=ex8:start_client().
% <0.131.0>
% 4> Cl2=ex8:start_client().
% <0.133.0>
% 5> Cl1 ! die.
% Se callo el proceso: <0.131.0>
% die
% Por razon de: muerte
    