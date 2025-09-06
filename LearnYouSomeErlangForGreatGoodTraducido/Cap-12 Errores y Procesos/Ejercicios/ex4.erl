-module(ex4).
-compile(export_all).

sup() ->
    spawn_monitor(worker, []).

worker() ->
    receive
      _ ->
        worker()
    end.