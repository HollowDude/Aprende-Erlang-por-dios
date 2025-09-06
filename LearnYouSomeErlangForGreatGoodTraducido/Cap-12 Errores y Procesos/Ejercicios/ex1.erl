-module(ex1).
-compile(export_all).

a() -> 1/0.

b() -> 
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, a, []),
    receive
        {'EXIT',Pid, normal} ->
            ok;
        {'EXIT',Pid, _} ->
            candel
    end.

% 12> ex1:b().
% =ERROR REPORT==== 22-Aug-2025::23:59:11.897000 ===
% Error in process <0.113.0> with exit value:
% {badarith,[{ex1,a,0,[{file,"ex1.erl"},{line,4}]}]}
%
% candel