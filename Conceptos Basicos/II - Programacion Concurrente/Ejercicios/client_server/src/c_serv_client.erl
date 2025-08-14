-module(c_serv_client).
-export([login/1, logout/0, listar/0]).

-include_lib("include/common.hrl").

logout() ->
    case whereis(?Client_Reg_Name) of
        undefined ->
            io:format("Ya ese user no esta autenticado o jamas lo estuviste~n", []);
        PID ->
            PID ! logout
    end.

login(Name) ->
    case whereis(?Client_Reg_Name) of
        undefined ->
            register(?Client_Reg_Name, spawn(fun() -> client_loop(?Server_Reg_Name, Name) end));
        _ ->
            io:format("Ya esta un usuario registrado en ese nodo~n", [])
    end.

listar() ->
    case whereis(?Client_Reg_Name) of
        undefined ->
            io:format("Ya ese user no esta autenticado o jamas lo estuviste~n", []);
        PID ->
            PID ! listar
    end.

client_loop(_Server_Node, Name) ->
    {?Server_Reg_Name, ?Server_Node} ! ?Msg_login(self(), Name),
    await_login(),
    client_loop(?Server_Node).

client_loop(Server_Node) ->
    receive
        logout ->
            {?Server_Reg_Name, Server_Node} ! ?Msg_logout(self()),
            exit(normal);
        listar ->
            {?Server_Reg_Name, Server_Node} ! ?Msg_listar(self()),
            receive
                ?Rsp_Listar(List) ->
                    io:format("Users registrados:~p~n", [List])
            end,
            client_loop(Server_Node)
    end.

await_login() ->
    receive
        ?Server_Stop(XQ) ->
            io:format("Se a detenido el proceso, motivo:~n~p~n", [XQ]),
            exit(normal);
        ?Server_Login ->
            io:format("Logeado!~n", [])
    after 30000  ->
        io:format("Timeout del servidor~n", []),
        exit(timeout)
    end.
