-module(c_serv).
-behaviour(gen_server).

-export([start_server/0, server_node/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("include/common.hrl").

start_server() ->
    case whereis(?Server_Reg_Name) of
        undefined ->
            {ok, PID} = gen_server:start_link({local, ?Server_Reg_Name}, ?MODULE, [], []),
            PID;
        _ ->
            io:format("Proceso iniciado con previedad~n", []),
            already_started
    end.

server_node() -> ?Server_Node.

init([]) ->
    {ok, []}.

handle_call({login, Name}, {From, _}, State) ->
    io:format("Servidor recibió login de ~p con nombre ~p~n", [From, Name]),
    NewState = handle_login(From, Name, State),
    {reply, ok, NewState};

handle_call(listar, _From, State) ->
    {reply, State, State};

handle_call(Unknown, _From, State) ->
    io:format("Llamada desconocida: ~p~n", [Unknown]),
    {reply, {error, unknown_call}, State}.

handle_cast({logout, From}, State) ->
    NewState = handle_logout(From, State),
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_login(PID, Name, List) ->
    case lists:keymember(Name, 2, List) of
        true ->
            PID ! ?Server_Stop(usuario_existe_en_otro_nodo),
            List;
        false ->
            PID ! ?Server_Login,
            [{PID, Name} | List]
    end.    

handle_logout(PID, List) ->
    lists:keydelete(PID, 1, List). 
