-module(messenger).
-export([server/1, start_server/0, login/1, logout/0, message/2, client/2]).

server_node() ->
    messenger@hollow.

server(UserList) ->
    receive
      {From, login, Name} ->
        NewUserList = server_login(From, Name, UserList),
        server(NewUserList);
      {From, logout} ->
        NewUserList = server_logout(From, UserList),
        server(NewUserList);
      {From, message_to, To, Message} ->
        server_transf(From, To, Message, UserList),
        io:format("Lista ahora es: ~n~p~n", [UserList]),
        server(UserList)
    end.

start_server() ->
    register(messenger, spawn(messenger, server, [[]])).

server_login(From, Name, UserList) ->
    case lists:keymember(Name, 2, UserList) of
        true ->
            From ! {messenger, stop, user_exists_at_other_node},
            UserList;
        false ->
            From ! {messenger, logged_in},
            [{From, Name} | UserList]
    end.

server_logout(From, UserList) ->
    lists:keydelete(From, 1, UserList).

server_transf(From, To, Message, UserList) ->
    case lists:keysearch(From, 1, UserList) of
        false ->
            From ! {messenger, stop, no_estas_logeado};
        {value, {From, Name}} ->
            server_transfer(From, Name, To, Message, UserList)
    end.

server_transfer(From, Name, To, Message, UserList) ->
    case lists:keysearch(To, 2, UserList) of
      false ->
        From ! {messenger, receiver_not_found};
      {value, {ToPID, To}} ->
        ToPID ! {message_from, Name, Message},
        From ! {messenger, sent}
end.

%User comandos:

login(Name) ->
    case whereis(mess_client) of
      undefined ->
        register(mess_client,
                spawn(messenger, client, [server_node(), Name]));
        _ -> ya_logeado
    end.

logout() ->
    mess_client ! logout.

message(ToName, Message) ->
    case whereis(mess_client) of
      undefined ->
        no_logeado;
      _ -> mess_client ! {message_to, ToName, Message},
      ok
    end.

client(Server_Node, Name) ->
    {messenger, Server_Node} ! {self(), login, Name},
    await_result(),
    client(Server_Node).

client(Server_Node) ->
    receive
      logout ->
        {messenger, Server_Node} ! {self(), logout},
        exit(normal);
      {message_to, ToName, Message} ->
        {messenger, Server_Node} ! {self(), message_to, ToName, Message},
        await_result();
      {message_from, FromName, Message} ->
        io:format("Mensaje recibido de ~p: ~n~p~n", [FromName, Message])
    end,
    client(Server_Node).

await_result() ->
    receive
      {messenger, stop, Why} ->
        io:format("~p~n", [Why]),
        exit(normal);
      {messenger, What} ->
        io:format("~p~n", [What])
    end.