# 📒 Messenger — Ejemplo con Robustez

## 📌 Interfaz de Usuario

- `login(Name)` — Inicia sesión con un nombre único en la red.
    
- `logoff()` — Cierra sesión.
    
- `message(ToName, Message)` — Envía un mensaje a otro usuario.
    

---

## 📌 Estructura General

- **Servidor `messenger`**
    
    - Mantiene la lista `{Pid, Name}` de usuarios conectados.
        
    - Registrado como `messenger`.
        
- **Cliente `mess_client`**
    
    - Corre en cada nodo con usuario logueado.
        
    - Registrado localmente como `mess_client`.
        

---

## 📌 Protocolo Cliente-Servidor

- **Logon**
    
    - `{ClientPid, logon, UserName}` →
        
        - `{messenger, stop, user_exists_at_other_node}`
            
        - `{messenger, logged_on}`
            
- **Mensaje**
    
    - `{ClientPid, message_to, ToName, Message}` →
        
        - `{messenger, stop, you_are_not_logged_on}`
            
        - `{messenger, receiver_not_found}`
            
        - `{messenger, sent}`
            
- **Logoff / Desconexión**
    
    - `'EXIT', ClientPid, Reason` (detectado con `trap_exit`)
        

---

## 📌 Cambios de Robustez

✅ **Servidor `trap_exit`**  
Detecta desconexión o error del cliente:

- Logoff voluntario.
    
- Caída de red.
    
- Nodo apagado.
    
- Operación ilegal.
    

→ Elimina al cliente de la lista con `server_logoff/2`.

✅ **Propagación de fallos**  
Si el servidor cae, todos los clientes reciben `{'EXIT', MessengerPID, noconnection}` y terminan.

✅ **Timeout en cliente**

- `await_result/0` espera 5 segundos.
    
- Si no hay respuesta → termina con `timeout`.
    
- Especialmente útil en `logon`, antes del `link`.
    

✅ **Manejo de enlaces fallidos** Si el cliente muere antes de ser enlazado → `{'EXIT', From, noproc}`.

✅ **`whereis(mess_client)`**  
Evita logins simultáneos en el mismo nodo.

---

## 📌 Código Completo

```erlang
%%% messenger.erl — Mensajería distribuida robusta

-module(messenger).
-export([start_server/0, server/0, 
         logon/1, logoff/0, message/2, client/2]).

%%% Nodo del servidor messenger
server_node() ->
    messenger@super.

%%% Servidor principal
server() ->
    process_flag(trap_exit, true),
    server([]).

server(User_List) ->
    receive
        {From, logon, Name} ->
            New_User_List = server_logon(From, Name, User_List),
            server(New_User_List);
        {'EXIT', From, _} ->
            New_User_List = server_logoff(From, User_List),
            server(New_User_List);
        {From, message_to, To, Message} ->
            server_transfer(From, To, Message, User_List),
            io:format("list is now: ~p~n", [User_List]),
            server(User_List)
    end.

%%% Inicia el servidor
start_server() ->
    register(messenger, spawn(messenger, server, [])).

%%% Registrar usuario en servidor
server_logon(From, Name, User_List) ->
    case lists:keymember(Name, 2, User_List) of
        true ->
            From ! {messenger, stop, user_exists_at_other_node},
            User_List;
        false ->
            From ! {messenger, logged_on},
            link(From),
            [{From, Name} | User_List]
    end.

%%% Eliminar usuario del servidor
server_logoff(From, User_List) ->
    lists:keydelete(From, 1, User_List).

%%% Transferir mensaje
server_transfer(From, To, Message, User_List) ->
    case lists:keysearch(From, 1, User_List) of
        false ->
            From ! {messenger, stop, you_are_not_logged_on};
        {value, {_, Name}} ->
            server_transfer(From, Name, To, Message, User_List)
    end.

server_transfer(From, Name, To, Message, User_List) ->
    case lists:keysearch(To, 2, User_List) of
        false ->
            From ! {messenger, receiver_not_found};
        {value, {ToPid, To}} ->
            ToPid ! {message_from, Name, Message}, 
            From ! {messenger, sent}
    end.

%%% Comandos de usuario
logon(Name) ->
    case whereis(mess_client) of 
        undefined ->
            register(mess_client, 
                     spawn(messenger, client, [server_node(), Name]));
        _ -> already_logged_on
    end.

logoff() ->
    mess_client ! logoff.

message(ToName, Message) ->
    case whereis(mess_client) of
        undefined ->
            not_logged_on;
        _ -> mess_client ! {message_to, ToName, Message},
             ok
end.

%%% Proceso cliente
client(Server_Node, Name) ->
    {messenger, Server_Node} ! {self(), logon, Name},
    await_result(),
    client(Server_Node).

client(Server_Node) ->
    receive
        logoff ->
            exit(normal);
        {message_to, ToName, Message} ->
            {messenger, Server_Node} ! {self(), message_to, ToName, Message},
            await_result();
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(Server_Node).

%%% Esperar respuesta del servidor
await_result() ->
    receive
        {messenger, stop, Why} ->
            io:format("~p~n", [Why]),
            exit(normal);
        {messenger, What} ->
            io:format("~p~n", [What])
    after 5000 ->
            io:format("No response from server~n", []),
            exit(timeout)
    end.
```

---
