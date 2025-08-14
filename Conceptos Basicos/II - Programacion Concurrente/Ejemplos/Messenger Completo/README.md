

---

# 📚 **Erlang Messenger** — Análisis Detallado

---

## 📌 **¿Qué es esto?**

Un programa en Erlang donde:

- Los **clientes** se conectan a un **servidor central** para **loguearse** con un nombre.
    
- Luego pueden **enviar mensajes** entre ellos, sin tener que saber en qué nodo está conectado cada uno.
    
- Cada nodo corre una instancia de Erlang.
    

**Nota**: Esto es _mensaje a mensaje_, no hay interfaz bonita, y hay fallos reconocidos (por ejemplo, qué pasa si un nodo se cae).

---

## 📌 **Estructura Básica**

### **Arquitectura**

- Un **servidor** central registrado como `messenger`.
    
- Cada **cliente** en cada nodo registrado como `mess_client`.
    
- Los procesos usan `send (!)` y `receive` para comunicarse.
    

### **Módulo y Exportaciones**

```erlang
-module(messenger).
-export([start_server/0, server/1, logon/1, logoff/0, message/2, client/2]).
```

---

## 📌 **Funciones Clave**

### **server_node()**

Devuelve el nombre del nodo donde está el servidor.

```erlang
server_node() ->
    messenger@super.
```

➡️ Cambiar este valor según dónde esté el servidor.

---

### **Servidor**

#### **start_server/0**

Inicia el servidor registrándolo como `messenger`.

```erlang
start_server() ->
    register(messenger, spawn(messenger, server, [[]])).
```

#### **server/1**

Loop recursivo que espera mensajes:

```erlang
server(User_List) ->
    receive
        {From, logon, Name} ->
            New_User_List = server_logon(From, Name, User_List),
            server(New_User_List);
        {From, logoff} ->
            New_User_List = server_logoff(From, User_List),
            server(New_User_List);
        {From, message_to, To, Message} ->
            server_transfer(From, To, Message, User_List),
            io:format("list is now: ~p~n", [User_List]),
            server(User_List)
    end.
```

➡️ Aquí maneja **login**, **logout** y **mensajes**.

---

### **Logon**

```erlang
server_logon(From, Name, User_List) ->
    case lists:keymember(Name, 2, User_List) of
        true ->
            From ! {messenger, stop, user_exists_at_other_node},
            User_List;
        false ->
            From ! {messenger, logged_on},
            [{From, Name} | User_List]
    end.
```

➡️ **Si ya existe ese nombre**, lo rechaza.

---

### **Logoff**

```erlang
server_logoff(From, User_List) ->
    lists:keydelete(From, 1, User_List).
```

➡️ Elimina al cliente por su pid.

---

### **Transferencia de Mensajes**

#### **server_transfer/4**

Primero verifica que el emisor esté logueado.

```erlang
server_transfer(From, To, Message, User_List) ->
    case lists:keysearch(From, 1, User_List) of
        false ->
            From ! {messenger, stop, you_are_not_logged_on};
        {value, {From, Name}} ->
            server_transfer(From, Name, To, Message, User_List)
    end.
```

#### **server_transfer/5**

Envía el mensaje si encuentra al receptor.

```erlang
server_transfer(From, Name, To, Message, User_List) ->
    case lists:keysearch(To, 2, User_List) of
        false ->
            From ! {messenger, receiver_not_found};
        {value, {ToPid, To}} ->
            ToPid ! {message_from, Name, Message},
            From ! {messenger, sent}
    end.
```

---

### **Cliente**

#### **logon/1**

Crea y registra un cliente en el nodo.

```erlang
logon(Name) ->
    case whereis(mess_client) of
        undefined ->
            register(mess_client, 
                     spawn(messenger, client, [server_node(), Name]));
        _ -> already_logged_on
    end.
```

#### **logoff/0**

Envía `logoff` al cliente.

```erlang
logoff() ->
    mess_client ! logoff.
```

#### **message/2**

Envía un mensaje a otro usuario.

```erlang
message(ToName, Message) ->
    case whereis(mess_client) of
        undefined -> not_logged_on;
        _ -> mess_client ! {message_to, ToName, Message},
             ok
    end.
```

---

### **client/2**

Inicia sesión enviando logon al servidor.

```erlang
client(Server_Node, Name) ->
    {messenger, Server_Node} ! {self(), logon, Name},
    await_result(),
    client(Server_Node).
```

### **client/1**

Maneja logoff, mensajes salientes y entrantes.

```erlang
client(Server_Node) ->
    receive
        logoff ->
            {messenger, Server_Node} ! {self(), logoff},
            exit(normal);
        {message_to, ToName, Message} ->
            {messenger, Server_Node} ! {self(), message_to, ToName, Message},
            await_result();
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(Server_Node).
```

---

### **await_result/0**

Espera respuesta del servidor.

```erlang
await_result() ->
    receive
        {messenger, stop, Why} ->
            io:format("~p~n", [Why]),
            exit(normal);
        {messenger, What} ->
            io:format("~p~n", [What])
    end.
```

---

## 📌 **Conceptos Nuevos**

✔ **Funciones listas**

- `lists:keymember/3`, `lists:keydelete/3`, `lists:keysearch/3`
    
- Permiten buscar, eliminar o verificar elementos en listas de tuplas.
    

✔ **Recursión**

- `server/1` y `client/1` son procesos recursivos infinitos.
    

✔ **whereis/1**

- Busca un proceso registrado localmente.
    

✔ **spawn/3**

- Crea un nuevo proceso concurrente.
    

✔ **register/2**

- Asocia un nombre a un pid.
    

✔ **send (!)**

- Envia un mensaje asincrónico a un proceso.
    

✔ **receive**

- Espera mensajes en la cola de mensajes.
    

✔ **exit(normal)**

- Finaliza un proceso.
    

---

## 📌 **Caso de Uso Completo**

**Nodos**:

- `messenger@super` → servidor
    
- `c1@bilbo`, `c2@kosken`, `c3@gollum` → clientes
    

**Ejemplo**:

```shell
(messenger@super)1> messenger:start_server().
true

(c1@bilbo)1> messenger:logon(peter).
logged_on

(c2@kosken)1> messenger:logon(james).
logged_on

(c3@gollum)1> messenger:logon(fred).
logged_on

(c1@bilbo)2> messenger:message(fred, "hello").
sent

# Fred recibe: Message from peter: "hello"

(c3@gollum)2> messenger:message(peter, "go away, I'm busy").
sent

(c3@gollum)3> messenger:logoff().
logoff

(c2@kosken)2> messenger:message(fred, "peter doesn't like you").
receiver_not_found
```

---

## 📌 **Resumen**

👉 Este código es un **ejemplo de cómo Erlang gestiona sistemas distribuidos usando procesos ligeros y paso de mensajes**.

✔ No hay llamadas directas, todo es **send/receive**  
✔ Cada nodo mantiene su propio cliente  
✔ Un servidor central gestiona el estado de usuarios logueados  
✔ Uso básico de **listas de tuplas** y **funciones estándar**

---
