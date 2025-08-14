# III - Robustez

---

## 1.0 Robustez

Hay varios problemas en el ejemplo del messenger del capítulo **A Larger Example**. Por ejemplo:

- Si un nodo donde un usuario está logueado se cae sin hacer `logoff`, ese usuario **permanece en la lista de usuarios del servidor**, pero **su cliente desaparece**. Esto hace imposible que ese usuario vuelva a iniciar sesión, ya que el servidor **cree que sigue conectado**.
    
- ¿Y qué pasa si el servidor se cae en medio de enviar un mensaje? Esto deja al cliente esperando **para siempre** en la función `await_result`.
    

## **1.1 Time-outs (Tiempos de espera)**

Antes de mejorar ese programa messenger, vamos a ver algunos principios generales usando el ejemplo clásico de **ping pong**.

En este ejemplo:

- Cuando "ping" termina, le manda el átomo `finished` a "pong" para avisarle que puede terminar.
    
- **Otra forma de terminar "pong"** es que salga si **no recibe un mensaje** de "ping" en un tiempo determinado.
    

Esto se hace añadiendo un **time-out** en "pong", como se ve en este ejemplo:

```erlang
-module(tut19).

-export([start_ping/1, start_pong/0,  ping/2, pong/0]).

ping(0, Pong_Node) ->
    io:format("ping finished~n", []);

ping(N, Pong_Node) ->
    {pong, Pong_Node} ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_Node).

pong() ->
    receive
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    after 5000 ->
            io:format("Pong timed out~n", [])
    end.

start_pong() ->
    register(pong, spawn(tut19, pong, [])).

start_ping(Pong_Node) ->
    spawn(tut19, ping, [3, Pong_Node]).
```

### **Explicación**

- `pong()` espera un mensaje `{ping, Ping_PID}`.
    
- Si lo recibe, imprime un mensaje, responde con `pong` y repite.
    
- Si **pasan 5000 milisegundos** sin recibir nada, imprime `Pong timed out`.
    

**La clave es este bloque:**

```erlang
after 5000 ->
    io:format("Pong timed out~n", [])
```

Esto inicia un **timer de 5 segundos** cuando se entra al `receive`.  
Si se recibe `{ping, PID}` antes, el timer se cancela y sigue normal.  
Si no, ejecuta la acción después de 5 segundos.

También se puede hacer dinámico:

```erlang
after pong_timeout() ->
```

Aunque en sistemas distribuidos Erlang, **es mejor supervisar procesos** que usar timeouts, salvo para **eventos externos** (como desconectar usuarios inactivos por 10 minutos, por ejemplo).

---

## 2.0 Manejo de Errores**

Antes de ver cómo Erlang maneja la supervisión y errores, entendamos cómo **termina un proceso** (o como se dice en Erlang, **cómo hace "exit"**).

- Si un proceso ejecuta `exit(normal)` o simplemente termina su ejecución, hace un **exit normal**.
    
- Si ocurre un error en tiempo de ejecución (dividir por cero, mal match, llamar una función que no existe, etc.), hace un **exit anormal**.
    
- Si ejecuta `exit(Razon)` donde **`Razon` no es `normal`**, también es un **exit anormal**.
    

### **Links (Enlaces entre procesos)**

Un proceso puede crear un **enlace bidireccional** con otro proceso usando:

```erlang
link(Other_Pid)
```

Cuando un proceso termina:

- **Envía una señal** a todos los procesos enlazados, indicando:
    
    - **De quién viene**
        
    - **La razón del exit**
        

**Comportamiento por defecto:**

- Si recibe `exit(normal)`, **se ignora**.
    
- Si recibe un `exit anormal`:
    
    1. Ignora todos los mensajes pendientes.
        
    2. Mata al proceso que recibe la señal.
        
    3. Propaga la señal de error a los procesos que estaban enlazados a este último.
        

**De esta forma**, todos los procesos de una transacción pueden estar enlazados, y si uno falla, **todos caen**.

Para crear y enlazar un proceso a la vez:

```erlang
spawn_link(Mod, Fun, Args)
```

---

### **Ejemplo con link**

```erlang
-module(tut20).

-export([start/1,  ping/2, pong/0]).

ping(N, Pong_Pid) ->
    link(Pong_Pid),
    ping1(N, Pong_Pid).

ping1(0, _) ->
    exit(ping);

ping1(N, Pong_Pid) ->
    Pong_Pid ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping1(N - 1, Pong_Pid).

pong() ->
    receive
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start(Ping_Node) ->
    PongPID = spawn(tut20, pong, []),
    spawn(Ping_Node, tut20, ping, [3, PongPID]).
```

### **Explicación**

- Ambos procesos se lanzan desde `start/1`.
    
- `ping` se puede lanzar en otro nodo.
    
- `ping` usa `exit(ping)` al terminar, lo que **envía una señal de exit** a `pong`, que por estar enlazado, **también muere**.
    

---

### **Trapping de exits**

Se puede cambiar el comportamiento por defecto con:

```erlang
process_flag(trap_exit, true)
```

Esto hace que, en lugar de morir, los procesos reciban los exits como mensajes normales:

```erlang
{'EXIT', FromPID, Reason}
```

**Ojo**: Esto no es común en programas normales, sino en **supervisores OTP**.  
Aquí lo usamos solo para ilustrar.

---

### **Ejemplo con trapping**

```erlang
-module(tut21).

-export([start/1,  ping/2, pong/0]).

ping(N, Pong_Pid) ->
    link(Pong_Pid), 
    ping1(N, Pong_Pid).

ping1(0, _) ->
    exit(ping);

ping1(N, Pong_Pid) ->
    Pong_Pid ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping1(N - 1, Pong_Pid).

pong() ->
    process_flag(trap_exit, true), 
    pong1().

pong1() ->
    receive
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong1();
        {'EXIT', From, Reason} ->
            io:format("pong exiting, got ~p~n", [{'EXIT', From, Reason}])
    end.

start(Ping_Node) ->
    PongPID = spawn(tut21, pong, []),
    spawn(Ping_Node, tut21, ping, [3, PongPID]).
```

### **Explicación**

- `pong` pone `trap_exit` en `true`.
    
- Cuando `ping` hace `exit(ping)`, se recibe el mensaje:
    

```erlang
{'EXIT', FromPID, Reason}
```

- En lugar de morir, `pong` muestra:
    

```erlang
pong exiting, got {'EXIT',<PID>,ping}
```

---
## Ejemplo extendido con robustez:
[Ejemplo extendido](../II%20-%20Programacion%20Concurrente/Ejemplos/Messenger%20Robustez/README.md)


---

## Listo para el ultimo capitulo?
[IV - Records & Macros](../IV%20-%20Records%20&%20Macros/README.md)

---