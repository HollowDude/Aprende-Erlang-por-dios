# II - Programacion Concurrente

## 1.0 Procesos

Erlang es un lenguaje funcional diseñado para manejar concurrencia y programación distribuida. La concurrencia permite ejecutar múltiples tareas simultáneamente, como lo hacen los sistemas operativos modernos. En Erlang, cada hilo de ejecución se llama **proceso**, ya que no comparten datos entre sí.

Para crear un nuevo proceso en Erlang, se usa la función `spawn`:

```erlang
spawn(Modulo, Funcion_Exportada, Lista_De_Argumentos).
```

Ejemplo:

```erlang
-module(tut14).
-export([start/0, say_something/2]).

say_something(What, 0) ->
    done;
say_something(What, Times) ->
    io:format("~p~n", [What]),
    say_something(What, Times - 1).

start() ->
    spawn(tut14, say_something, [hello, 3]),
    spawn(tut14, say_something, [goodbye, 3]).
```

Al ejecutar `start()`, se crean dos procesos que imprimen "hello" y "goodbye" tres veces cada uno, intercaladamente. La función `spawn` devuelve un identificador de proceso (PID), como `<0.63.0>`.

## 1.2 Paso de Mensajes

Los procesos en Erlang se comunican mediante **paso de mensajes**. Cada proceso tiene una cola de entrada donde recibe mensajes y los procesa con `receive`.

Ejemplo de comunicación entre procesos con _ping-pong_:

```erlang
-module(tut15).
-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("ping finished~n", []);
ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_PID).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start() ->
    Pong_PID = spawn(tut15, pong, []),
    spawn(tut15, ping, [3, Pong_PID]).
```

En este código, `start()` crea dos procesos:

- `pong()` espera mensajes. Si recibe `{ping, PID}`, responde con `pong`.
    
- `ping(N, Pong_PID)` envía `{ping, self()}` a `pong`, espera la respuesta `pong`, e itera hasta que N llegue a 0, cuando envía `finished` y termina.
    

El operador `!` se usa para enviar mensajes:

```erlang
PID ! Mensaje.
```

Los mensajes son cualquier término de Erlang: listas, tuplas, átomos, enteros, pids, etc.

Cuando un proceso ejecuta `receive`, busca el primer mensaje en su cola que coincida con un patrón. Si no hay coincidencias, el mensaje permanece en la cola hasta que otro `receive` lo procese. Si no hay mensajes disponibles, el proceso se bloquea hasta recibir uno.

Este modelo de concurrencia hace que Erlang sea eficiente para sistemas distribuidos y concurrentes.


## 2.0 ¿Qué es esto de **Distributed Programming** en Erlang?

La **programación distribuida** en Erlang no es un parche ni una librería externa. Es **parte nativa del lenguaje y su máquina virtual (BEAM)**. Eso significa que Erlang fue diseñado, desde su base, para permitir que programas corran en varias computadoras a la vez y se comuniquen como si fueran procesos locales.  
**¿Cómo?** Gracias a un sistema de nodos conectados por mensajes.

---

## Seguridad: **.erlang.cookie**

Para que dos nodos Erlang puedan hablar entre sí, **tienen que compartir un token secreto llamado "cookie"**. Esto evita que cualquiera se conecte y empiece a mandar mensajes sin autorización.

### ¿Cómo se hace?

1. En tu **home directory** de cada máquina creas un archivo llamado `.erlang.cookie`
    
2. Dentro le pones **exactamente el mismo texto** en todas (por ejemplo: `this_is_very_secret`)
    
3. Le das permisos estrictos:
    
    ```bash
    chmod 400 .erlang.cookie
    ```
    

👉 Si no tienen la misma cookie o los permisos están mal, los nodos se rechazan.

---

## ¿Qué es un nodo en Erlang?

Cada vez que levantas un entorno interactivo de Erlang con nombre, estás creando un **nodo**:

```bash
erl -sname ping
```

Eso levanta un nodo llamado `ping@máquina` que puede conectarse a otros.

> 🔍 Nota:  
> `-sname` usa nombres cortos (válido en la misma red LAN)  
> `-name` usa nombres completos tipo `ping@192.168.0.2`

---

## Comunicación entre procesos

En Erlang los procesos se comunican **solo por mensajes**. Esto funciona igual local o remoto.  
**La diferencia está en cómo se referencia al destinatario.**

- Local:
    
    ```erlang
    pong ! mensaje
    ```
    
- Remoto:
    
    ```erlang
    {pong, Pong_Node} ! mensaje
    ```
    

👉 Lo que cambia es que **le decimos a qué nodo pertenece el proceso registrado con ese nombre**.

---

## 🏓 Ping-Pong distribuido

### ¿Qué hace este programa?

Simula dos procesos:

- **ping** manda mensajes `ping` a **pong**
    
- **pong** responde con `pong`
    
- Se repite N veces hasta terminar.
    

Se puede correr:

- **en un solo nodo** (todo local)
    
- **o en nodos separados**, incluso en diferentes computadoras.
    

### Código clave (versión distribuida)

#### `ping/2`

Envía `{ping, self()}` a `{pong, Pong_Node}`  
Espera la respuesta `pong`, y repite.

#### `pong/0`

Recibe mensajes:

- `finished` => termina.
    
- `{ping, PID}` => responde `pong` al `PID`, y sigue.
    

### Inicio de procesos

- En **pong@gollum**:
    
    ```erlang
    tut17:start_pong().
    ```
    
- En **ping@kosken**:
    
    ```erlang
    tut17:start_ping(pong@gollum).
    ```
    

> 🔍 `self()` devuelve el pid del proceso actual.  
> `spawn` crea procesos, incluso en otros nodos si se especifica.

---

## Lanzando procesos remotos con `spawn/4`

En `tut18`, ya no se levanta `ping` desde la shell, sino que **pong levanta a ping remotamente**:

```erlang
spawn(Ping_Node, tut18, ping, [3, node()])
```

Eso crea `ping` en `Ping_Node`, enviándole como parámetro `3` y el `node()` actual (pong@gollum).

---

## Detalles curiosos y no triviales

- **Los PIDs en Erlang son globalmente únicos entre nodos conectados**.  
    Eso significa que un PID contiene no solo el identificador del proceso, sino también a qué nodo pertenece.  
    Por eso se puede hacer:
    
    ```erlang
    PID ! mensaje
    ```
    
    y no importa si está en otro nodo — **la VM sabe cómo enrutarlo**.
    
- **El output se queda donde el proceso fue creado**, no donde ocurre la acción.  
    Por eso en `tut18`, aunque ping está en `ping@kosken`, los `io:format` se ven en `pong@gollum`.  
    Esto es porque la shell desde donde se hace `spawn` actúa como su grupo de control.
    

---

## Consideraciones

- **Es simple, pero no trivial.**  
    No porque sea fácil de escribir significa que sea trivial de entender. El manejo de nodos, cookies, visibilidad de procesos y rutas de mensajes exige atención.
    
- **Seguridad básica.**  
    La autenticación con `.erlang.cookie` es débil y no escala bien en entornos de producción real (es buena para sistemas internos o entornos controlados).  
    Por eso existen alternativas como **Erlang Distribution over TLS**, pero eso es otro tema.
    
- **Concurrencia natural**  
    El modelo de mensajes sin estado compartido entre procesos **reduce drásticamente los errores típicos de la programación concurrente**.
    

---

## Conclusión

Este esquema de programación distribuida de Erlang es:

- **Minimalista**: solo necesitas nombres de nodo y pids.
    
- **Robusto**: los mensajes siempre llegan (o fallan de forma detectable)
    
- **Natural**: usa el mismo modelo local-remoto, sin distinción interna
    
- **Seguro a nivel básico**: cookies compartidas evitan conexiones no autorizadas.
    

Y lo mejor — **está incluido de forma nativa en el lenguaje desde hace décadas**. No es casual que sea el backend de WhatsApp, RabbitMQ, y otros sistemas donde **alta disponibilidad y tolerancia a fallos** son indispensables.

---
## Ejemplo extenso de lo anteriormente aprendido:
[Messenger Example](Ejemplos/Messenger%20Completo/)

---

## Ahora estarías listo para estudiar:
[Robustez](../III%20-%20Robustez/README.md)