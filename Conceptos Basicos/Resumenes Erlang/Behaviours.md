### 📌 ¿Qué es un Behaviour en Erlang/OTP?

Un **behaviour** (comportamiento) en Erlang es una **plantilla de diseño de módulos**. Define una serie de **funciones preestablecidas que tú como programador debes implementar**.  
Es la forma que tiene OTP de ofrecer **patrones reutilizables de código robusto**, para construir sistemas concurrentes, distribuidos y tolerantes a fallos.

> Piensa en los behaviours como interfaces + lógica estándar que tú "rellenas" con tu código.

---

### 🧩 ¿Por qué existen? ¿Qué problema resuelven?

Antes de OTP, todo el manejo de procesos, mensajes, fallos y reinicios lo hacías tú a mano. Eso lleva a errores, duplicación y poca fiabilidad. Los **behaviours abstraen esas tareas repetitivas**, obligándote a seguir **buenas prácticas y estructuras conocidas**.

Con behaviours:

- Evitas escribir tú mismo bucles de recepción de mensajes (`receive`).
    
- Te enfocas en la lógica de negocio.
    
- Aprovechas el **supervisor tree** de OTP.
    
- Tienes **tolerancia a fallos, reinicio automático**, etc.
    

---

### 🧰 ¿Qué tipos de behaviours existen en OTP?

#### 1. `gen_server` – (Generic Server)

- Para **procesos que manejan estado**.
    
- Tú defines cómo manejar llamadas síncronas (`handle_call`), asincrónicas (`handle_cast`) y otros mensajes.
    
- Muy usado para lógica de negocio, cachés, controladores, etc.
    

#### 2. `gen_statem` – (State Machine)

- Como `gen_server` pero orientado a **máquinas de estado finito**.
    
- Útil cuando tu sistema tiene **estados bien definidos y transiciones claras** (por ejemplo, conexión de red: `connecting → connected → closed`).
    

#### 3. `gen_event`

- Para sistemas con **múltiples manejadores de eventos**.
    
- Permite un sistema donde varios módulos reaccionan a eventos emitidos por un productor.
    
- No se usa tanto hoy, pero sigue disponible.
    

#### 4. `supervisor`

- Para **supervisar otros procesos**.
    
- Reinicia procesos cuando fallan, según estrategias definidas (`one_for_one`, `one_for_all`, etc).
    
- Parte fundamental de sistemas tolerantes a fallos.
    

#### 5. `application`

- Encapsula todo un sistema OTP como una **unidad autónoma y arrancable**.
    
- Define puntos de entrada, dependencias, y configuración.
    
- Se usa para integrar módulos en proyectos OTP completos.
    

---

### 🛠️ ¿Cómo usar un behaviour?

Ejemplo básico: `gen_server`

```erlang
-module(my_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, my_server}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

---

### 🧾 ¿Qué nos permite hacer que antes era difícil?

Sin behaviours tendrías que:

- Implementar tú mismo el bucle de mensajes con `receive`.
    
- Manejar tú los errores, caídas y reinicios de procesos.
    
- Escribir estructura para iniciar y enlazar procesos manualmente.
    

Con behaviours:

- Ganas consistencia, claridad y mantenimiento a largo plazo.
    
- Tienes menos errores sutiles en concurrencia y fallos.
    
- Puedes escalar sistemas con _supervisores jerárquicos_.
    

---

### 🎯 ¿Cuándo usar behaviours?

- Siempre que crees un **proceso con estado** o necesites lógica en respuesta a mensajes: `gen_server`.
    
- Si tienes múltiples **estados con transiciones complejas**: `gen_statem`.
    
- Cuando tu sistema crece: **usa `supervisors` para robustez**.
    
- Para paquetes o sistemas grandes: encapsúlalo como una `application`.
    

> Casi todos los sistemas OTP reales usan behaviours. No es opcional si buscas calidad.

---

### 🧪 Notas finales

- Los behaviours **no hacen magia**, solo estructuran y estandarizan.
    
- Puedes crear tus propios behaviours si necesitas patrones personalizados.
    
- Entender bien `gen_server` y `supervisor` es la base del desarrollo en Erlang/OTP.
    
---
  *!!!Resumen de GPT*