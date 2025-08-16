## Diseñando una Aplicación Concurrente
### Entendiendo el problema 

Guiándome por el libro "Learn You Some Erlang for great good"(LYSE), encontré un muy buen capitulo dedicado al asunto de la concurrencia. En este se propone hacer una aplicación que sirva para crear recordatorios y alarmas para los mismos.

***Para empezar el software debe permitir**:*
- Añadir eventos, los cuales se denominaran desde ahora "Procesos x, y, z..." y cada uno tendrá una fecha tope, un nombre y descripción .
- Mostrar un mensaje de alerta cuando la fecha tope de x, o y o z llegue.
- Cancelar un evento por su nombre.
- Se debe poder actualizar el software mientras se ejecuta ya que no tenemos "Almacenamiento Persistente" por el momento.
- Sera una CLI app pero deberá de poder interactuar con otros servicios como uno web.

*La estructura que la app deberá tener seria algo así:*

![alt](Media/Pasted%20image%2020250721123008.png)
>Donde el cliente, el event server y los x, y, z son todos procesos.

**El event server tendrá estas tareas**:
- Aceptar subscripciones de clientes.
- Recibir y redirigir al cliente dueño de un evento especifico los mensajes o alarmas que  estos manden.
- Aceptar messages para agregar eventos por los clientes, cosa, osea, crear los procesos x, y, z...
- Aceptar messages para cancelar los eventos x, y, z...
- El event server debe ser terminado por el cliente.

**El proceso cliente tendrá estas tareas**:
- Suscribirse a un event server y poder recibir mensajes del mismo como notificaciones.
- Pedir al event server añadir un evento con todo lo que este debe llevar.
- Pedir al event server cancelar un evento especifico.
- Monitorear el event server para saber si este no se a caído o tiene fallos.
- Apagar el event server si es necesario.

**Los procesos x, y, z son realmente las notificaciones esperando enviarse, y deben cumplir con**:
- Enviar una notificación en forma de message a el event server cuando su tiempo caduque.
- Recibir un message de cancelación y eliminarse.

*Aquí hay un grafico que explica mucho mejor todo el flujo:*

![alt](Media/Pasted%20image%2020250721124230.png)
>Esto representa todos los procesos que tendremos. Al dibujar todas las flechas aquí y decir que son mensajes hemos escrito un protocolo de alto nivel, o al menos su esqueleto.

### Definiendo un Protocolo correcto

Ahora que conocemos como cada proceso o componente de la aplicación debe comportarse y comunicarse seria una muy buena idea hacer una lista de los **messages** que se enviaran y especificar como deberían lucir exactamente.

*Comencemos con la comunicación entre el cliente y el event server:*

![alt](Media/Pasted%20image%2020250721125510.png)
>Nótese que entre el cliente y el servidor hay un monitoreo bilateral. Usaremos dos monitores porque la dependencia no es obvia entre el cliente y el servidor ya que el cliente no puede o no tiene sentido que exista sin el servidor pero el servidor puede hacerlo sin un cliente.

*Siguiente message:*

![alt](Media/Pasted%20image%2020250721222838.png)
>Este message envía la orden de crear un evento al servidor, siendo respondido con el átomo "ok" a no ser que haya un error.

*La operación inversa para remover un evento:*

![alt](Media/Pasted%20image%2020250721223102.png)
>

*Además el servidor deberá poder enviar una notificación cuando el evento se termine:*

![alt](Media/Pasted%20image%2020250721223243.png)
> 

*En todo caso solo necesitaremos los siguientes dos messages para cuando queramos apagar el servidor, o cuando se caiga por errores:*

![alt](Media/Pasted%20image%2020250721223832.png)
>No se envía una confirmación mas directa cuando el servidor muere ya que el monitoreo se encargara personalmente de alertarnos.

> [!NOTE]
> Todo esto es bastante resumidamente lo que ocurrirá entre el servidor y el cliente.
> Ahora lidiaremos con los messages entre el servidor y los procesos evento. Un punto importante para destacar antes de fajarnos con todo esto es que seria muy buena idea tener un enlace directo entre el servidor y los procesos evento, esto porque queremos claramente que todos los eventos mueran si el server así lo hace.

*Bueno! Cuando un servidor crea un evento, este deberá asignarle un identificador especial(El nombre del evento), cuando el tiempo de alguno de estos eventos termine deberán enviar un mensaje al servidor diciéndolo:*

![alt](Media/Pasted%20image%2020250721232514.png)
>

*Por otro lado el evento deberá velar por messages de cancelación por parte del servidor:*

![alt](Media/Pasted%20image%2020250722003544.png)
>

*Para culminar necesitaremos un ultimo message para nuestro protocolo. El que nos permitirá mejorar nuestro servidor:*

![alt](Media/Pasted%20image%2020250722005100.png)
>No es necesaria una respuesta. Cuando estemos codeando nos daremos cuenta de por que.

> [!NOTE]
> Teniendo el protocolo definido y una idea general de como la jerarquía de nuestros procesos debe verse, podemos por fin empezar a trabajar en el proyecto.

### Levantemos los cimientos!

*Para comenzar debemos crear una estructura basica de Erlang que debe lucir algo asi:*

---
```erlang
ebin/
inlude/
priv/
src/
```
---

***Estos directorios  guardan archivos con las siguientes distribuciones:***
- La `ebin/` guarda los archivos ya compilados.
- La `include/` se usa para almacenar los `.hrl` no privados de nuestra aplicación, básicamente los que pueden ser accesibles por otras aplicaciones.
- La `priv/` es usada para guardar ejecutables o archivos necesarios, como drivers(Por ejemplo) para interactuar con tu aplicación.(No la necesitaremos ene este especifico proyecto).
- la `src/` es usada para guardar los `.erl` de nuestro proyecto

>[!Nota]
>Esta estructura puede variar en algunos otros proyectos, agregando por ejemplo la `conf/` para ahi dentro especificar archivos de configuración de el programa, la `doc/` para documentación (Así lo planeo hacer en mi repositorio) y `lib/` o `deps/` para librerías de terceros que deba usar nuestro proyecto.

### Un Modulo Evento
>[!Para aclarar antes de comenzar]
>Antes de comenzar a crear nuestro primer modulo, se debe destacar que este protocolo no esta completo. Ayuda a representar los datos enviados de un proceso a otro pero no los detalles. La mayoría de los mensajes se encapsulan en la forma `{Pid, Ref, Message}`, donde ***Pid*** es el remitente y ***Ref*** es un identificador único del mensaje para ayudar a determinar qué respuesta proviene de qué remitente. Si enviáramos muchos mensajes antes de buscar respuestas, no sabríamos qué respuesta corresponde a cada mensaje sin una referencia.

Con todo esto esclarecido si que podemos ahora dirigirnos a la carpeta `src/` de nuestro proyecto y crear un archivo `event.erl` donde comenzaremos a escribir código. Bueno! El núcleo de nuestro `event.erl` va a ser la función `loop/1` , la cual, debería verse algo parecida a este esqueleto:

---

```erlang
loop(State) ->
	receive
		{Server, Ref, cancel} ->
			...
	after Delay ->
			...
end.
```

---
>Ahi tendríamos dos cosas principalmente, una sería la forma de cancelar un evento mediante el paso de mensajes, y que luego del paso del tiempo establecido ocurra algo. 

La variable `state` deberá contener datos como el valor de tiempo de espera *(en segundos)* y el nombre del evento (para enviar el mensaje {done, Id}). También deberá conocer el PID del servidor de eventos para enviarle notificaciones.
Todo esto es necesario para el estado del bucle. Declaremos un registro `state` al principio del archivo:

---

```erlang
-module(event).
-compile(export_all).
-record(state, {
					server,
					name="",
					to_go=0
}).
```

---
>Lo de `-compile(export_all).` debe destacarse que solo se usa para evitar perder el tiempo eligiendo que funciones se exportan y cuales no, en producción eso se elimina.

**Refinemos un poco mas usando esto anterior:**

---

```erlang
loop(S = #state{server=Server}) ->
	receive
		{Server, Ref, cancel} ->
			Server ! {Ref, ok}
	after S#state.to_go*1000 ->
		Server ! {done, S#state.name}
end.
```

---
>Para cambiar el valor de la variable `to_go` de segundos a milisegundos es que multiplicamos por ***1000*** . Como alternativa se puede llamar a `timer:seconds/1`, que convierte de segundos a milisegundos automáticamente.

>[!Que no se te vaya la mano con el Zuko de Uva(Kool-Aid para los gringos)]
>¡Atención, un error de lenguaje! Necesitamos enlazar la variable Server en la cabecera de la función, ya que se usa para la coincidencia de patrones en la sección de recepción. Recuerda que los registros son trucos. La expresión `S#state.server` se expande secretamente a `element(2, S)`, que no es un patrón válido para la coincidencia.
Esto funciona correctamente para `S#state.to_go` después de la parte`"after"`, ya que puede ser una expresión que se evaluará más adelante.

>[!Sabias que...?]
>Kool-Aid es la marca de una _mezcla en polvo saborizada para preparar bebidas_, que pertenece a la compañía Kraft Foods. En Cuba sin embargo el equivalente seria Zuko(No el de Avatar :v), supongo hehe

***Bien, hagámosle un pequeño test a todo esto:***

---

```powershell
6> c(event).
{ok,event}
7> rr(event, state).
[state]
8> spawn(event, loop, [#state{server=self(), name="test", to_go=5}]).
<0.60.0>
9> flush().
ok
10> flush().
Shell got {done,"test"}
ok
11> Pid = spawn(event, loop, [#state{server=self(), name="test", to_go=500}]).
<0.64.0>
12> ReplyRef = make_ref().
#Ref<0.0.0.210>
13> Pid ! {self(), ReplyRef, cancel}.
{<0.50.0>,#Ref<0.0.0.210>,cancel}
14> flush().
Shell got {#Ref<0.0.0.210>,ok}
ok
```

---
>Primero, importamos el registro del módulo de eventos con `rr(Mod)`. Luego, generamos el bucle de eventos con el shell como servidor `(self())`. Este evento debería ejecutarse después de 5 segundos. La novena expresión se ejecutó después de 3 segundos y la décima después de 6 segundos. Se puede ver que recibimos el mensaje `{done, "test"}` en el segundo intento.
  Inmediatamente después, probamos la función de cancelación *(con 500 segundos de sobra para escribirla)*. Creamos la referencia(`Ref`), enviamos el mensaje y recibimos una respuesta con la misma `Ref`, por lo que sabemos que el `ok` que recibimos provenía de este proceso y no de ningún otro en el sistema.
  El mensaje de cancelación está encapsulado con una referencia, pero el mensaje de `"done"` no, simplemente porque no esperamos que provenga de un lugar específico (cualquier lugar servirá; no coincidiremos en la recepción), ni deberíamos querer responderle.
 
***Hagamos otra prueba. ¿Qué tal un evento que ocurra el próximo año?:***

---

```powershell
15> spawn(event, loop, [#state{server=self(), name="test", to_go=365*24*60*60}]).
<0.69.0>
16>
=ERROR REPORT==== DD-MM-YYYY::HH:mm:SS ===
Error in process <0.69.0> with exit value: {timeout_value,[{event,loop,1}]}
```

---
>Uffff. Eso dio error, bueno, es debido a que Erlang tiene una limitación a 50 días en milisegundos, parece insignificante pero hay que valorar 3 puntos al respecto:
>- Esto es una putada al hacer pruebas y escribir el modulo en medio de este capitulo.
>- Erlang realmente, no es lo mas optimo para ciertas tareas, y lo que vemos aquí es la consecuencia de usar `timers` de forma no pensada por los implementadores.
>- Bueno! No es realmente un problema. Podemos solucionarlo.

La solución que aplicaremos a este problema es escribir una función que divida el valor del tiempo de espera en varias partes si resulta ser demasiado largo. Esto también requerirá soporte de la función loop/1. Por lo tanto, la forma de dividir el tiempo es dividirlo en partes iguales de 49 días (ya que el límite es de aproximadamente 50) y luego sumar el resto con todas estas partes iguales. La suma de la lista de segundos ahora debería ser el tiempo original:

---

```Erlang
%% Because Erlang is limited to about 49 days (49*24*60*60*1000) in
%% milliseconds, the following function is used.
normalize(N) ->
	Limit = 49*24*60*60,
	[N rem Limit | lists:duplicate(N div Limit, Limit)].
```

---
>La función `lists:duplicate/2` tomará una expresión dada como segundo argumento y la reproducirá tantas veces como el valor del primer argumento `([a,a,a] = lists:duplicate(3, a))`. Si enviáramos a `normalize/1` el valor $98*24*60*60+4$, devolvería `[4,4233600,4233600]`

Ahora `loop/1` de nuestro modulo debería verse algo así:

---

```Erlang
%% Loop usa una lista para los tiempos con el objetivo de tener alrededor de ~49 dias de limite
%% on timeouts.
loop(S = #state{server=Server, to_go=[T|Next]}) ->
	receive
		{Server, Ref, cancel} ->
		Server ! {Ref, ok}
	after T*1000 ->
		if Next =:= [] ->
			Server ! {done, S#state.name};
		Next =/= [] ->
			loop(S#state{to_go=Next})
		end
end.
```

---
>Esto toma el primer elemento de la lista `to_go` y espera hasta que termine.
  Una vez hecho esto, se verifica el siguiente elemento de la lista de tiempo de espera. Si está vacío, el tiempo de espera finaliza y se notifica al servidor. De lo contrario, el bucle continúa con el resto de la lista hasta que finaliza.
  Puedes probar el bucle revisado. Debería funcionar con normalidad, pero ahora admite años y años de tiempo de espera.

#### Añadamos una interfaz!
Sería muy molesto tener que llamar manualmente a algo como `event:normalize(N)` cada vez que se inicia un proceso de evento, especialmente porque nuestra solución alternativa no debería ser un problema para los programadores que usan nuestro código. La forma estándar de hacerlo es usar una función `init` que gestione toda la inicialización de los datos necesarios para que la función de bucle funcione correctamente. Mientras tanto, añadiremos las funciones estándar `start` y `start_link`.

---

```Erlang
start(EventName, Delay) ->
	spawn(?MODULE, init, [self(), EventName, Delay]).
	
start_link(EventName, Delay) ->
	spawn_link(?MODULE, init, [self(), EventName, Delay]).
%%% Eventos internos:
init(Server, EventName, Delay) ->
	loop(#state{server=Server,
	name=EventName,
	to_go=normalize(Delay)}).
```

---

La interfaz ahora es mucho más limpia. Sin embargo, antes de realizar pruebas, sería bueno que el único mensaje que podemos enviar, `cancelar`, también tuviera su propia función de interfaz:

---

```Erlang
cancel(Pid) ->
	%% Monitorizar en caso de que el proceso ya esté muerto.
	Ref = erlang:monitor(process, Pid),
	Pid ! {self(), Ref, cancel},
	receive
		{Ref, ok} ->
			erlang:demonitor(Ref, [flush]),
			ok;
		{'DOWN', Ref, process, Pid, _Reason} ->
			ok
end.
```

---
>¡Ah, un nuevo truco! Aquí usamos un `monitor` para ver si el proceso está ahí. Si el proceso ya está inactivo, evitamos tiempos de espera innecesarios y devolvemos el resultado correcto, como se especifica en el protocolo. Si el proceso responde con la referencia, sabemos que pronto inactivo, así que la eliminamos para evitar recibirlos cuando ya no nos interesen. Tenga en cuenta que también proporcionamos la opción flush, que purgará el mensaje DOWN si se envió antes de que tuviéramos tiempo de demostrarlo.

Probemos estas funciones:

---

```Erlang
17> c(event).
{ok,event}
18> f().
ok
19> event:start("Event", 0).
<0.103.0>
20> flush().
Shell got {done,"Event"}
ok
21> Pid = event:start("Event", 500).
<0.106.0>
22> event:
```

---
>**And it works!**

Lo ultimo molesto con lo que tendremos que lidear en nuestro modulo es el hecho de tener que poner el tiempo restante en segundos. Seria mucho mejor poder poner un formato mas estándar como `({{Year, Month, Day}, {Hour, Minute, Second}})`.

Sin mas añadamos esta función que calcula la diferencia entre la fecha que se ingresa y la de tu pc:

---

```Erlang
time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
	Now = calendar:local_time(),
	ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
		calendar:datetime_to_gregorian_seconds(Now),
	Secs = if ToGo > 0 -> ToGo;
				ToGo =< 0 -> 0
	end,
	normalize(Secs).
```

---
>Ah!, si... El módulo de calendario tiene nombres de función bastante peculiares. Este calcula la cantidad de segundos entre el momento actual y el momento en que se supone que se activará el evento. Si el evento es del pasado, devolvemos 0 para que notifique al servidor lo antes posible. Ahora corrige la función `init` para que llame a esta función en lugar de `normalize/1`. También puedes cambiar el nombre de las variables de `delay` a `DateTime` si quieres que los nombres sean más descriptivos.

---

```erlang
init(Server, EventName, DateTime) ->
	loop(#state{server=Server,
		name=EventName,
		to_go=time_to_go(DateTime)}).
```

---

*Bueno! Ya que el modulo evento esta terminado podemos tomarnos un descanso. Haz un nuevo evento para probar, vete a tomar medio litro de leche o de cerveza y regresa justo a tiempo para ver que ya el mensaje del evento llego!*

### El modulo servidor!
Trabajemos con esto. de acuerdo con el protocolo el esqueleto se debería ver así:

---

```erlang
-module(evserv).
-compile(export_all).
loop(State) ->
	receive
		{Pid, MsgRef, {subscribe, Client}} ->
			...
		{Pid, MsgRef, {add, Name, Description, TimeOut}} ->
			...
		{Pid, MsgRef, {cancel, Name}} ->
			...
		{done, Name} ->
			...
		shutdown ->
			...
		{'DOWN', Ref, process, _Pid, _Reason} ->
			...
		code_change ->
			...
		Unknown ->
			io:format("Unknown message: ~p~n",[Unknown]),
			loop(State)
	end.
```

---
>Se notara que las llamadas que requieren respuestas están encapsuladas con el mismo formato de antes `{Pid, Ref, Message}`.

El servidor deberá mantener dos cosas en su estado:
1. Una lista de clientes subscritos.
2. Una lista de cada proceso evento que el servidor "spawned".
El protocolo dice que cuando el evento finalice, el servidor debe recibir `{done, Name}`, sin embargo envía `{done, Name, Descripction}` . La idea aca es tener la menor cantidad de trafico y mantener al proceso evento ocupándose estrictamente de lo que es hace falta.

***Bueno!*, aca estará la lista de clientes y la lista de eventos:**

---

```erlang
-record(state, {events, %% list of #event{} records
		clients}). %% list of Pids
-record(event, {name="",
		description="",
		pid,
		timeout={{1970,1,1},{0,0,0}}}).
```

---

Y el `loop` ahora tendrá la `record` definida en los argumentos que recibe:

---

```erlang
loop(S = #state{}) ->
	receive
		...
	end.
```

---

Sería ideal que tanto los eventos como los clientes fueran orddicts. Es poco probable que tengamos cientos de ellos a la vez. Como recordarán del Capítulo 9, los orddicts se adaptan perfectamente a esa necesidad.

*Escribiremos una función de inicio para gestionar esto.*

---

```erlang
init() ->
%% Aca se pueden cargar eventos de un archivo estatico.
%% Debes pasar un argumento a init para indicar donde esta el %% recurso de los eventos. Luego lo cargas desde aca.
%% Lo otro puediera ser pasar directo los eventos al servidor %% a traves de esta funcion.
loop(#state{events=orddict:new(),
			 clients=orddict:new()}).
```

---

Con el esqueleto y las inicializaciones hechas empezaremos a implementar cada message uno a uno!

#### Encarguémonos de los "messages"!

Empezaremos por el de las ***suscripciones***. Queremos mantener una lista de todos los suscriptores porque, cuando finaliza un evento, necesitamos notificarles. Además, nuestro protocolo menciona que debemos monitorearlos. Esto tiene sentido, ya que no queremos retener clientes bloqueados y enviar mensajes inútiles sin motivo. El código debería verse así:

---

```erlang
{Pid, MsgRef, {subscribe, Client}} ->
	Ref = erlang:monitor(process, Client),
	NewClients = orddict:store(Ref, Client, S#state.clients),
	Pid ! {MsgRef, ok},
	loop(S#state{clients=NewClients});
```

---
>Esta sección de `loop/1` inicia un monitor y almacena la información del cliente en el orddict bajo la clave `Ref`. La razón es simple: la única otra ocasión en la que necesitaremos obtener el `ID` del cliente será si recibimos un mensaje `EXIT` del monitor, que contendrá la referencia (lo que nos permitirá eliminar la entrada del orddict).

El siguiente mensaje que nos interesa es ***aquel en el que añadimos eventos***. Ahora, es posible devolver un estado de error.
La única validación que realizaremos será comprobar las marcas de tiempo que aceptamos. Si bien es fácil suscribirse al diseño `{{Año, Mes, Día}, {Hora, Minuto, Segundos}}`, debemos asegurarnos de no aceptar eventos el 29 de febrero cuando no estamos en un año bisiesto, ni en ninguna otra fecha inexistente. Además, no queremos aceptar valores de fecha imposibles como "5 horas, menos 1 minuto y 75 segundos". Una sola función puede encargarse de validar todo esto.
El primer componente que usaremos es la función `calendar:valid_date/1`.
Como su nombre indica, esta función comprueba si la fecha es válida. Lamentablemente, la rareza del módulo calendar no se limita a los nombres peculiares; en realidad, no existe ninguna función que confirme que `{H,M,S}` tenga valores válidos. Tendremos que implementarla también, siguiendo el esquema de nombres peculiar.

---

```erlang
valid_datetime({Date,Time}) ->
	try
		calendar:valid_date(Date) andalso valid_time(Time)
	catch
		error:function_clause -> %% not in {{D,M,Y},{H,Min,S}} format
			false
	end;
valid_datetime(_) ->
	false.
valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when H >= 0, H < 24,
						   M >= 0, M < 60,
						   S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.
```
 
 ---
 
 La función `valid_datetime/1` ahora se puede usar en la parte donde intentamos agregar el mensaje:

---

```erlang
{Pid, MsgRef, {add, Name, Description, TimeOut}} ->
	case valid_datetime(TimeOut) of
		true ->
			EventPid = event:start_link(Name, TimeOut),
			NewEvents = orddict:store(Name,
									#event{name=Name,
										description=Description,
										pid=EventPid,
										timeout=TimeOut},
									S#state.events),
			Pid ! {MsgRef, ok},
			loop(S#state{events=NewEvents});
		false ->
			Pid ! {MsgRef, {error, bad_timeout}},
			loop(S)
	end;
```

---
>Si el tiempo es válido, generamos un nuevo proceso de evento y almacenamos sus datos en el estado del servidor de eventos antes de enviar una confirmación al llamante. Si el tiempo de espera es incorrecto, notificamos al cliente, en lugar de que el error pase desapercibido o bloquee el servidor. Se podrían añadir comprobaciones adicionales para detectar conflictos de nombres u otras restricciones. (¡Acuérdate de actualizar la documentación del protocolo!)

El siguiente mensaje definido en nuestro protocolo es ***aquel en el que cancelamos un evento***. Cancelar un evento nunca falla en el lado del cliente, por lo que el código es más sencillo. Simplemente verifique si el evento está en el registro de estado del proceso. Si lo está, use la función event:cancel/1 que definimos para eliminarlo y enviar "ok". Si no se encuentra, informe al usuario que todo salió bien de todos modos; el evento no se está ejecutando, y eso es lo que el usuario quería:

---

```erlang
{Pid, MsgRef, {cancel, Name}} ->
	Events = case orddict:find(Name, S#state.events) of
				{ok, E} ->
					event:cancel(E#event.pid),
					orddict:erase(Name, S#state.events);
				error ->
					S#state.events
			  end,
	Pid ! {MsgRef, ok},
	loop(S#state{events=Events});
```

---

Ahora toda la interacción voluntaria del cliente al servidor de eventos está cubierta. Abordemos lo que ocurre entre el servidor y los eventos. Hay dos mensajes que gestionar: la cancelación de los eventos (lo cual ya está hecho) y el tiempo de espera de los eventos. Ese mensaje es simplemente `{done, Name}`:

---

```erlang
{done, Name} ->
	case orddict:find(Name, S#state.events) of
		{ok, E} ->
			send_to_clients({done, E#event.name, E#event.description},
			S#state.clients),
			NewEvents = orddict:erase(Name, S#state.events),
			loop(S#state{events=NewEvents});
		error ->
%% esto puede pasar si se cancela y se termina a la vez
			loop(S)
	end;
```

---

La función `send_to_clients/2` hace lo que indica su nombre y se define de la siguiente manera:

---

```erlang
send_to_clients(Msg, ClientDict) ->
	orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).
```

---

Eso debería ser todo para la mayor parte del código del bucle. ***Lo que queda es la gestión de los diferentes mensajes de estado: clientes que se caen, apagado, actualizaciones de código, etc. Aquí vienen:***

---

```erlang
shutdown ->
	exit(shutdown);
{'DOWN', Ref, process, _Pid, _Reason} ->
	loop(S#state{clients=orddict:erase(Ref, S#state.clients)});
code_change ->
	?MODULE:loop(S);
Unknown ->
	io:format("Unknown message: ~p~n",[Unknown]),
	loop(S)
```

---
>El primer caso `(shutdown)` es bastante explícito. Se recibe el mensaje de finalización; se deja que el proceso finalice. Si se desea guardar el estado en el disco, este podría ser un posible lugar para hacerlo. Si se desea una semántica de guardado/salida más segura, esto podría implementarse en cada mensaje de adición, cancelación o finalización. La carga de eventos desde el disco podría realizarse en la función de inicio, generándolos a medida que llegan.

>Las acciones del mensaje `"DOWN"` también son bastante simples. Significa que un cliente finalizó, por lo que lo eliminamos de la lista de clientes en el estado.

>Los mensajes desconocidos solo se mostrarán con `io:format/2` para fines de depuración, aunque una aplicación de producción real probablemente usaría un módulo de registro dedicado. De lo contrario, toda esa información útil se desperdiciaría en una salida que nadie busca en producción.

A continuación viene el mensaje de cambio de código. Este es lo suficientemente interesante como para tener su propia sección.

##### Para los amantes del código complejo...

Para realizar la carga de código activo, Erlang cuenta con el servidor de código. Este servidor es básicamente un proceso de la máquina virtual (VM) encargado de una tabla ETS (una tabla de base de datos en memoria, nativa de la VM, que se describe más adelante en el Capítulo 25(Del libro)). El servidor de código puede almacenar dos versiones de un mismo módulo en memoria, y ambas versiones pueden ejecutarse simultáneamente. Una nueva versión de un módulo se carga automáticamente al compilarlo con `c(Módulo)`, al cargarlo con `l(Módulo)` o al cargarlo con una de las muchas funciones del módulo de código, sobre las que puede consultar la documentación de Erlang.
Un concepto importante es que Erlang tiene llamadas locales y externas. Las llamadas locales son aquellas llamadas a funciones que pueden realizarse con funciones que no se pueden exportar. Tienen el formato Nombre(Argumentos). Una llamada externa solo se puede realizar con funciones exportadas y tiene la forma Módulo:Función(Argumentos). El nombre exacto de una llamada externa es llamada completa. Cuando hay dos versiones de un módulo cargadas en la máquina virtual, todas las llamadas locales se realizan a través de la versión que se está ejecutando en un proceso. Sin embargo, las llamadas completas siempre se realizan en la versión más reciente del código disponible en el servidor de código. Por lo tanto, si las llamadas locales se realizan desde la versión completa, se realizan en la nueva versión del código.
![alt](Media/Pasted%20image%2020250814064139.png)
>Dado que cada proceso/actor en Erlang necesita realizar una llamada recursiva para cambiar de estado, es posible cargar versiones completamente nuevas de un actor mediante una llamada recursiva externa.

>[!Notita]
>Si se carga una tercera versión de un módulo mientras un proceso aún ejecuta la primera, la máquina virtual cerrará dicho proceso, asumiendo que era un proceso huérfano sin supervisor ni forma de actualizarse. Si nadie ejecuta la versión más antigua, simplemente se descarta y se conservan las más recientes.

Hay maneras de vincular tu código a un módulo del sistema que enviará mensajes cada vez que se cargue una nueva versión de un módulo. De esta manera, puedes activar la recarga del módulo solo al recibir dicho mensaje y hacerlo siempre con una función de actualización de código, por ejemplo, `MyModule:Upgrade(CurrentState)`, que podrá transformar la estructura de datos de estado según la especificación de la nueva versión. Esta gestión de "suscripción" la realiza automáticamente el framework OTP, que comenzaremos a estudiar en el Capítulo 14.
Para la aplicación de recordatorio, no usaremos el servidor de código, sino un mensaje `code_change` personalizado del `shell`, que realiza una recarga muy básica.
Eso es prácticamente todo lo que necesitas saber para realizar la carga de código activa.
Sin embargo, aquí tienes un ejemplo más genérico:

---

```erlang
-module(hotload).
-export([server/1, upgrade/1]).

server(State) ->
	receive
		update ->
			NewState = ?MODULE:upgrade(State),
			?MODULE:server(NewState); %% Loop in the new version of the module.
		SomeMessage ->
			%% Do something here.
			server(State) %% Stay in the same version no matter what.
	end.
upgrade(OldState) ->
%% Transform and return the state here.
```

---
>Como puedes ver, nuestro `?MODULE:loop(S)` se ajusta a este patrón.

#### Esconde los mensajes! De verdad, hazlo...(Seguimos con los mensajes muchachos)

¡Esconde los mensajes! Si esperas que los usuarios desarrollen tu código y procesos, debes ocultar los mensajes en las funciones de la interfaz. Esto es lo que usamos para el módulo `evserv`:

---

```erlang
start() ->
	register(?MODULE, Pid=spawn(?MODULE, init, [])),
	Pid.
start_link() ->
	register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
	Pid.
terminate() ->
	?MODULE ! shutdown.
```

---
>Registramos el módulo del servidor porque, por ahora, solo deberíamos tener uno ejecutándose a la vez. Si se quisiera ampliar la aplicación de recordatorios para que admita a muchos usuarios, sería recomendable registrar los nombres con el módulo `global`, y sería aún mejor usar la biblioteca `gproc`.

Para esta aplicación de ejemplo, lo que tenemos aquí será suficiente.

>[!Notita]
>La biblioteca `gproc` es un diccionario de procesos para Erlang que ofrece varias funciones útiles, además de las del diccionario integrado, como el uso de cualquier término como alias, múltiples nombres para un proceso, la espera del registro de otros procesos, la asignación de nombres atómicos y contadores. Está disponible en http://github.com/uwiger/gproc.

El primer mensaje que escribimos es el siguiente que debemos abstraer: cómo suscribirse. El pequeño protocolo o especificación que escribimos anteriormente requería un monitor, así que este se agrega. En cualquier momento, si la referencia devuelta por el mensaje de suscripción está en un mensaje `DOWN`, el cliente sabrá que el servidor ha dejado de funcionar.

---

```erlang
subscribe(Pid) ->
	Ref = erlang:monitor(process, whereis(?MODULE)),
	?MODULE ! {self(), Ref, {subscribe, Pid}},
	receive
		{Ref, ok} ->
			{ok, Ref};
		{'DOWN', Ref, process, _Pid, Reason} ->
			{error, Reason}
	after 5000 ->
		{error, timeout}
	end.
```

---

*El siguiente mensaje a abstraer es el evento que agrega:*

---

```erlang
add_event(Name, Description, TimeOut) ->
	Ref = make_ref(),
	?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
	receive
		{Ref, Msg} -> Msg
	after 5000 ->
		{error, timeout}
	end.
```

---

Tenga en cuenta que reenviamos al cliente el mensaje `{error, bad_timeout}` que podría recibirse. También podríamos haber decidido bloquear el cliente lanzando `erlang:error(bad_timeout)`. La comunidad aún debate si bloquear el cliente o reenviar el mensaje de error es la mejor opción. Aquí está la función alternativa para bloquear el cliente:

---

```erlang
add_event2(Name, Description, TimeOut) ->
	Ref = make_ref(),
	?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
	receive
		{Ref, {error, Reason}} -> erlang:error(Reason);
		{Ref, Msg} -> Msg
	after 5000 ->
		{error, timeout}
	end.
```

---

Luego está la cancelación de eventos, que simplemente toma un nombre:

---

```erlang
cancel(Name) ->
	Ref = make_ref(),
	?MODULE ! {self(), Ref, {cancel, Name}},
	receive
		{Ref, ok} -> ok
	after 5000 ->
		{error, timeout}
	end.
```

---

Por último, se ofrece una pequeña ventaja para el cliente: una función que acumula todos los mensajes durante un periodo determinado. Si se encuentran mensajes, se toman todos y la función regresa lo antes posible.

---

```erlang
listen(Delay) ->
	receive
		M = {done, _Name, _Description} ->
			[M | listen(0)]
	after Delay*1000 ->
		[]
	end.
```

---

Esto es especialmente útil cuando se trabaja con aplicaciones en las que el cliente busca actualizaciones, mientras que las aplicaciones que siempre están escuchando pueden usar un mecanismo basado en push y, por lo tanto, no necesitarían dicha función.

### Probando esta historia!

Ahora deberías poder compilar la aplicación y probarla.
Para simplificar un poco las cosas, escribiremos un archivo `makefile` de Erlang específico para compilar el proyecto. Abre un archivo llamado `Emakefile` y colócalo en el directorio base del proyecto. El archivo contiene términos de Erlang y le proporciona al compilador la receta para crear archivos `.beam ` perfectos y concisos.

---

```powershell
{'src/*', [debug_info,
	{i, "src"},
	{i, "include"},
	{outdir, "ebin"}]}.
```

---

Esto le indica al compilador que agregue `debug_info` a los archivos (esta opción rara vez se descarta), que busque archivos de encabezado en los directorios `src/` e `include/` para compilar módulos en `src/` y que los muestre en `ebin/`.
Vaya a la línea de comandos y ejecute `erl -make` desde el directorio base del proyecto. Todos los archivos se compilarán y se guardarán automáticamente en el directorio `ebin/`. Inicie la shell de Erlang introduciendo `erl -pa ebin/`. La opción `-pa directory` le indica a la máquina virtual de Erlang que agregue esa ruta a los lugares donde puede buscar módulos.
Otra opción es iniciar la `shell` como de costumbre y ejecutar `make:all([load])`. Esto buscará un archivo llamado `Emakefile` en el directorio actual, lo recompilará (si ha cambiado) y cargará los nuevos archivos.
Ahora debería poder rastrear miles de eventos. Pruébelo.

---

```powershell
1> evserv:start().
<0.34.0>
2> evserv:subscribe(self()).
{ok,#Ref<0.0.0.31>}
3> evserv:add_event("Hey there", "test", FutureDateTime).
ok
4> evserv:listen(5).
[]
5> evserv:cancel("Hey there").
ok
6> evserv:add_event("Hey there2", "test", NextMinuteDateTime).
ok
7> evserv:listen(2000).
[{done,"Hey there2","test"}]
```

---

Esto funciona correctamente. Escribir cualquier cliente ahora debería ser bastante sencillo, dadas las pocas funciones básicas de interfaz que hemos creado.

### Agreguemos los famosos supervisores c:
Para que nuestro ejemplo sea una aplicación más estable, deberíamos escribir un "reiniciador", como hicimos en el Capítulo 12(Del libro). Abra un archivo llamado `sup.erl` donde estará nuestro supervisor:

---

```erlang
-module(sup).
-export([start/2, start_link/2, init/1, loop/1]).

start(Mod,Args) ->
	spawn(?MODULE, init, [{Mod, Args}]).
	
start_link(Mod,Args) ->
	spawn_link(?MODULE, init, [{Mod, Args}]).
	
init({Mod,Args}) ->
	process_flag(trap_exit, true),
	loop({Mod,start_link,Args}).
	
loop({M,F,A}) ->
	Pid = apply(M,F,A),
	receive
	{'EXIT', _From, shutdown} ->
		exit(shutdown); % will kill the child too
	{'EXIT', Pid, Reason} ->
		io:format("Process ~p exited for reason ~p~n",[Pid,Reason]),
		loop({M,F,A})
	end.
```

---

Esto es similar al reiniciador del Capítulo 12, aunque este es un poco más genérico. Puede usar cualquier módulo, siempre que tenga una función `start_link`. Reiniciará el proceso que supervisa indefinidamente, a menos que el `supervisor` se finalice con una señal de salida de apagado. Aquí está en uso:

---

```powershell
1> c(evserv), c(sup).
{ok,sup}
2> SupPid = sup:start(evserv, []).
<0.43.0>
3> whereis(evserv).
<0.44.0>
4> exit(whereis(evserv), die).
true
Process <0.44.0> exited for reason die
5> exit(whereis(evserv), die).
Process <0.48.0> exited for reason die
true
6> exit(SupPid, shutdown).
true
7> whereis(evserv).
undefined
```

---
>Como puedes ver, matar al supervisor también matará a su hijo.

>[!Notita]
>Exploraremos supervisores mucho más avanzados y flexibles en el Capítulo 18. Estos son los que se tienen en cuenta al hablar de árboles de supervisión. El supervisor que se muestra aquí es solo la forma más básica que existe y no es exactamente adecuado para entornos de producción en comparación con el entorno real.

### "Namespaces" (o falta de los espacios)

Debido a que Erlang tiene una estructura de módulos plana (sin jerarquía), algunas aplicaciones pueden tener conflictos de nombres entre sus módulos. Un ejemplo es el módulo de usuario, de uso frecuente, que casi todos los proyectos intentan definir al menos una vez. Esto entra en conflicto con el módulo de usuario incluido en Erlang. Puede comprobar si hay conflictos con el código de `función:clash/0`.
Debido a la posibilidad de conflictos, el patrón común es anteponer el nombre del proyecto al nombre de cada módulo. En este caso, los módulos de nuestra aplicación de recordatorios deberían renombrarse como `recall_evserv`, `recall_sup` y `recall_event`.
Algunos programadores deciden añadir un módulo, con el nombre de la propia aplicación, que encapsula las llamadas comunes que los programadores podrían realizar al usar su propia aplicación. Ejemplos de llamadas podrían ser funciones como iniciar la aplicación con un supervisor, suscribirse al servidor y añadir y cancelar eventos. También es importante tener en cuenta otros espacios de nombres, como los nombres registrados que no deben entrar en conflicto, las tablas de bases de datos, etc.
Esto es prácticamente todo para una aplicación concurrente de Erlang muy básica.
Esta demostró que podíamos tener varios procesos concurrentes sin pensarlo demasiado: supervisores, clientes, servidores, procesos utilizados como temporizadores (y podríamos tener miles de ellos), etc. No hay necesidad de sincronizarlos, ni bloqueos, ni un bucle principal real. El paso de mensajes ha simplificado la compartimentación de nuestra aplicación en unos pocos módulos con preocupaciones y tareas separadas.
Las llamadas básicas dentro de `evserv.erl` ahora se pueden usar para construir clientes que puedan interactuar con el servidor de eventos desde fuera de la máquina virtual de Erlang y hacer que el programa sea realmente útil.
Sin embargo, antes de hacerlo, le sugiero que lea sobre el framework OTP.
Los siguientes capítulos cubrirán algunos de sus componentes básicos, que permiten aplicaciones mucho más robustas y elegantes. Gran parte del poder de Erlang reside en el uso del framework OTP. Es una herramienta cuidadosamente diseñada y bien diseñada que cualquier programador de Erlang que se precie debe conocer.