
---

## Introducción

En este capitulo vamos a empezar con el *framework* **OTP**(Abreviatura de Open Telecom Plataform) de **Erlang**. Cabe decir que en estos días, mas que para aplicaciones de telecomunicaciones se usa mayormente para aplicaciones con algunas propiedades de telecomunicaciones. Si la mayor parte de la grandeza de **Erlang** viene de su altísimo concurrencia y distribución, y la otra de su manejo de errores, entonces la tercera parte seria el *framework* **OTP**. 

En los capítulos anteriores, vimos algunos ejemplos de prácticas comunes para escribir aplicaciones concurrentes con las funciones integradas del lenguaje: enlaces, monitores, servidores, tiempos de espera, salidas de trampa, etc. La programación concurrente presentaba algunos inconvenientes: las tareas debían realizarse en un orden determinado, se debían evitar las condiciones de carrera y un proceso podía detenerse en cualquier momento. También abordamos la carga de código activo, la asignación de nombres a los procesos, la adición de supervisores y otras técnicas.

Hacer todo esto manualmente requiere mucho tiempo y es propenso a errores. Hay casos especiales que se pueden olvidar y trampas en las que se puede caer. El *framework* **OTP** se encarga de esto agrupando estas prácticas esenciales en un conjunto de bibliotecas cuidadosamente diseñadas y perfeccionadas a lo largo de los años. Todo programador de **Erlang** debería usarlas.
El *framework* **OTP** también *es un conjunto de módulos y estándares diseñados para ayudarte a crear aplicaciones*. Dado que la mayoría de los programadores de **Erlang** terminan usando **OTP**, la mayoría de las aplicaciones de **Erlang** que encontrarás tenderán a seguir estos estándares.

---

## El proceso común, pero abstraído

Una de las cosas que hemos hecho muchas veces en los ejemplos de procesos anteriores es dividir todo según tareas muy específicas. En la mayoría de los procesos, teníamos una función encargada de generar el nuevo proceso, otra encargada de asignarle sus valores iniciales, un bucle principal... Resulta que estas partes suelen estar presentes en todos los programas concurrentes que escribas, independientemente del uso que se le dé al proceso.

![alt](Media/Pasted%20image%2020250817131840.png)

Los ingenieros e informáticos responsables del *framework* **OTP** detectaron estos patrones y los incluyeron en varias bibliotecas comunes. Las bibliotecas **OTP** se construyen con código equivalente a la mayoría de las abstracciones que utilizamos (como el uso de referencias para etiquetar mensajes), con la ventaja de haber sido utilizadas durante años en el campo y construidas con mucha más precaución que en nuestras implementaciones. Contienen funciones para generar e inicializar procesos de forma segura, enviarles mensajes con tolerancia a fallos y realizar muchas otras tareas. Sin embargo, rara vez debería necesitar usar estas bibliotecas. Las abstracciones que contienen son tan básicas y universales que se construyeron sobre ellas muchas más cosas interesantes.

![alt](Media/Pasted%20image%2020250817131912.png)

En este capítulo y en los siguientes, analizaremos algunos de los usos comunes de los procesos y cómo pueden abstraerse y generalizarse.
Posteriormente, para cada uno de ellos, exploraremos la implementación correspondiente con los comportamientos del marco OTP.

---

## El `basic server`

El patrón común que exploraremos en este capítulo es uno que ya hemos utilizado. Para el servidor de eventos que describimos en el `Capítulo 13`, utilizamos un modelo **cliente/servidor**. El servidor de eventos recibe llamadas del cliente, actúa sobre ellas y luego responde al cliente si el protocolo así lo indica.

---

### Introduciendo el `"kitty" server`

En este capítulo, usaremos un servidor muy simple, lo que nos permitirá centrarnos en sus propiedades esenciales. Aquí está el servidor `kitty_server`:

---

```erlang
%Version nativa
-module(kitty_server).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-record(cat, {name, color=green, description}).

%Negocio muy legal de gatos, dicen que los naranjas te dan un viaje mas fuerte c;

%Client Api
start_link() ->
    spawn_link(fun init/0).

%Llammadas sincronas
order_cat(Pid, Name, Color, Description) ->
    Ref = monitor(process, Pid),
    Pid ! {self(), Ref, {order, Name, Color, Description}},
    receive
        {Ref, Cat} ->
            demonitor(Ref, [flush]),
            Cat;
        {'DOWN', Ref, process, Pid, Reason} ->
            error(Reason)
        after 5000 ->
            error(timeout)
    end.
  
%Esta es asincrona
return_cat(Pid, Cat = #cat{}) ->
    Pid ! {return, Cat},
    ok

%Esta es sincrona
close_shop(Pid) ->
    Ref = monitor(process, Pid),
    Pid ! {self(), Ref, terminate},
    receive
        {Ref, ok} ->
            demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, Reason} ->
            error(Reason)
        after 5000 ->
            error(timeout)
    end.

%Server function
init() -> loop([]).

loop(Cats) ->
    receive
        {Pid, Ref, {order, Name, Color, Description}} ->
            if
                Cats =:= [] ->
                    Pid ! {Ref, make_cat(Name, Color, Description)},
                    loop(Cats);
                cats =/= [] ->
                    Pid ! {Ref, hd(Cats)},
                    loop(tl(Cats))
            end;
        {return, Cat = #cat{}} ->
            loop([Cat|Cats]);
        {Pid, Ref, terminate} ->
            Pid ! {Ref, ok},
            terminate(Cats);
        Unknown ->
            %Ponemos sin mas unos logs comicones:
            io:format("Mensaje desconocido: ~p~n", [Unknown]),
            loop(Cats)
    end.
  
%Funciones privadas sin mas.
make_cat(Name, Color, Description) ->
    #cat{name=Name, color = Color, description = Description}.

terminate(Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
    ok.
```

---

Así que este es un ***servidor/tienda*** de *gatitos*. El funcionamiento es extremadamente simple: Describes un gato y lo *obtienes*. Si alguien devuelve un *gato*, se añade a una lista y se envía automáticamente como el siguiente pedido, en lugar de lo que el cliente realmente pidió (**estamos en esta tienda de gatitos por el dinero, no por las sonrisas c;**).

---

```erlang
1> c(kitty_server).
{ok,kitty_server}
2> rr(kitty_server).
[cat]
3> Pid = kitty_server:start_link().
<0.57.0>
4> Cat1 = kitty_server:order_cat(Pid, carl, brown, "loves to burn bridges").
#cat{name = carl,color = brown,
description = "loves to burn bridges"}
5> kitty_server:return_cat(Pid, Cat1).
ok
6> kitty_server:order_cat(Pid, jimmy, orange, "cuddly").
#cat{name = carl,color = brown,
description = "loves to burn bridges"}
7> kitty_server:order_cat(Pid, jimmy, orange, "cuddly").
#cat{name = jimmy,color = orange,description = "cuddly"}
8> kitty_server:return_cat(Pid, Cat1).
ok
9> kitty_server:close_shop(Pid).
carl was set free.
ok
10> kitty_server:close_shop(Pid).
** exception error: no such process or port
in function kitty_server:close_shop/1
```

---

Al revisar el código fuente del módulo, podemos ver patrones que ya hemos aplicado. Las secciones donde activamos y desactivamos monitores, aplicamos temporizadores, recibimos datos, usamos un bucle principal, manejamos la función init, etc., deberían resultar familiares. Debería ser posible abstraer estas cosas que repetimos constantemente. Empecemos con la **API** del cliente.

---

### Generalizando llamadas

Lo primero que se observa en el código fuente es que ambas llamadas síncronas son extremadamente similares. Estas son las llamadas que probablemente se incluirían en bibliotecas de abstracción, como se mencionó anteriormente. Por ahora, las abstraeremos como una sola función en un nuevo módulo que contendrá todas las partes genéricas del servidor Kitty.

---

```erlang
-module(my_server).
-compile(export_all).

call(Pid, Msg) ->
	Ref = erlang:monitor(process, Pid),
	Pid ! {self(), Ref, Msg},
	receive
		{Ref, Reply} ->
			erlang:demonitor(Ref, [flush]),
			Reply;
		{'DOWN', Ref, process, Pid, Reason} ->
			erlang:error(Reason)
		after 5000 ->
			erlang:error(timeout)
	end.
```

---

Esto toma un mensaje y un PID, los inserta en la función y luego reenvía el mensaje de forma segura. A partir de ahora, podemos sustituir el envío del mensaje por una llamada a esta función. Por lo tanto, si reescribiéramos un nuevo servidor Kitty para emparejarlo con el servidor abstracto my_server, podría comenzar así:

---

```erlang
-module(kitty_server2).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-record(cat, {name, color=green, description}).

%%% Client API
start_link() -> spawn_link(fun init/0).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
	my_server:call(Pid, {order, Name, Color, Description}).
	
%% This call is asynchronous.
return_cat(Pid, Cat = #cat{}) ->
	Pid ! {return, Cat},
	ok.

%% Synchronous call
close_shop(Pid) ->
	my_server:call(Pid, terminate).
```

---

### Generalizando el `server loop`

El siguiente fragmento de código genérico que tenemos no es tan obvio como la función `call/2`. Tenga en cuenta que todos los procesos que hemos escrito hasta ahora tienen un bucle donde todos los mensajes se corresponden con patrones. Esta parte es un poco delicada, pero aquí necesitamos separar la correspondencia de patrones del bucle en sí. Una forma rápida de hacerlo sería añadir esto:

---

```erlang
loop(Module, State) ->
	receive
		Message -> Module:handle(Message, State)
	end.
```

---

Entonces el modulo especifico debería de verse **así**:

---

```erlang
handle(Message1, State) -> NewState1;
handle(Message2, State) -> NewState2;
...
handle(MessageN, State) -> NewStateN.
```

---

Esto es mejor, pero hay maneras de hacerlo aún más claro y entendible.
Si prestaste atención al leer o ingresar al módulo `kitty_server` (Y espero que sí, mi amiguito!), habrás notado que tenemos una forma específica de llamar *sincrónicamente* y otra forma de llamar *asincrónicamente*. Sería muy útil que nuestra implementación genérica del servidor pudiera proporcionar una forma clara de identificar cada tipo de llamada.
Para lograr esto, necesitaremos hacer coincidir diferentes tipos de mensajes en `my_server:loop/2`. Esto significa que necesitaremos modificar un poco la función `call/2` para que las llamadas *sincrónicas* sean obvias. Lo haremos añadiendo el átomo sync al mensaje en la segunda línea de la función, como se muestra a continuación:

---

```erlang
call(Pid, Msg) ->
	Ref = erlang:monitor(process, Pid),
	Pid ! {sync, self(), Ref, Msg},
	receive
		{Ref, Reply} ->
			erlang:demonitor(Ref, [flush]),
			Reply;
		{'DOWN', Ref, process, Pid, Reason} ->
			erlang:error(Reason)
		after 5000 ->
			erlang:error(timeout)
	end.
```

---

Ahora podemos proporcionar una nueva función para llamadas *asíncronas*. La función `cast/2` se encargará de esto:

---

```erlang
cast(Pid, Msg) ->
	Pid ! {async, Msg},
	ok.
```

---

Ahora el `loop` se ve así:

---

```erlang
loop(Module, State) ->
	receive
		{async, Msg} ->
			loop(Module, Module:handle_cast(Msg, State));
		{sync, Pid, Ref, Msg} ->
			loop(Module, Module:handle_call(Msg, Pid, Ref, State))
	end.
```

---

También podrías agregar ranuras específicas para gestionar mensajes que no se ajusten al concepto *sincrónico/asincrónico* (quizás enviados accidentalmente) o para incluir tus funciones de depuración y otras funciones como la recarga de código activo.
Un aspecto decepcionante de nuestro bucle es que la *abstracción* tiene fugas. Los programadores que usen `my_server` aún necesitarán conocer las referencias al enviar mensajes sincrónicos y responderlos. Esto hace que la abstracción sea inútil. Para usarla, aún necesitas comprender todos los detalles aburridos. Aquí tienes una solución rápida:

---

```erlang
loop(Module, State) ->
	receive
		{async, Msg} ->
			loop(Module, Module:handle_cast(Msg, State));
		{sync, Pid, Ref, Msg} ->
			loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
	end.
```

---

Con las variables `Pid` y `Ref` ubicadas en una tupla, se pueden pasar como un único argumento a la otra función como una variable con un nombre como From.
Así, el usuario no necesita saber nada sobre el funcionamiento interno de la variable.
En su lugar, proporcionaremos una función para enviar respuestas que debería comprender el contenido de From:

---

```erlang
reply({Pid, Ref}, Reply) ->
	Pid ! {Ref, Reply}.
```

---

### Iniciador de funciones

Lo que queda por hacer es especificar las funciones de inicio (start, start_link e init) que transmiten los nombres de los módulos, etc. Una vez añadidas, el módulo debería verse así:

---

```erlang
-module(my_server).
-export([start/2, start_link/2, call/2, cast/2, reply/2]).

%%% Public API
start(Module, InitialState) ->
	spawn(fun() -> init(Module, InitialState) end).
	
start_link(Module, InitialState) ->
	spawn_link(fun() -> init(Module, InitialState) end).
	
call(Pid, Msg) ->
	Ref = erlang:monitor(process, Pid),
	Pid ! {sync, self(), Ref, Msg},
	receive
		{Ref, Reply} ->
			erlang:demonitor(Ref, [flush]),
			Reply;
		{'DOWN', Ref, process, Pid, Reason} ->
			erlang:error(Reason)
		after 5000 ->
			erlang:error(timeout)
		end.
	
cast(Pid, Msg) ->
	Pid ! {async, Msg},
	ok.
	
reply({Pid, Ref}, Reply) ->
	Pid ! {Ref, Reply}.
	
%%% Private stuff
init(Module, InitialState) ->
	loop(Module, Module:init(InitialState)).
	loop(Module, State) ->
	receive
		{async, Msg} ->
			loop(Module, Module:handle_cast(Msg, State));
		{sync, Pid, Ref, Msg} ->
			loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
	end.
```

---

### Generalizando el `"kitty" server`

A continuación, debemos re-implementar el servidor **Kitty**, ahora `kitty_server2`, como un módulo de devolución de llamada que respete la interfaz que definimos para `my_server`. Mantendremos la misma interfaz que en la implementación anterior, excepto que todas las llamadas ahora se redirigen a través de `my_server`.

---

```erlang
-module(kitty_server2).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-record(cat, {name, color=green, description}).

%%% Client API
start_link() -> my_server:start_link(?MODULE, []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
	my_server:call(Pid, {order, Name, Color, Description}).

%% This call is asynchronous.
return_cat(Pid, Cat = #cat{}) ->
	my_server:cast(Pid, {return, Cat}).
	
%% Synchronous call
close_shop(Pid) ->
	my_server:call(Pid, terminate).
```

---

Tenga en cuenta que añadimos un segundo -export() al principio del módulo. Estas son las funciones que my_server deberá llamar para que todo funcione:

---

```erlang
%%% Server functions
init([]) -> []. %% no treatment of info here!

handle_call({order, Name, Color, Description}, From, Cats) ->
	if Cats =:= [] ->
		my_server:reply(From, make_cat(Name, Color, Description)),
		Cats;
	Cats =/= [] ->
		my_server:reply(From, hd(Cats)),
		tl(Cats)
	end;
handle_call(terminate, From, Cats) ->
	my_server:reply(From, ok),
	terminate(Cats).
	
handle_cast({return, Cat = #cat{}}, Cats) ->
	[Cat|Cats].
```

---

Luego lo que necesitamos es reinsertar las funciones privadas:

---

```erlang
%%% Private functions
make_cat(Name, Col, Desc) ->
	#cat{name=Name, color=Col, description=Desc}.
	
terminate(Cats) ->
	[io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
	exit(normal).
```

---

Solo asegúrate de reemplazar el `ok` que teníamos antes por `exit(normal)` en `terminate/1`; de lo contrario, el servidor seguirá ejecutándose.
Deberías poder compilar y probar el código, y ejecutarlo exactamente igual que en la versión anterior. El código es bastante similar, pero veamos qué ha cambiado.

---
## Lo especifico contra lo genérico

Nuestro ejemplo del servidor *Kitty* demuestra la esencia de **OTP** (conceptualmente hablando). De esto se trata realmente **OTP**: tomar todos los componentes genéricos, extraerlos en bibliotecas, asegurarse de que funcionen correctamente y luego reutilizar ese código cuando sea posible. Luego, solo queda centrarse en lo específico, algo que siempre cambiará de una aplicación a otra.
Obviamente, no se obtiene mucho beneficio de esta manera con solo el servidor *Kitty*. Parece un poco abstracción por la abstracción misma. Si la aplicación que se necesita enviar a un cliente no fuera más que el servidor *Kitty*, entonces la primera versión podría ser suficiente. Sin embargo, para aplicaciones más grandes, podría valer la pena separar las partes genéricas del código de las secciones específicas.
Imaginemos que tenemos un software **Erlang** ejecutándose en un servidor. Nuestro software cuenta con varios servidores para gatitos en ejecución, un proceso veterinario (envías a tus gatitos dañados y te los devuelve curados), un salón de belleza para gatos, un servidor para comida de mascotas, etc. La mayoría de estos se pueden implementar con un patrón *cliente-servidor*. Con el tiempo, tu complejo sistema se llena de diferentes servidores funcionando.
Añadir servidores añade complejidad en términos de código, así como en términos de pruebas, mantenimiento y comprensión. Cada implementación puede ser diferente, estar programada con distintos estilos por distintas personas, etc.
Sin embargo, si todos estos servidores comparten la misma abstracción común `my_server`, se reduce sustancialmente esa complejidad. Se comprende el concepto básico del módulo al instante, y hay una única implementación genérica para probar y documentar. El resto del esfuerzo se puede dedicar a cada implementación específica del servidor.
Esto significa que se reduce considerablemente el tiempo dedicado al seguimiento y la resolución de errores (simplemente se realiza en un solo lugar para todos los servidores). También significa que reduces la cantidad de errores que introduces. Si tuvieras que reescribir el bucle `my_server:call/3` o el bucle principal del proceso constantemente, no solo consumiría más tiempo, sino que la probabilidad de olvidar algún paso se dispararía, y con ello los errores. Menos errores significan menos llamadas nocturnas para corregir algo, lo cual es definitivamente bueno para todos (apuesto a que tampoco aprecias ir a la oficina en tus días libres para corregir errores).
Otro resultado interesante de separar lo genérico de lo específico es que facilitamos al instante la prueba de nuestros módulos individuales.
Si quisieras realizar pruebas unitarias a la antigua implementación del servidor *Kitty*, tendrías que generar un proceso por prueba, asignarle el estado correcto, enviar tus mensajes y esperar la respuesta esperada. Por otro lado, nuestro segundo servidor *Kitty* requiere que ejecutemos las llamadas de función únicamente sobre las funciones `handle_call/3` y `handle_cast/2`, y veamos qué generan como nuevo estado. No es necesario configurar servidores. Simplemente pase el estado como parámetro de función.
Tenga en cuenta que esto también significa que el aspecto genérico del servidor es mucho más fácil de probar, ya que puede implementar funciones muy simples que solo le permiten concentrarse en el comportamiento que desea observar.
Una ventaja menos obvia de usar abstracciones comunes de esta manera es que si todos usan exactamente el mismo backend para sus procesos, cuando alguien optimiza ese backend único para hacerlo un poco más rápido, todos los procesos que lo usan también se ejecutarán un poco más rápido. Para que este principio funcione en la práctica, suele ser necesario que muchas personas usen las mismas abstracciones y se esfuercen en ellas. Afortunadamente para la comunidad de **Erlang**, eso es lo que ocurre con el *framework* **OTP**. En nuestros módulos de servidor *Kitty*, hay varios aspectos que aún no hemos abordado: procesos con nombre, configuración de tiempos de espera, adición de información de depuración, qué hacer con mensajes inesperados, cómo integrar la carga de código activo, gestión de errores específicos, eliminación de la necesidad de escribir la mayoría de las respuestas, gestión de la mayoría de las formas de apagar un servidor, garantizar que el servidor se comporte correctamente con los **supervisores**, y más. Repasar todo esto es superfluo en este texto, pero sería necesario en productos reales que deban lanzarse. De nuevo, puede que veas por qué hacer todo esto por tu cuenta es una tarea un tanto arriesgada. Por suerte para ti (y para quienes darán soporte a tus aplicaciones), el equipo de **Erlang/OTP** logró gestionar todo esto con el comportamiento de `gen_server`. `gen_server` es como `my_server` pero con esteroides, ademas cuenta con años de pruebas y uso en producción.

---

## "Callback to the future!"

Similar a la interfaz que comenzamos a diseñar en este capítulo, el **OTP** `gen_server` nos pide que proporcionemos funciones para gestionar la inicialización y finalización de procesos, el manejo de solicitudes síncronas y asincrónicas realizadas mediante el paso de mensajes y algunas otras tareas.

---

### La función `init`

La primera función de retorno es una función `init/1`. Es similar a la que usamos con `my_server`, ya que se utiliza para inicializar el estado del servidor y realizar todas las tareas puntuales de las que depende. La función puede devolver `{ok, Estado}`, `{ok, Estado, Tiempo de Espera}`, `{ok, Estado, hibernate}`, `{stop, Razón}` o `ignore`. El valor de retorno *normal* `{ok, Estado}` no necesita explicación, salvo que el `Estado` se pasará directamente al bucle principal del proceso como el estado que se conservará posteriormente. La variable `Tiempo de Espera`(Timeout) se añade a la tupla siempre que se necesite una fecha límite antes de la cual se espera que el servidor reciba un mensaje. Si no se recibe ningún mensaje antes de la fecha límite, se envía al servidor un mensaje especial (el átomo `timeout`), que debe gestionarse con `handle_info/2` (descrito más adelante en este capítulo). Esta opción rara vez se usa en código de producción, ya que no siempre se sabe qué mensajes se recibirán, y cualquiera de ellos será suficiente para reiniciar el temporizador. Suele ser mejor usar una función como `erlang:start_timer/3` y gestionar los procesos manualmente para un mejor control.
Por otro lado, si se prevé que el proceso tardará mucho tiempo en recibir una respuesta y se preocupa por la memoria, se puede añadir el átomo`hibernate` a la tupla. La *hibernación* reduce el tamaño del estado del proceso hasta que recibe un mensaje, a costa de cierta potencia de procesamiento.
Si se duda sobre el uso de la *hibernación*, probablemente no sea necesaria.
La devolución de `{stop, Reason}` debería realizarse cuando algo salió mal durante la inicialización.

>[!Vistazo de cerca a la hibernación]
>Existe una definición más técnica de hibernación de procesos, si te interesa.
  Cuando se llama a la función BIF `erlang:hibernate(M,F,A)`, se descarta la pila de llamadas del proceso en ejecución (la función nunca retorna). La recolección de basura se activa entonces, y lo que queda es un montón continuo que se reduce al tamaño de los datos del proceso. Esto básicamente compacta todos los datos para que el proceso ocupe menos espacio.
  Una vez que el proceso recibe un mensaje, se llama a la función `M:F` con `A` como argumentos y se reanuda la ejecución.

>[!Notita]
>Mientras `init/1` se ejecuta, la ejecución del proceso que generó el servidor está bloqueada. Esto se debe a que está esperando un mensaje de "*listo*" enviado automáticamente por el módulo `gen_server` para asegurarse de que todo salió bien.

---

### La función `handle_call`

La función `handle_call/3` es usada para trabajar con mensajes sincrónicos. Esta toma 3 argumentos, `Request`(Solicitud), `From` y `State`(Estado). Es muy similar a como programamos nuestro `handle_call/3` en `my_server`. La mayor diferencia es la forma en que se le responde a los mensajes. En nuestra abstracción de un servidor, era necesario el uso de `my_server:reply/2` para responderle al proceso. En este caso(De un `gen_server`), son posibles ocho valores de retorno diferentes, en forma de tuplas:

1. `{reply, Respuesta, NuevoEstado}`
2. `{replay, Respuesta, NuevoEstado, Timeout}`
3. `{reply, Respuesta, NuevoEstado, hibernate}`
4. `{noreplay, NuevoEstado}`
5. `{noreplay, NuevoEstado, TimeOut}`
6. `{noreplay, NuevoEstado, hibernate}`
7. `{stop, Razon, Respuesta, NuevoEstado}`
8. `{stop, Razon, NuevoEstado}`

Para todos estos valores, `TimeOut` y el átomo `hibernate` trabajan de la misma forma que para `init/1`. Lo que sea que este en `Respuesta` se retornara a quien haya hecho la llamada al servidor en principios.
Ten en cuenta que hay tres opciones posibles para `noreply`. Al usar `noreply`, la parte genérica del servidor asumirá que te encargas de enviar la respuesta tú mismo. Esto se puede hacer con `gen_server:reply/2`, que se usa de la misma manera que `my_server:reply/2`.
Generalmente, solo necesitarás las tuplas de respuesta. Sin embargo, existen algunas razones válidas para usar `noreply`, como cuando quieres que otro proceso envíe la respuesta por ti o cuando quieres enviar un acuse de recibo (“¡Hola! ¡Recibí el mensaje!”, o algo asi) pero procesarlo después (sin responder esta vez). Si eliges esta opción, es absolutamente necesario usar `gen_server:reply/2`; de lo contrario, la llamada se interrumpirá y provocará un fallo.

---
### La función `handle_cast`

La devolución de llamada `handle_cast/2` funciona de forma muy similar a la de `my_server`. Toma los parámetros `Message`(O `Msg`) y `State`(O `Estado`) y se utiliza para gestionar llamadas asíncronas. Puedes hacer lo que quieras, de forma similar a lo que se puede hacer con `handle_call/3`. Por otro lado, solo las tuplas sin respuestas son valores de retorno válidos:

1. `{noreplay, NuevoEstado}`
2. `{noreplay, NuevoEstado, Timeout}`
3. `{noreplay, NuevoEstado, hibernate}`
4. `{stop, Razon, NuevoEstado}`

---

### La función `handle_info`

Anteriormente, mencioné que nuestro servidor no gestionaba los mensajes que no se ajustaban a nuestra interfaz. Bueno, `handle_info/2` es la solución. Es muy similar a `handle_cast/2` y, de hecho, devuelve las mismas tuplas. La diferencia está en que esta devolución de llamada solo está disponible para los mensajes enviados directamente con el operador `!` y los especiales, como el tiempo de espera de `init/1`, las notificaciones de los monitores y las señales `EXIT`.

---

### La función `terminate`

La función de retorno `terminate/2` se invoca siempre que una de las tres funciones `handle_something` devuelva una tupla con la forma `{stop, Razon, NuevoEstado}` o` {stop, Razon, Respuesta, NuevoEstado}`. Toma dos parámetros, `Razon` y `Estado`, que corresponden a los mismos valores de las tuplas de parada.
La función `terminate/2` también se invocará cuando su padre (el proceso que la generó) muera, solo si `gen_server` está interceptando salidas.

>[!Notita]
>Si se utiliza cualquier motivo distinto a `normal`, `shutdown` o `{shutdown, Term}` al llamar a `terminate/2`, el *framework* **OTP** lo considerará un fallo y comenzará a registrar el estado del proceso, el motivo de los fallos, los últimos mensajes recibidos, etc. Esto facilita la depuración, lo que podría salvarle la vida en muchas ocasiones.

Esta función es prácticamente lo opuesto a `init/1`, por lo que cualquier acción realizada allí debería tener su opuesto en `terminate/2`. Es el conserje de tu servidor: la función encargada de cerrar la puerta después de asegurarse de que todos se hayan ido. Por supuesto, la función cuenta con el apoyo de la propia máquina virtual, que normalmente debería eliminar todas las tablas `ETS` (**véase el Capítulo 25**), cerrar todos los `sockets` (**véase el Capítulo 23**) y encargarse de otras tareas. Ten en cuenta que el valor de retorno de esta función no importa realmente, ya que el código deja de ejecutarse después de ser llamado.

---

### La función `code_change`

La función `code_change/3` permite actualizar el código. Su formato es `code_change(PreviousVersion, State, Extra)`. Aquí, la variable `PreviousVersion` es el término de la versión (**consulta el Capítulo 2 si no lo recuerdas**) en caso de una actualización o `{down, Version}` en caso de una degradación (simplemente recargando código antiguo). La variable `State` contiene todo el estado actual del servidor para que puedas convertirlo.
Imagina por un momento que usáramos un `orddict` para almacenar todos nuestros datos. Sin embargo, con el tiempo, el `orddict` se vuelve demasiado lento y decidimos reemplazarlo con un *diccionario* normal. Para evitar que el proceso se bloquee en la siguiente llamada a la función, la conversión de una estructura de datos a otra se puede realizar allí de forma segura. Solo necesitamos devolver el nuevo estado con `{ok, NewState}`. Usaremos esta función en el **Capítulo 22**, cuando veamos las repeticiones y la variable `Extra`. No nos preocuparemos por esto por ahora.
Así que ya tenemos todas las devoluciones de llamada definidas. No te preocupes si estás un poco perdido. El framework `OTP` a veces es un poco circular: para entender la parte **A** del *framework*, necesitas entender la parte **B**, pero la parte **B** requiere que entiendas la parte **A**. La mejor manera de evitar esa confusión es implementar un `gen_server`.

---

## .BEAM me up, Scotty!

Ahora construiremos el `kitty_gen_server`!
Bien, sera bastante similar al `kitty_server2`, solo con cambios mínimos en la `API`. Primero creemos el nuevo modulo:

---

```erlang
-module(kitty_gen_server).
-behavior(gen_server).
```

---
>Curioso es que `behavior` y `behaviour` son aceptados de igual manera en el compilador de **Erlang**. Mientras lo escribía(Creyendo que estaba mal me percate(Antes de leerlo)).

Si tratamos de compilar esto:

---

```erlang
1> c(kitty_gen_server).
./kitty_gen_server.erl:2: Warning: undefined callback function code_change/3
(behavior 'gen_server')
./kitty_gen_server.erl:2: Warning: undefined callback function handle_call/3
(behavior 'gen_server')
./kitty_gen_server.erl:2: Warning: undefined callback function handle_cast/2
(behavior 'gen_server')
./kitty_gen_server.erl:2: Warning: undefined callback function handle_info/2
(behavior 'gen_server')
./kitty_gen_server.erl:2: Warning: undefined callback function init/1
(behavior 'gen_server')
./kitty_gen_server.erl:2: Warning: undefined callback function terminate/2
(behavior 'gen_server')
{ok,kitty_gen_server}
```

---

Eso funciona... mas o menos. Las alertas hablan de que faltan `call-backs`. Esto es por el `behavior(gen_server)`. un comportamiento o `behavior` es escencialmente una de especificarle a un modulo la forma que debe adoptar. El `behavior` es el contrato que cierra el trato entre la parte genérica del código que funciona bien y la parte específica y propensa a errores (la **tuya**).

---

***==Definamos behaviors==***
Hacer tus propios `behaviors` es de hecho bastante sencillo. Solo necesitas exportar una función llamada `behavior_info/1`. Implementada así:

---
```erlang
-module(my_behavior).
-export([behavior_info/1]).
%% init/1, some_fun/0 and other/3 ahora son callbacks esperados.
behavior_info(callbacks) -> [{init,1}, {some_fun, 0}, {other, 3}];
behavior_info(_) -> undefined.
```

---

Y eso es todo al respecto. Ahora puedes usar `-behavior(my_behavior).` en cualquier modulo, implementalos para obtener alertas al compilar si olvidas una función!

---

Volviendo al `kitty_gen_server` la primera función que teníamos era `start_link/0`.  Podemos cambiarla a:

---

```erlang
start_link() -> gen_server:start_link(?MODULE, [], []).
```

---

El primer parámetro es el módulo de devolución de llamada, el segundo es un término que se pasa a `init/1` y el tercero se refiere a las opciones de depuración para servidores en ejecución. Se podría agregar un cuarto parámetro en la primera posición: `{local, Name}`, que es el nombre con el que se registrará el servidor. Tenga en cuenta que mientras que la versión anterior de la función simplemente devolvía un `PID`, esta devuelve `{ok, Pid}`.
Las siguientes funciones ahora son estas:

---

```erlang
%% llamadas sincronas
order_cat(Pid, Name, Color, Description) ->
	gen_server:call(Pid, {order, Name, Color, Description}).
	
%% Esta es asincrona.
return_cat(Pid, Cat = #cat{}) ->
	gen_server:cast(Pid, {return, Cat}).
	
%% Sincrona
close_shop(Pid) ->
	gen_server:call(Pid, terminate).
```

---

Todas estas llamadas son equivalentes a las que teníamos en `my_server`. Tenga en cuenta que se puede pasar un tercer parámetro a `gen_server:call` para establecer un tiempo de espera en *milisegundos*.
Si no se establece un tiempo de espera para la función (o para el *átomo* `infinity`), el valor predeterminado es de **5** segundos. Si no se recibe respuesta antes de que se agote el tiempo, la llamada se bloquea. Este valor es totalmente arbitrario, y muchos usuarios habituales de **Erlang** le dirán que debería cambiarse a `infinity`. En mi experiencia, a menudo quería que las respuestas llegaran en menos de **5** segundos, y que este temporizador forzara los bloqueos generalmente me ha ayudado a diagnosticar problemas más importantes.
Ahora podremos agregar las devoluciones de llamada de `gen_server`. La Tabla *14-1* muestra la relación entre las llamadas y las devoluciones de llamada.

![alt](Media/Pasted%20image%2020250817181155.png)

Luego tenemos los demas parametros, que son mas bien dedicados a casos especiales: `handle_info/2`, `terminate/2` y `code_change/3`.
Comencemos cambiando los que ya tenemos para que encaje con el modelo: `init/1`, `handle_call/3` y `handle_cast/2`.

---

```erlang
%%% Funciones del servidor
init([]) -> {ok, []}. %% no se maneja la info aca!

handle_call({order, Name, Color, Description}, _From, Cats) ->
	if Cats =:= [] ->
		{reply, make_cat(Name, Color, Description), Cats};
	Cats =/= [] ->
		{reply, hd(Cats), tl(Cats)}
	end;
handle_call(terminate, _From, Cats) ->
	{stop, normal, ok, Cats}.
	
handle_cast({return, Cat = #cat{}}, Cats) ->
	{noreply, [Cat|Cats]}.
```

---

Nuevamente, aquí se han producido pocos cambios. De hecho, el código ahora es más corto gracias a abstracciones más inteligentes.
Ahora llegamos a las nuevas devoluciones de llamada. La primera es `handle_info/2`. Dado que este es un módulo de juguete y no tenemos un sistema de registro predefinido, bastará con mostrar los mensajes inesperados.

---

```erlang
handle_info(Msg, Cats) ->
	io:format("Unexpected message: ~p~n",[Msg]),
	{noreply, Cats}.
```

---

Como regla general, registre siempre los mensajes inesperados en
`handle_cast/2` y `handle_info/2`. También podría querer registrarlos en
`handle_call/3`, pero, en general, no responder a las llamadas (junto con el tiempo de espera predeterminado de **5** segundos) es suficiente para lograr el mismo resultado.
La siguiente es la función de devolución de llamada `terminate/2`. Será muy similar a la función privada `terminate/1` que usamos anteriormente.

---

```erlang
terminate(normal, Cats) ->
	[io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
	ok.
```

---

Y aca estaría el ultimo parámetro, `code_change/3`:

---

```erlang
code_change(_OldVsn, State, _Extra) ->
%% Solo la pondremos por ahora por el behavoir,
%% pero no la usaremos.
{ok, State}.
```

---

*Solo recuerda mantener en privada la función `make_cat/3`*

*Bien!* Ahora podemos probar todo nuestro nuevo código c:

---

```erlang
1> c(kitty_gen_server).
{ok,kitty_gen_server}
2> rr(kitty_gen_server).
[cat]
3> {ok, Pid} = kitty_gen_server:start_link().
{ok,<0.253.0>}
4> Pid ! <<"Test handle_info">>.
Unexpected message: <<"Test handle_info">>
<<"Test handle_info">>
5> Cat = kitty_gen_server:order_cat(Pid, "Cat Stevens",
5> white, "not actually a cat").
#cat{name = "Cat Stevens",color = white,
description = "not actually a cat"}
6> kitty_gen_server:return_cat(Pid, Cat).
ok
7> kitty_gen_server:order_cat(Pid, "Kitten Mittens",
7> black, "look at them little paws!").
#cat{name = "Cat Stevens",color = white,
description = "not actually a cat"}
```

---

Como devolvimos el gato `Cat` al servidor, nos lo devuelven antes de poder pedir nada nuevo. Si lo volvemos a intentar, deberíamos conseguir lo que queremos.

---

```erlang
8> kitty_gen_server:order_cat(Pid, "Kitten Mittens",
8> black, "look at them little paws!").
#cat{name = "Kitten Mittens",color = black,
description = "look at them little paws!"}
9> kitty_gen_server:return_cat(Pid, Cat).
ok
10> kitty_gen_server:close_shop(Pid).
"Cat Stevens" was set free.
ok
```

---

¡Candelaaaaa, funciona esto!
¿Qué podemos decir de esta aventura genérica?
Probablemente lo mismo de antes: separar lo genérico de lo específico es una gran idea en todos los aspectos. El mantenimiento es más sencillo. La complejidad se reduce.
El código es más seguro, más fácil de probar y menos propenso a errores.
Y si hay errores, son más fáciles de corregir.
Los servidores genéricos son solo una de las muchas abstracciones disponibles, pero sin duda son una de las más utilizadas. Exploraremos más sobre estas abstracciones y comportamientos en los próximos capítulos.