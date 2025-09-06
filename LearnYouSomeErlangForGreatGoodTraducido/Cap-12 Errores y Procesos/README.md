
---

## Introducción

En la mayoría de los lenguajes, las excepciones se gestionan desde el flujo de ejecución del programa, como lo hicimos con `try` y `catch` en ejemplos anteriores.
El problema con este enfoque tan común es que el código normal debe gestionar los errores pendientes en todos los niveles, o simplemente se delega la carga de garantizar la seguridad en la capa superior, hasta que se termina con el eterno `try` ... `catch` de nivel superior, que lo captura todo, pero no sabe nada al respecto. Es más complejo en el mundo real, pero así es como se ve generalmente.
**Erlang** también admite este modelo, como ya se ha visto.
Sin embargo, **Erlang** también admite un nivel diferente de gestión de excepciones que permite trasladar la gestión de excepciones fuera del flujo normal de ejecución del programa, a un proceso concurrente diferente. Esto suele dar como resultado un código muy limpio, donde solo se considera el caso ideal.
En este capítulo, analizamos las herramientas básicas que lo hacen posible: `links`, `monitors` y procesos nombrados(`named processes` - traducción original). También abordaremos algunas prácticas generales que hacen que el uso de estas herramientas sea más eficiente.

---

## Links

Un **link** es una especifica clase de relación que puede crearse entre dos *procesos*. Cuando la relación se establece y uno de los dos procesos muere por un error, un `unexpected throw` o directamente por un `exit`(Ver en el capitulo 7 del libro), el otro proceso enlazado también muere, enlazando sus separados ciclos de vida en uno solo.
Esto es un concepto muy util desde la perspectiva de fallar lo antes posible para parar los errores. Si el proceso que tiene el error falla, pero los que dependen de el continúan corriendo entonces todos estos que dependían de el van a verse lidiando con una dependencia desaparecida. Dejándolos morir a todos y luego reiniciandolos a todos es usualmente una alternativa aceptable. Los **links** nos permiten hacer exactamente esto.
Para establecer un **link** entre dos procesos, **Erlang** tiene una función nativa llamada `link/1`, la cual toma un `PID` como argumento. AL llamarla, crea un enlace entre el actual proceso y el dueño de la `PID` enviada. Para separarlos, sin mas se debe usar `unlink/1`.
Cuando uno de los procesos enlazados falla, un tipo especial de mensaje es enviado con información relacionada con el problema. Este mensaje no se envía jamas si el proceso muere de forma natural.

Veamos cómo funciona esta nueva función, como parte del archivo `linkmon.erl`:

---

```erlang
myproc() ->
	timer:sleep(5000),
	exit(reason).
```

---

 Si intentas las llamadas siguientes y esperas 5 segundos entre cada comando `spawn` deberías poder ver el `shell` rompiéndose solo por alguna razon cuando se establece un vinculo entre los dos procesos:
 
---

```erlang
1> c(linkmon).
{ok,linkmon}
2> spawn(fun linkmon:myproc/0).
<0.52.0>
3> link(spawn(fun linkmon:myproc/0)).
true
** exception error: reason
```

---

Aqui una foto de como funciona esto:

![alt](Media/Pasted%20image%2020250817010000.png)

En todo caso, este mensaje `{'EXIT', B, Reason}` no puede ser capturado con una clausula `try` ... `catch`como es usual. Se necesita otro mecanismo para hacer esto, como se explica en `It's a Trap` mas adelante.

>[!Notita]
>Si quisieras terminar otro proceso desde el `shell`, podrías usar la función
  `exit/2`, que utiliza estos mecanismos para terminar procesos. Se llama así:
  `exit(Pid, Reason)`. Pruébala si quieres.

Los enlaces se utilizan para establecer grupos más grandes de procesos que deberían morir juntos. Aquí hay un ejemplo:

---

```erlang
chain(0) ->
	receive
	_ -> ok
	after 2000 ->
		exit("chain dies here")
	end;
	
chain(N) ->
	Pid = spawn(fun() -> chain(N-1) end),
	link(Pid),
	receive
		_ -> ok
	end.
```

---

Esta función tomará un entero `N` e iniciará `N` procesos enlazados. Para pasar el argumento `N-1` al siguiente proceso de "cadena" (que llama a `spawn/1`), el ejemplo encapsula la llamada dentro de una función anónima para que ya no necesite argumentos. Llamar a `spawn(?MODULE, chain, [N-1])` habría tenido un efecto similar.
Aquí, tendremos muchos procesos enlazados, que mueren a medida que cada uno de sus sucesores termina.

---

```erlang
4> c(linkmon).
{ok,linkmon}
5> link(spawn(linkmon, chain, [3])).
true
** exception error: "chain dies here"
```

---

Como pueden ver, el shell recibe la señal de muerte de otro proceso. Aquí tienen una representación gráfica de los procesos generados y los enlaces que se caen:

---

```erlang
[shell] == [3] == [2] == [1] == [0]
[shell] == [3] == [2] == [1] == *dead*
[shell] == [3] == [2] == *dead*
[shell] == [3] == *dead*
[shell] == *dead*
*dead, error message shown*
[shell] <-- restarted
```

---

Tras la interrupción del proceso que ejecuta `linkmon:chain(0)`, el error se propaga a lo largo de la cadena de enlaces hasta que el propio proceso de `shell` se bloquea. El fallo podría haber ocurrido en cualquiera de los procesos enlazados. Dado que los enlaces son bidireccionales, basta con que uno de ellos se bloquee para que los demás también lo hagan.

>[!Notita]
>Los enlaces no se pueden apilar. Si se llama a `link/1` quince veces para los mismos dos procesos, solo existirá un enlace entre ellos, y una sola llamada a `unlink/1` será suficiente para destruirlo.

Tenga en cuenta que `link(spawn(Function))` o `link(spawn(M,F,A))` ocurren en más de un paso. En algunos casos, es posible que un proceso muera antes de que se establezca el enlace y provoque un comportamiento inesperado. Por esta razón, se ha añadido al lenguaje la función `spawn_link/1-3`. Esta función toma los mismos argumentos que `spawn/1-3`, crea un proceso y lo enlaza como si `link/1` ya existiera, excepto que todo se realiza como una operación atómica (las operaciones se combinan en una sola, que puede fallar o tener éxito, pero nada más). Esto generalmente se considera más seguro y, además, ahorra un paréntesis.

---

### It's a Trap!

La propagación de errores entre procesos se realiza mediante un proceso similar al paso de mensajes, pero con un tipo especial de mensaje llamado señales. Las señales de salida son mensajes "secretos" que actúan automáticamente sobre los procesos, eliminándolos. He mencionado repetidamente que, para ser confiable, una aplicación debe poder eliminar y reiniciar un proceso rápidamente. Actualmente, los **links** pueden servir para la eliminación. Lo que falta es el reinicio. Para reiniciar un proceso, primero necesitamos una forma de saber que ha finalizado. Esto se puede lograr añadiendo una capa de enlaces (la guinda del pastel) con un concepto llamado procesos del sistema. Los procesos del sistema son básicamente procesos normales, excepto que pueden convertir las señales de salida en mensajes normales. Esto se hace llamando a `process_flag(trap_exit, true)` en un proceso en ejecución. Nada es más elocuente que un ejemplo. Repitamos el ejemplo de la cadena con un proceso del sistema al principio:

---

```erlang
1> process_flag(trap_exit, true).
true
2> spawn_link(fun() -> linkmon:chain(3) end).
<0.49.0>
3> receive X -> X end.
{'EXIT',<0.49.0>,"chain dies here"}
```

---

Ah! Ahora la cosa se pone interesante. Para volver a los diagramitas de ejemplo, lo que ocurre se ve mas o menos asi:

---

```erlang
[shell] == [3] == [2] == [1] == [0]
[shell] == [3] == [2] == [1] == *dead*
[shell] == [3] == [2] == *dead*
[shell] == [3] == *dead*
[shell] <-- {'EXIT,Pid,"chain dies here"} -- *dead*
[shell] <-- still alive!
```

---

Y este es el mecanismo que permite reiniciar rápidamente los procesos. Al escribir programas usando procesos del sistema, es fácil crear un proceso cuya única función es comprobar si algo falla y reiniciarlo cuando falla. Abordaremos esto con más detalle en el Capítulo 13, donde aplicaremos estas técnicas.

---

### Viejas excepciones, Nuevos conceptos

Regresemos a las funciones de excepción presentadas en el Capítulo 7 y veamos cómo se comportan en procesos que interceptan salidas. Primero, estableceremos las bases para experimentar sin un proceso del sistema. Analizaremos los resultados de lanzamientos, errores y salidas no interceptadas en procesos vecinos.

---

### Excepciones y trampas

Hay muchas razones por las que los procesos suelen morir. Veamos algunas y cómo se manifiestan cuando las salidas quedan atrapadas:

**Fuente de la excepción**: `spawn_link(fun() -> ok end)`
	**Resultado no Capturado**: Ninguno
	**Resultado Capturado**: `{'EXIT', <0.61.0>, normal}`
	El proceso finalizó normalmente, sin problemas. Tenga en cuenta que esto se parece un poco al resultado de catch `exit(normal)`, excepto que se añade un PID a la tupla para identificar qué proceso falló.

**Fuente de la excepción**: `spawn_link(fun() -> exit(reason) end)`
	**Resultado no Capturado**: `** exception exit: reason`
	**Resultado Capturado**: `{'EXIT', <0.55.0>, reason}`
	El proceso ha finalizado por una razón específica. Si no hay una salida atrapada, el proceso se bloquea. Al salir, se recibe un mensaje.

**Fuente de la excepción**: `spawn_link(fun() -> exit(normal) end)`
	**Resultado no Capturado**: Ninguno
	**Resultado Capturado**: `{'EXIT', <0.58.0>, normal}`
	Esto emula correctamente un proceso que termina normalmente. En algunos casos, es posible que desee finalizar un proceso como parte del flujo normal de un programa, sin que ocurra nada excepcional. Esta es la forma de hacerlo.

**Fuente de la excepción**: `spawn_link(fun() -> 1/0 end)`
	**Resultado no Capturado**: 
	`Error in process <0.44.0> with exit value: {badarith, [{erlang, '/', [1,0]}]}`
	**Resultado Capturado**: `{'EXIT', <0.52.0>, {badarith, [{erlang, '/', [1,0]}]}}`
	El error `({badarith, Reason})` nunca se detecta con un bloque `try` ... `catch` y se convierte en un `EXIT`. En este punto, se comporta exactamente igual que `exit(reason)`, pero con un seguimiento de la pila que proporciona más detalles sobre lo sucedido.

**Fuente de la excepción**: `spawn_link(fun() -> erlang:error(reason) end)`
	**Resultado no Capturado**: 
	`Error in process <0.47.0> with exit value: {reason, [{erlang, apply, 2}]}`
	**Resultado Capturado**: `{'EXIT', <0.74.0>, {reason, [{erlang, apply, 2}]}}`
	Esto es prácticamente igual que con `1/0`. Es normal: `erlang:error/1` está diseñado para permitirte hacer precisamente eso.

**Fuente de la excepción**: `spawn_link(fun() -> throw(rocks) end)`
	**Resultado no Capturado**: 
	`Error in process <0.51.0> with exit value: {{nocatch, rocks}, [{erlang, apply, 2}]}`
	**Resultado Capturado**: `{'EXIT', <0.79.0>, {{nocatch, rocks}, [{erlang, apply, 2}]}}`
	Como el lanzamiento nunca es interceptado por un `try` ... `catch`, se convierte en un error, que a su vez se convierte en una salida. Sin interceptar salidas, el proceso falla. Mientras intercepte salidas, gestiona el error sin problemas.

Y eso es todo en cuanto a las excepciones habituales. Todo es normal y todo va bien. Suceden cosas excepcionales, los procesos fallan y se envían diferentes señales.

---
### `exit/2` Cambia todo!

Luego está `exit/2`. Este es el equivalente a un arma en el proceso **Erlang**. Permite que un proceso elimine a otro a distancia, de forma segura. A continuación, se presentan algunas de las posibles llamadas:

**Fuente de la excepción**: `exit(self(), normal)`
	**Resultado no Capturado**:  `** exception exit: normal`
	**Resultado Capturado**: `{'EXIT', <0.31.0>, normal}`
	Al no atrapar salidas, `exit(self(), normal)` actúa igual que `exit(normal)`. De lo contrario, recibirá un mensaje con el mismo formato que habría recibido al escuchar los enlaces de procesos externos que terminan.
	
**Fuente de la excepción**: `exit(spawn_link(fun() -> timer:sleep(50000) end), normal)`
	**Resultado no Capturado**:  *Nada*
	**Resultado Capturado**:  *Nada*
	Esto básicamente es una llamada a exit(Pid, normal). Este comando no tiene ninguna utilidad, ya que un proceso no se puede finalizar remotamente con el argumento `razón normal` como argumento.

**Fuente de la excepción**: `exit(spawn_link(fun() -> timer:sleep(50000) end), reason)`
	**Resultado no Capturado**:  `** exception exit: reason`
	**Resultado Capturado**:  `{'EXIT', <0.52.0>, reason}`
	Este es el proceso externo que termina por la razón misma. Parece lo mismo que si el proceso externo llamara a la salida (razón) sobre sí mismo.
	
**Fuente de la excepción**: `exit(spawn_link(fun() -> timer:sleep(50000) end), kill)`
	**Resultado no Capturado**:  `** exception exit: killed`
	**Resultado Capturado**:  `{'EXIT', <0.58.0>, killed}`
	Sorprendentemente, el mensaje cambia del proceso de muerte al generador. El generador ahora recibe `killed` en lugar de `kill`. Esto se debe a que `kill` es una señal de salida especial, como se explica en la siguiente sección.

**Fuente de la excepción**: `exit(self(), kill)`
	**Resultado no Capturado**:  `** exception exit: killed`
	**Resultado Capturado**:  `** exception exit: killed`
	¡Uy, mira eso! Parece que es imposible atraparlo. La siguiente excepción no lo hace más fácil.

**Fuente de la excepción**: `spawn_link(fun() -> exit(kill) end)`
	**Resultado no Capturado**:  `** exception exit: killed`
	**Resultado Capturado**:  `{'EXIT', <0.67.0>, kill}`
	Esto se está volviendo confuso. Cuando otro proceso se autodestruye con exit(kill), y no se capturan las salidas, el propio proceso se elimina con la razón. Sin embargo, cuando se capturan las salidas, no sucede así.

---

### Matándome (no tanto...) osea, suavemente

Aunque se pueden interceptar la mayoría de las razones de salida, hay situaciones en las que podría querer eliminar brutalmente un proceso. Quizás uno de sus procesos esté interceptando salidas, pero también está atascado en un bucle infinito, sin leer ningún mensaje(Desastroso).
La razón de interrupción actúa como una señal especial que no se puede interceptar. Esto garantiza que cualquier proceso que se termine con ella esté realmente muerto. Normalmente, la interrupción es un último recurso cuando todo lo demás ha fallado.
Como la razón de interrupción nunca se puede interceptar, debe cambiarse a `killed` cuando otros procesos reciben el mensaje. Si no se cambiara, todos los demás procesos vinculados a ella morirían a su vez por la misma razón de interrupción, y a su vez, eliminarían a sus vecinos, y así sucesivamente. Se produciría una cascada de interrupciones.
Esto también explica por qué `exit(kill)` parece `killed` cuando se recibe de otro proceso vinculado (la señal se modifica para que no se propague en cascada), pero sigue pareciendo `kill` cuando se intercepta localmente.
Si todo esto le parece confuso, no se preocupe. Muchos programadores piensan lo mismo. Las señales de salida son un poco extrañas. Por suerte, no hay otros casos especiales aparte de los descritos aquí. Una vez que los comprendas, podrás comprender la mayor parte de la gestión de errores concurrentes de **Erlang** sin problemas.

---

## Monitores!

Tal vez todo esto de matar procesos no es exactamente lo que buscas. Tal vez no sientes los deseos de llevarte contigo el resto de la humanidad cuando te vayas?
Tal vez eres un poco mas un `S.T.A.L.K.E.R` (Atrapado? hehe...okno)
Bueno, en ese caso y en cualquier caso,  los `monitors` o monitores sea lo que necesitas. Estos resulta que son un tipo bien especifico de los `links`, solo se diferencian en dos grandes cosas:

- Son unidireccionales.
- Puedes tener unos cuantos entre dos procesos (Se amontonan y tienen identificadores)

Los monitores son útiles cuando un proceso desea saber qué sucede con otro proceso, pero ninguno de ellos es realmente vital para el otro. También son útiles para apilar referencias identificables individualmente. Esto puede parecer inútil al principio, pero es excelente para escribir bibliotecas que necesitan saber qué sucede con otros procesos. ¿Por qué los enlaces no son apropiados para esto? Dado que los enlaces no se apilan, una biblioteca que configura un enlace y luego lo elimina podría estar manipulando enlaces importantes no relacionados. Los monitores (y el apilamiento) permiten a los programadores de bibliotecas separar el uso de la monitorización de otros no relacionados. Dado que cada monitor tiene una identidad única, es posible elegir cuál escuchar o manipular.
Los enlaces son más una estructura organizativa que los monitores. Al diseñar la arquitectura de la aplicación, se determina qué proceso realizará qué tareas y qué dependerá de qué. Algunos procesos supervisarán a otros, otros no podrían funcionar sin un proceso gemelo, y así sucesivamente. Esta estructura suele ser algo fijo y conocido de antemano. Los enlaces son útiles en este caso, pero no necesariamente deben usarse fuera de él.
Pero, ¿qué sucede si tienes dos o tres bibliotecas diferentes a las que llamas y todas necesitan saber si un proceso está activo? Si usaras enlaces para esto, rápidamente te encontrarías con un problema al necesitar desvincular un proceso. Los enlaces no son apilables, así que al desvincular uno, los desvinculas a todos y arruinas las suposiciones hechas por las demás bibliotecas. Por lo tanto, necesitas enlaces apilables, y los monitores son la solución, ya que pueden eliminarse individualmente. Además, ser unidireccionales es útil en las bibliotecas porque otros procesos no deberían necesitar estar al tanto de ellas.
Entonces, ¿cómo es un monitor? Para verlo, configuremos uno. La función es `erlang:monitor/2`, donde el primer argumento siempre es el proceso atom y el segundo es el PID:

---

```erlang
1> erlang:monitor(process, spawn(fun() -> timer:sleep(500) end)).
#Ref<0.0.0.77>
2> flush().
Shell got {'DOWN',#Ref<0.0.0.77>,process,<0.63.0>,normal}
ok
```

---

Cada vez que un proceso que monitorizas se cae, recibirás un mensaje con el formato `{'DOWN', MonitorReference, process, Pid, Reason}`. La referencia te permite monitorizar el proceso. Recuerda que los monitores son apilables, por lo que es posible desactivar más de uno. Las referencias te permiten rastrear cada uno de forma única. También ten en cuenta que, al igual que con los enlaces, existe una función atómica para generar un proceso mientras lo monitorizas:
`spawn_monitor/1-3`:

---

```erlang
3> {Pid, Ref} = spawn_monitor(fun() -> receive _ -> exit(boom) end end).
{<0.73.0>,#Ref<0.0.0.100>}
4> erlang:demonitor(Ref).
true
5> Pid ! die.
die
6> flush().
ok
```

---

En este caso, demostramos el otro proceso antes de que fallara, por lo que no tuvimos rastro de su muerte. La función demonitor/2 también existe y proporciona más información. El segundo parámetro puede ser una lista de opciones. Solo existen dos: info y flush.

---

```erlang
7> f().
ok
8> {Pid, Ref} = spawn_monitor(fun() -> receive _ -> exit(boom) end end).
{<0.35.0>,#Ref<0.0.0.35>}
9> Pid ! die.
die
10> erlang:demonitor(Ref, [flush, info]).
false
11> flush().
ok
```

---

La opción `info` indica si existía un monitor al intentar eliminarlo. Por eso, la línea 10 devolvió `false`. Usar la opción `flush` elimina el mensaje `DOWN` del buzón, si existía, lo que hace que `flush()` no encuentre nada en el buzón del proceso actual.

---
### Nombrando los Procesos

Con los enlaces y monitores cubiertos, queda otro problema por resolver: ¿Qué hacemos cuando detectamos que un proceso del que dependemos ha finalizado?
Usemos las siguientes funciones del módulo `linkmon.erl`:

---

```erlang
start_critic() ->
	spawn(?MODULE, critic, []).
	
judge(Pid, Band, Album) ->
	Pid ! {self(), {Band, Album}},
	receive
		{Pid, Criticism} -> Criticism
		after 2000 ->
			timeout
	end.
	
critic() ->
	receive
		{From, {"Rage Against the Turing Machine", "Unit Testify"}} ->
			From ! {self(), "They are great!"};
		{From, {"System of a Downtime", "Memoize"}} ->
			From ! {self(), "They're not Johnny Crash but they're good."};
		{From, {"Johnny Crash", "The Token Ring of Fire"}} ->
			From ! {self(), "Simply incredible."};
		{From, {_Band, _Album}} ->
			From ! {self(), "They are terrible!"}
	end,
	critic().
```

---

Ahora fingiremos que vamos de compras por las tiendas, comprando música(O bajándotela pirata de Telegram).
Hay algunos álbumes que suenan interesantes, pero nunca estamos del todo seguros.
Decidimos llamar a nuestro amigo, el **crítico**.

---

```erlang
1> c(linkmon).
{ok,linkmon}
2> Critic = linkmon:start_critic().
<0.47.0>
3> linkmon:judge(Critic, "Genesis", "The Lambda Lies Down on Broadway").
"They are terrible!"
```

---

Debido a una tormenta solar o algo así (estoy intentando encontrar algo realista aquí),
la conexión se interrumpe... :v

---

```erlang
4> exit(Critic, solar_storm).
true
5> linkmon:judge(Critic, "Genesis", "A trick of the Tail Recursion").
timeout
```

---

Esto es molesto. Ya no podemos recibir críticas de los álbumes. Para mantener viva la crítica, crearemos un proceso básico de `supervisión` cuya única función es reiniciar la crítica cuando se caiga.

---

```erlang
start_critic2() ->
	spawn(?MODULE, restarter, []).
	
restarter() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(?MODULE, critic, []),
	receive
		{'EXIT', Pid, normal} -> % not a crash
			ok;
		{'EXIT', Pid, shutdown} -> % manual termination, not a crash
			ok;
		{'EXIT', Pid, _} ->
			restarter()
	end.
```

---

Aquí, el *reiniciador* será su propio proceso. A su vez, iniciará el proceso del *crítico* y, si este muere por una causa anormal, `restarter/0` entrará en bucle y creará un nuevo crítico. Tenga en cuenta que añadimos una cláusula para `{'EXIT', Pid, shutdown}` como una forma de eliminar manualmente el crítico si fuera necesario.
El problema con nuestro enfoque es que no hay forma de encontrar el `PID` del *crítico* y, por lo tanto, no podemos llamarlo para obtener su opinión. Una de las soluciones que ofrece **Erlang** es asignar nombres a los procesos. Dar un nombre a un proceso permite reemplazar el `PID` impredecible por un átomo. Este átomo puede usarse exactamente como un `PID` al enviar mensajes.
Para asignar un nombre a un proceso, utilice la función `erlang:register(Name,Pid)`. Si el proceso muere, perderá automáticamente su nombre. Como alternativa, puede usar un `register/1` para hacerlo manualmente. Puedes obtener una lista de todos los procesos registrados con `registered/0`, o una más detallada con el comando de `shell` :  `regs()`. Podemos reescribir la función `restarter/0` de la siguiente manera:

---

```erlang
restarter() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(?MODULE, critic, []),
	register(critic, Pid),
	receive
		{'EXIT', Pid, normal} -> % not a crash
			ok;
		{'EXIT', Pid, shutdown} -> % manual termination, not a crash
			ok;
		{'EXIT', Pid, _} ->
			restarter()
	end.
```

---

Como pueden ver, el `registro/2` siempre asignará a nuestro *crítico* el nombre *crítico*, sin importar cuál sea el `PID`. Por lo tanto, necesitamos eliminar la necesidad de pasar un `PID` desde las funciones de abstracción. Probemos esto:

---

```erlang
judge2(Band, Album) ->
	critic ! {self(), {Band, Album}},
	Pid = whereis(critic),
	receive
		{Pid, Criticism} -> Criticism
		after 2000 ->
			timeout
	end.
```

---

Aquí, la línea `Pid = whereis(critic)` se usa para encontrar el `PID` del *crítico* y así realizar una coincidencia de patrones con él en la expresión de recepción. Queremos coincidir con este `PID` porque garantiza que coincidamos con el mensaje correcto. (¡Podría haber **500** en el buzón ahora mismo!). Sin embargo, esto puede ser la fuente de un problema. Este código asume que el `PID` del crítico se mantendrá igual entre las dos primeras líneas de la función. Sin embargo, es completamente plausible que ocurra lo siguiente:

---

```erlang
1. critic ! Message
2. critic receives
3. critic replies
4. critic dies
5. whereis fails
6. critic is restarted
7. code crashes
```

---

Esto también es una posibilidad:

---

```erlang
1. critic ! Message
2. critic receives
3. critic replies
4. critic dies
5. critic is restarted
6. whereis picks up
wrong pid
7. message never matches
```

---

Las cosas podrían salir mal en un proceso diferente y causar problemas en otro si no hacemos las cosas correctamente. En este caso, el valor del átomo *crítico* puede verse en múltiples procesos. Esto se conoce como estado compartido. El problema radica en que diferentes procesos pueden acceder al valor del crítico y modificarlo prácticamente al mismo tiempo, lo que resulta en información inconsistente y errores de software. El término común para este tipo de situaciones es condición de carrera. Las condiciones de carrera son particularmente peligrosas porque dependen de la temporización de los eventos. En prácticamente todos los lenguajes concurrentes y paralelos, esta temporización depende de factores impredecibles, como la ocupación del procesador, adónde van los procesos y qué datos procesa el programa.

>[!Que no se te vaya la mano con el Zuko!]
>Quizás hayas oído que **Erlang** suele estar libre de condiciones de carrera o *interbloqueos*, lo que hace que el código paralelo sea seguro. Esto es cierto en muchas circunstancias, pero solo porque el paso de mensajes a través de un buzón fuerza cierta ordenación de eventos y porque el lenguaje restringe seriamente la cantidad de estado compartido que se puede tener. En general, nunca debes asumir que tu código está completamente libre de condiciones de carrera.
  Los procesos con nombre son solo un ejemplo de las múltiples maneras en que el código paralelo puede fallar.
  Otros ejemplos incluyen el acceso a archivos en el ordenador (para modificarlos) y la actualización de los mismos registros de la base de datos desde muchos procesos diferentes.

Por suerte, es relativamente fácil corregir el código de ejemplo si no asumimos que el proceso nombrado permanece igual. En su lugar, usaremos referencias (creadas con `make_ref()`) como valores únicos para identificar los mensajes y asegurarnos de recibir los mensajes correctos del proceso correcto. Tendremos que reescribir la función `critic/0` en `critic2/0` y `judge/3` en `judge2/2`.

---

```erlang
judge2(Band, Album) ->
	Ref = make_ref(),
	critic ! {self(), Ref, {Band, Album}},
	receive
		{Ref, Criticism} -> Criticism
		after 2000 ->
			timeout
	end.
	
critic2() ->
	receive
		{From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
			From ! {Ref, "They are great!"};
		{From, Ref, {"System of a Downtime", "Memoize"}} ->
			From ! {Ref, "They're not Johnny Crash but they're good."};
		{From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
			From ! {Ref, "Simply incredible."};
		{From, Ref, {_Band, _Album}} ->
			From ! {Ref, "They are terrible!"}
	end,
	critic2().
```

---

Y luego cambia `restarter/0` para que se ajuste, haciendo que genere `critic2/0` en lugar de `critic/0`.
Ahora, las demás funciones deberían seguir funcionando correctamente y los usuarios no notarán la diferencia. Bueno, la notarán porque renombramos las funciones y modificamos el número de parámetros, pero no sabrán qué detalles de implementación se modificaron ni por qué fue importante. Solo verán que su código se simplificó y ya no necesitan enviar un `PID` en las llamadas a funciones.
Aquí tienes un ejemplo:

---

```erlang
6> c(linkmon).
{ok,linkmon}
7> linkmon:start_critic2().
<0.55.0>
8> linkmon:judge2("The Doors", "Light my Firewall").
"They are terrible!"
9> exit(whereis(critic), kill).
true
10> linkmon:judge2("Rage Against the Turing Machine", "Unit Testify").
"They are great!"
```

---

Y ahora, aunque eliminamos el *crítico*, uno nuevo regresó al instante para resolver nuestros problemas. Esa es la utilidad de los procesos con nombre. Si hubiéramos intentado llamar a `linkmon:judge/2` sin un proceso registrado, el operador `!` dentro de la función habría generado un error de argumento erróneo, lo que garantiza que los procesos que dependen de los procesos con nombre no puedan ejecutarse sin ellos.
En el capítulo 13, pondremos en práctica la programación concurrente con **Erlang** escribiendo una aplicación real.

>[!Nombra lo que vale la pena nombrar]
>Recuerde que los **átomos** se pueden usar en una cantidad limitada (aunque elevada). Nunca debe crear átomos dinámicos. Esto significa que los procesos con nombres deben reservarse para servicios importantes exclusivos de una instancia de la máquina virtual y para procesos que deben estar presentes durante toda la ejecución de la aplicación.
  Si necesita procesos con nombres, pero son transitorios o ninguno puede ser exclusivo de la máquina virtual, es posible que deba representarlos como un grupo.
  Vincularlos y reiniciarlos si fallan podría ser una opción más sensata, en lugar de intentar usar nombres dinámicos.