# Cap-12 Errores y Procesos

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

## It's a Trap!

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

## Viejas excepciones, Nuevos conceptos

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

Luego está `exit/2`. Este es el equivalente a un arma en el proceso **Erlang**. Permite que un proceso elimine a otro a distancia, de forma segura. A continuación, se presentan algunas de las posibles llamadas.

---

>**EN PROCESOOOOOOOOOOOOOOOOOOOOOO**

---