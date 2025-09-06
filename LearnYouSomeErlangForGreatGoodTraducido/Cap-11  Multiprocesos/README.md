# Cap-11 Multiprocesos

---

## State your State(Mejor no lo traduzco)

Empecemos el capitulo creando un programa `kitchen.erl` que va a ser un modulo con la capacidad de por ahora, poder almacenar y extraer comida de un refrigerador. La siguiente función puede servir como la base de este proceso:

---

```erlang
-module(kitchen).
-compile(export_all).

fridge1() ->
	receive
		{From, {store, _Food}} ->
			From ! {self(), ok},
			fridge1();
		{From, {take, _Food}} ->
			%% uh....
			From ! {self(), not_found},
			fridge1();
		terminate ->
			ok
	end.
```

---
>Bueno, todo mal por aca, al guardar algo el proceso debe responder con su mejor `ok`, pero realmente no esta guardando nada en ninguna parte no? `fridge1()` es llamada y luego la función empieza de la nada sin un estado inicial. Ademas al tratar de extraer comida tampoco hay una actualización de estado porque para empezar ni siquiera hay un estado que actualizar.

Bueno! Ya quedo bastante claro que para sacar y meter comida necesitaremos agregarle un estado a nuestra función `fridge1()`. Y con la ayuda de la recursividad el estado de un proceso puede ser llevado enteramente en los parámetros de la función en cuestión. En el caso de este refrigerador una solución puede ser simplemente almacenar toda la comida en una **lista**:

---

```erlang
fridge2(FoodList) ->
	receive
		{From, {store, Food}} ->
			From ! {self(), ok},
			fridge2([Food|FoodList]);
		{From, {take, Food}} ->
			case lists:member(Food, FoodList) of
				true ->
					From ! {self(), {ok, Food}},
					fridge2(lists:delete(Food, FoodList));
				false ->
					From ! {self(), not_found},
					fridge2(FoodList)
			end;
		terminate ->
			ok
	end.
```

---
>Fijémonos en que `fridge2/1` recibe un argumento, `FoodList`. Puedes ver que cuando se envía el mensaje `{From, {store, Food}}`, la función añadirá esa comida a la lista antes de volverse a llamar recursivamente.

Bueno, en todo caso la función usa `list:member/2` para verificar que comidas son parte de la lista. En dependencia del resultado la comida es devuelta al proceso que llamo y removida de la lista o `not_found` es devuelto en caso de que no se encuentre:

---

```powershell
1> c(kitchen).
{ok,kitchen}
2> Pid = spawn(kitchen, fridge2, [[baking_soda]]).
<0.51.0>
3> Pid ! {self(), {store, milk}}.
{<0.33.0>,{store,milk}}
4> flush().
Shell got {<0.51.0>,ok}
ok
```

---

Guardar alimentos en el refrigerador parece funcionar. Ahora intentemos guardar algo más y luego sacarlo del refrigerador:

---

```powershell
5> Pid ! {self(), {store, bacon}}.
{<0.33.0>,{store,bacon}}
6> Pid ! {self(), {take, bacon}}.
{<0.33.0>,{take,bacon}}
7> Pid ! {self(), {take, turkey}}.
{<0.33.0>,{take,turkey}}
8> flush().
Shell got {<0.51.0>,ok}
Shell got {<0.51.0>,{ok,bacon}}
Shell got {<0.51.0>,not_found}
ok
```

---

Como era de esperar, podemos sacar el tocino del refrigerador porque lo pusimos primero (junto con la leche y el bicarbonato), pero el proceso del refrigerador no encuentra el pavo cuando lo pedimos. Por eso recibimos el último mensaje {<0.51.0>,not_found}. Más interesante aún, debido al funcionamiento de los buzones, está garantizado que incluso si mil personas de repente buscaran el último trozo de pavo del refrigerador al mismo tiempo, solo una de ellas podría conseguirlo.

---
## Amamos los mensajes ah? Bien, ocultemos nuestro tesoro c;

Algo molesto sobre los ejemplos anteriores es que el programador que va a sacar las cosas del refrigerador debe conocer el protocolo que a sido diseñado para tal proceso. Eso es una carga poco util. Una buena forma de solventar es abstraer los mensajes con la ayuda de funciones que manejen el envió y la recepción de mensajes:

---

```erlang
store(Pid, Food) ->
	Pid ! {self(), {store, Food}},
	receive
		{Pid, Msg} -> Msg
	end.
	
take(Pid, Food) ->
	Pid ! {self(), {take, Food}},
	receive
		{Pid, Msg} -> Msg
	end.
```

---
>Ahora la interacción con el proceso es muuucho mas clara no?

*Probemos esto!*

---

```erlang
9> c(kitchen).
{ok,kitchen}
10> f().
ok
11> Pid = spawn(kitchen, fridge2, [[baking_soda]]).
<0.73.0>
12> kitchen:store(Pid, water).
ok
13> kitchen:take(Pid, water).
{ok,water}
14> kitchen:take(Pid, juice).
not_found
```

---

Ya no tenemos que preocuparnos de como los mensajes funcionan internamente nunca mas! Si deseas enviar el `self()` o un especifico átomo como `store` o `take` solo necesitaras la `PID` y saber a que función debes llamar.  Esto esconde todo el trabajo sucio y hace mucho mas fácil construir dentro del proceso `refrigerador`.

Ahora también es el momento de que ocultemos la necesidad de generar un proceso. Ya trabajamos con lo de ocultar mensajes, pero aun seguimos dandole la carga al usuario de que cree un proceso. Añadiendo la funcion `start/1` de la siguiente forma se soluciona:

---

```erlang
start(FoodList) ->
	spawn(?MODULE, fridge2, [FoodList]).
```

---
>Fácil ah? Mejor ve considerando que esta es la forma sencilla amiguito.

Aquí, `?Module` es un ***macro*** que devuelve el nombre del modulo en que se encuentra el proceso que se va a generar. En principios no parece haber una gran ventaja al escribir esa función, pero de hecho si que la hay. La esencial ventaja es tener una consistencia con las llamadas a `take/2` y `store/2`. Gracias a esto todo lo relacionado con el proceso del refrigerador es ahora manejado por el modulo `kitchen`. Si queremos agregar `logging` cuando el proceso `fridge` es iniciado o iniciar un segundo proceso(Como por ejemplo, una nevera), eso seria muuuucho mas fácil de  hacer gracias a `start/1`. Sin embargo, si se deja que el usuario realice la generación a través de spawn/3, entonces cada lugar que inicie un refrigerador deberá agregar las nuevas llamadas. Esto es propenso a errores, y los errores son un fastidio. A que si?

Veamos esto en funcionamiento!

---

```erlang
15> f().
ok
16> c(kitchen).
{ok,kitchen}
17> Pid = kitchen:start([rhubarb, dog, hotdog]).
<0.84.0>
18> kitchen:take(Pid, dog).
{ok,dog}
19> kitchen:take(Pid, dog).
not_found
```

---

Yayyyy! el perrito(La salchicha claramente c;) a sido retirada del refrigerador para ser cocinado y nuestra abstracción es completa! 

---

## Time Out!

Hagamos una pequeña prueba con la ayuda del comando `pid(A,B,C)`, el cual nos permitira cambiar los tres enteros A, B y C en una PID. Aqui, deliberadamente suministraremos a `kitchen:take/2` una PID falsa:

---

```erlang
20> kitchen:take(pid(0,250,0), dog).
```

---
>Uy, tremendo problema este, el `shell` de ***Erlang*** se a quedado congelado

Bueno, esto ocurre por como `take/2` esta implementada. Para entender lo que esta ocurriendo vamos a analizar en un caso normal:
1. Un mensaje para guardar comida es enviado por ti (El `shell`) hacia el proceso `fridge`.
2. Tu proceso cambia cambia al *modo **recibir*** y espera mensajes nuevos.
3. El refrigerador guarda la comida y envía `ok` a tu proceso(`shell`).
4. Tu proceso(`shell`) lo recibe y sigue con su vida muy tranquilo.

Aquí esta lo que ocurre cuando se te *freezea* el `shell`:
1. Un mensaje para guardar comida es enviado por ti (El `shell`) hacia un proceso desconocido.
2. Tu proceso cambia cambia al *modo **recibir*** y espera mensajes nuevos.
3. El proceso desconocido o bien no existe siquiera, o bien no tiene definido esperar por tal mensaje. Entonces procede a no hacer nada al respecto.
4. Entonces tu proceso(`shell`) se queda atascado en lo del *modo **recibir***.

Esto es molesto, sobre todo porque no hay gestión de errores posible. No ha ocurrido nada ilegal; el programa simplemente espera eternamente, lo cual es un punto muerto. En general, cualquier proceso que se ocupe de operaciones *asíncronas* (como se realiza el paso de mensajes en **Erlang**) necesita una forma de abandonar el proceso después de cierto tiempo si no recibe señales de recibir datos. Un navegador web hace esto cuando una página o imagen tarda demasiado en cargar, y tú lo haces cuando alguien tarda demasiado en contestar el teléfono o tu profesor de Calculo demora demasiado en llegar a dar una conferencia. **Erlang** ciertamente tiene un mecanismo adecuado para gestionar los tiempos de espera, y forma parte de la construcción de recepción:

---

```erlang
receive
	Match -> Expression1
after Delay ->
	Expression2
end.
```

---
>Bien! Realmente si esta manejado, y ya sabíamos de esto. Lo malo es que debes manejarlo tu mismito.

La parte entre `receive` y `after` es exactamente la misma que has visto hasta ahora. La parte `after` se activará si el retardo (en milisegundos) ha transcurrido sin recibir un mensaje que coincida con el patrón `Match`.
Cuando esto sucede, se ejecuta la Expresión 2.
Escribiremos dos nuevas funciones de interfaz, `store2/2` y `take2/2`, que actuarán exactamente igual que `store/2` y `take/2`, excepto que dejarán de esperar después de tres segundos:

---

```erlang
store2(Pid, Food) ->
	Pid ! {self(), {store, Food}},
	receive
		{Pid, Msg} -> Msg
	after 3000 ->
		timeout
	end.
	
take2(Pid, Food) ->
	Pid ! {self(), {take, Food}},
	receive
		{Pid, Msg} -> Msg
	after 3000 ->
		timeout
	end.
```

---

>[!Notita]
>Se suele decir que `after` solo toma milisegundos como valor, pero en realidad es posible usar el átomo `infinity`. Si bien esto no es útil en muchos casos (podría ser mejor eliminar la cláusula `after` por completo), a veces se usa cuando el programador puede enviar el tiempo de espera a una función que espera un resultado. De esta manera, si tu quieres realmente esperar eternamente, puedes hacerlo.


Los temporizadores tienen otros usos además de simplemente dejar de funcionar después de un tiempo excesivo. Uno de ellos es la implementación de la función `timer:sleep/1`, que usamos en el Capítulo 10(Del libro). Así es como se implementa (incorporémosla en un nuevo módulo `multiproc.erl`):

---

```erlang
sleep(T) ->
	receive
		after T -> ok
	end.
```

---

En este caso específico, ningún mensaje coincidirá jamás en la parte de recepción de la construcción porque no existe un patrón. En su lugar, se llamará a la parte `after` de la construcción una vez transcurrido el retardo `T`.
Otro caso especial es cuando el tiempo de espera es 0:

---

```erlang
flush() ->
	receive
		_ -> flush()
		after 0 ->
			ok
	end.
```

---

Cuando esto sucede, la máquina virtual **Erlang** intentará encontrar un mensaje que se ajuste a uno de los patrones disponibles. En el caso anterior, cualquier coincidencia. Mientras haya mensajes, la función `flush/0` se llamará a sí misma recursivamente hasta que el buzón esté vacío. Después, se ejecuta la parte del código `after 0 -> ok` y la función retorna.

---

## `Receives` Selectivos! (Recepción Selectiva)

El concepto de "*vaciado*" o `flushing` de **Erlang** permite implementar una recepción selectiva, que puede priorizar los mensajes recibidos mediante la anidación de llamadas (Entendiste? Yo tampoco la primera vez tranquilo, vamos paso a paso):

---

```erlang
important() ->
	receive
		{Priority, Message} when Priority > 10 ->
			[Message | important()]
		after 0 ->
			normal()
	end.
	
normal() ->
	receive
		{_, Message} ->
			[Message | normal()]
	after 0 ->
		[]
	end.
```

---

Esta función creará una lista de todos los mensajes, colocando primero aquellos con una prioridad superior a **10**:

---

```erlang
1> c(multiproc).
{ok,multiproc}
2> self() ! {15, high}, self() ! {7, low}, self() ! {1, low}, self() ! {17, high}.
{17,high}
3> multiproc:important().
[high,high,low,low]
```

---

Debido a que usamos el ***bit posterior a 0*** o `after 0` bit, se obtendrán todos los mensajes hasta que no queden, pero el proceso intentará capturar todos aquellos con una prioridad superior a **10** antes incluso de considerar los demás mensajes, que se acumulan en la llamada `normal/0`. Esta práctica se denomina ***recepción selectiva***. Si le parece interesante, tenga en cuenta que a veces es insegura debido a la forma en que **Erlang** la gestiona.

---

## Los peligros de la recepción selectiva

Cuando se envían mensajes a un proceso, se almacenan en el buzón hasta que este los lee y coinciden con un patrón, incluso si el proceso que los envió originalmente ha finalizado. Los mensajes se almacenan en el orden en que se recibieron. Esto significa que cada vez que se introduce una recepción para que coincida con un mensaje, se escanea el buzón, comenzando por el primer mensaje recibido (el más antiguo). Ese mensaje más antiguo se compara con cada patrón de la recepción hasta que uno coincida. Cuando coincide, el mensaje se elimina del buzón y el código del proceso se ejecuta con normalidad hasta la siguiente recepción. Cuando se ejecuta esta siguiente recepción, la máquina virtual buscará el mensaje más antiguo que se encuentre en el buzón (el siguiente al que se eliminó), y así sucesivamente:

![alt](Media/Pasted%20image%2020250816233036.png)

Cuando no es posible encontrar un mensaje, este se guarda en una cola de guardado y se intenta con el siguiente. Si el segundo mensaje coincide, el primero se vuelve a colocar en la parte superior del buzón para intentarlo más tarde.

![alt](Media/Pasted%20image%2020250816233151.png)

Esto le permite centrarse únicamente en los mensajes útiles. Ignorar algunos mensajes para procesarlos posteriormente de la manera descrita es la esencia de las recepciones selectivas. Si bien son útiles, el problema con las recepciones selectivas es que si su proceso tiene muchos mensajes que no le interesan, leer los mensajes útiles tomará cada vez más tiempo (y los procesos también crecerán en tamaño).
En el último ejemplo de correspondencia de mensajes, imaginemos que queremos el mensaje 367, pero los primeros 366 mensajes son basura, ignorados por nuestro código. Para obtener el mensaje 367, el proceso debe intentar encontrar esos 366 mensajes basura. Una vez hecho esto, y todos los mensajes se han colocado en la cola, se extrae el mensaje 367 y los primeros 366 se vuelven a colocar en la parte superior del buzón. El siguiente mensaje útil podría estar mucho más oculto y tardar aún más en encontrarlo.
Este tipo de recepción es una causa frecuente de problemas de rendimiento en Erlang. Si tu aplicación se ejecuta con lentitud y sabes que hay muchos mensajes circulando, esta podría ser la causa. Si estas recepciones selectivas están causando una ralentización considerable en tu código, lo primero que debes hacer es preguntarte por qué recibes mensajes que no deseas. ¿Se envían los mensajes a los procesos correctos? ¿Son correctos los patrones? ¿Tienen un formato incorrecto? ¿Estás utilizando un proceso donde debería haber muchos? Responder a una o varias de estas preguntas podría resolver tu problema.

>[!Optimizaciones conversacionales]
>Desde la versión R14A, se ha añadido una nueva optimización al compilador de Erlang. Esta simplifica la recepción selectiva en casos muy específicos de comunicación entre procesos. Un ejemplo de esta función es la función `optimized/1` en `multiproc.erl`.
  Para que esta optimización funcione, se debe crear una referencia (ya sea mediante `make_ref()` o iniciando un monitor, como se describe en el Capítulo 12 del libro) en una función y luego enviarla en un mensaje. En la misma función, se realiza una recepción selectiva. Si ningún mensaje coincide a menos que contenga la misma referencia, el compilador se asegura automáticamente de que la máquina virtual omita los mensajes recibidos antes de la creación de dicha referencia.
  Tenga en cuenta que no debe forzar su código para que se ajuste a estas optimizaciones. Los desarrolladores de Erlang solo buscan patrones de uso frecuente y luego los agilizan. Si escribe código idiomático, las optimizaciones deberían venir a usted, no al revés.


---

## Más trampas en los buzones de correo

Debido al riesgo de que mensajes inútiles contaminen el buzón de un proceso, los programadores de Erlang a veces toman medidas defensivas contra tales eventos. Una defensa estándar podría ser la siguiente:

---

```erlang
receive
	Pattern1 -> Expression1;
	Pattern2 -> Expression2;
	Pattern3 -> Expression3;
		...
	PatternN -> ExpressionN;
	Unexpected ->
		io:format("unexpected message ~p~n", [Unexpected])
end.
```

---

Esto garantiza que cualquier mensaje coincida con al menos una cláusula. La variable `Unexpected` coincidirá con cualquier valor, eliminará el mensaje inesperado del buzón y mostrará una advertencia. Dependiendo de la aplicación, podría ser conveniente almacenar el mensaje en algún tipo de registro donde pueda encontrar información al respecto más adelante. Si los mensajes van al proceso equivocado, sería una pena perderlos definitivamente y sería difícil averiguar por qué ese otro proceso no recibe lo que debería, ya que es casi seguro que se trata de un error. En los casos en que necesite trabajar con una prioridad en sus mensajes y no pueda usar una cláusula `catchall`, una forma más inteligente de gestionarlos es implementar un `min-heap` (consulte https://secure.wikimedia.org/wikipedia/en/wiki/Min-heap) o usar el módulo `gb_trees` (discutido en el Capítulo 9 del libro) y volcar allí todos los mensajes recibidos (asegúrese de poner el número de prioridad primero en la clave para que se utilice para ordenar los mensajes). Después, puede buscar el elemento más pequeño o más grande en la estructura de datos según sus necesidades.
En la mayoría de los casos, esta técnica debería permitirle recibir mensajes con una prioridad de forma más eficiente que las recepciones selectivas. Sin embargo, podría ralentizar el proceso si la mayoría de los mensajes que recibe tienen la máxima prioridad posible. Como siempre, la clave está en perfilar y medir antes de optimizar.
Ahora que hemos visto cómo mantener el estado en los procesos, el siguiente paso es gestionar errores de forma eficiente con múltiples procesos, que es el tema del Capítulo 12. Vamos a allá!