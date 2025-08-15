## I - Programacion Secuencial Basica

1. ***El shell de Erlang***
   
   -Solo escribe "erl" en el PowerShell y listo. Puedes hacer sumas y cosas.
   -Con `halt()` puedes salir directamente
   
2. ***Modulos y funciones***
 
	-Los archivos tienen la ext  ".erl".
	-El modulo usado debe tener el mismo nombre del archivo, en este caso le ponen "tut" y asi quedaria:
	```erlang
		-module(tut).
		-export([double/1]).
		
		double(X) ->
		    2 * X.
	```
	-Esta claro que lo de arriba devuelve un numero multiplicado por 2. Ahora, para correr un codigo:
	```shell
		3> c(tut).
		{ok,tut}
	```
	-Donde c(el nombre del modulo o archivo a ejecutar) y "ok, tut" vendria siendo que todo salio ok! Eso lo compila, para en este caso ejecutar la funcion. Abre el shell y:
	```shell
		4> tut:double(10).
		20
	```
	-Cada archivo de erlang debe contener modulos erlang, la primer linea por lo general va a ser el nombre del modulo que representara a ese archivo, en este caso `-module(tut).` (Al final de cada linea notese el '.')
	-Cuando usas la funcion en otro modulo, la sintaxis vendria siendo `module_name:function_name(arguments)` (Como fue el caso de encima cuando llamo a la funcion)
	-En la segunda linea del archivo anterior tenemos que `-export([double/1]).` lo cual significa que en el modulo mencionado encima vamos a tener un metodo llamado `double` que tomara un solo argumento `/1`  . Ademas especifica con el export que la funcion no sera privada del modulo, sino que podra llamarse desde fuera.
	
	A continuacion se presenta otro ejemplo en el cual se haya el factorial de un argumento(Si es 4, sera 4x3x2x1 = 24)(modulo y archivo tut1.erl):
	```erlang
	-module(tut1).
	-export([fac/1]).
	
	fac(1) ->
	    1;
	fac(N) ->
	    N * fac(N - 1).
	```
	-La primera `fac(1)`, dice que el factorial de 1 sera = 1. Notese que acaba en `;` esta vez.
	-La segunda `fac(N)`, dice que el factorial de un numero N sera ese numero x la factorial de N - 1. En este caso si que acaba en `.`
	-Esto lo veo como una pequeña recursividad:
	```erlang
		fac(4) -> (=24)
			4 x fac(3) -> (=4x6)
				3 x fac(2) -> (=3x2)
					2x fac(1) -> (=2x1)
					 1;
		.
	```
	
	Ahora veamos esa misma pero con otra funcion exportable que tiene dos argumentos, como sera la multiplicacion de dos numeros:
	```erlang
	-module(tut1).
	-export([fac/1, mult/2]).
	
	fac(1) ->
	    1;
	fac(N) ->
	    N * fac(N - 1).
	
	mult(X, Y) ->
	    X * Y.
	```
	-Notese que las variables se deben declarar con una letra mayuscula al principio.
3. ***Atoms***
   
	Los *Atoms* son otro tipo de datos en erlang, estos en deferencia de las variables comienzan con una letra minuscula y son solo nombres, nada se les asigna, nada mas, solo nombres.
	A continuacion un codigo util para convertir de inch a cm:
	```erlang
	-module(tut2).
	-export([convert/2]).
	
	convert(M, inch) ->
	    M / 2.54;
	
	convert(N, centimeter) ->
	    N * 2.54.
	```
	Aca algunas pruebas:
	```shell
	10> tut2:convert(3, inch).
	1.1811023622047243
	11> tut2:convert(7, centimeter).
	17.78
	```
	Si se trata de ingresar otro atomo diferente a los que estan escritos:
	```shell
	12> tut2:convert(3, miles).
	** exception error: no function clause matching tut2:convert(3,miles) (tut2.erl, line 4)
	```
	-Las dos partes de la funcion convert() son llamadas clausulas, cada una se identifica por su atomo.
4. ***Tuplas***

	 Para hacer las cosas mas entendibles y separarlas mejor ***erlang*** usa las ***tuplas***, y se denotan con `{}` 
	 Entonces en el caso de la funcion para convertir y tal tenemos el problema de que es algo confuso el tema de los parametros que recibe, `(3, inches)` por ejemplo significaria que convertira *3 cms a inch o 3 inch a cms*. Para aclararlo lo separaremos en ***tuplas***:
	```erlang
	-module(tut3).
	-export([convert_length/1]).
	
	convert_length({centimeter, X}) ->
	    {inch, X / 2.54};
	convert_length({inch, Y}) ->
	    {centimeter, Y * 2.54}.
	```
	-En este caso `{inch, 3}` denota 3 pies y `{centimeter, 3}` denota 3 cms.
	Vendria resultando algo asi:
	```shell
	14> c(tut3).
	{ok,tut3}
	15> tut3:convert_length({inch, 5}).
	{centimeter,12.7}
	16> tut3:convert_length(tut3:convert_length({inch, 5})).
	{inch,5.0}
	```
	En el caso 16 como es normal una funcion puede pasar por argumentos la ejecucion de otra que de como resultado una tupla valida para esta otra funcion, en este caso lo convierte a centimetros y en la linea 16 lo vuelve a dejar en inchs.
	Tambien decir que una tupla puede tener tuplas dentro y cuantas partes desee:
	```shell
	{moscow, {c, -10}}
	{cape_town, {f, 70}}
	{paris, {f, 28}}
	```
5. ***Listas***
   
	Aca las listas se representan como `[]` y en ellas se pueden guardar tuplas o los elementos que desees. Se puede hacer una lista de la temperatura de ciudades del mundo:
	```shell
	[{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}},
	 {paris, {f, 28}}, {london, {f, 36}}]
	```
	-Aca notese que no importa mucho lo de el salto de linea en codigo.
	A partir de aca el tuto habla mucha fana de las listas y de separarlas y noseque, que hueso, vease en https://devdocs.io/erlang~26/doc/getting_started/seq_prog
	>***Notita:***
	Aunque si que se menciona algo curioso al final del cap, Erlang no posee strings ni char, sin embargo el compilador de Erlang identifica los strings como listas de unicode(Esta talla que di en Arquitectura)En el siguiente caso se representa "abc" como `[97,98,99].`:
	```shell
	30> [97,98,99].
	"abc"
	```
6. ***Maps***
   
	Como en cualquier lenguaje un mapa es un 'set' de un *valor* con su '*key*', se encapsula en `#{}` y va mas o menos asi:
	```shell
	#{"key" => 42 }.
	#{"key" => 42}
	```
	Iendo al grano y a lo dificil, utilizando tallas interesantes:
	A continuacion un ejemplazo de muestra como calcular 'alpha blending' usando mapas para referenciar colores y 'alpha channels':
	```erlang
	-module(color).
	
	-export([new/4, blend/2]).
	
	-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).
	
	new(R,G,B,A) when ?is_channel(R), ?is_channel(G),
	                  ?is_channel(B), ?is_channel(A) ->
	    #{red => R, green => G, blue => B, alpha => A}.
	
	blend(Src,Dst) ->
	    blend(Src,Dst,alpha(Src,Dst)).
	
	blend(Src,Dst,Alpha) when Alpha > 0.0 ->
	    Dst#{
	        red   := red(Src,Dst) / Alpha,
	        green := green(Src,Dst) / Alpha,
	        blue  := blue(Src,Dst) / Alpha,
	        alpha := Alpha
	    };
	blend(_,Dst,_) ->
	    Dst#{
	        red   := 0.0,
	        green := 0.0,
	        blue  := 0.0,
	        alpha := 0.0
	    }.
	
	alpha(#{alpha := SA}, #{alpha := DA}) ->
	    SA + DA*(1.0 - SA).
	
	red(#{red := SV, alpha := SA}, #{red := DV, alpha := DA}) ->
	    SV*SA + DV*DA*(1.0 - SA).
	green(#{green := SV, alpha := SA}, #{green := DV, alpha := DA}) ->
	    SV*SA + DV*DA*(1.0 - SA).
	blue(#{blue := SV, alpha := SA}, #{blue := DV, alpha := DA}) ->
	    SV*SA + DV*DA*(1.0 - SA).
	```
	Algunas cosas interesantes de esto vendrian siendo:
		-El macro `is_channel` se define para garantizar que realmente esta recibiendo los 4 valores de R,G,B,A, no es obligatorio en la sintaxis pero garantiza reducir  desordenes de sintaxis o **"syntax cluttering"**.  Osea valida que reciba un float entre 0.0 y 1.0 como `is_channel`:
		`-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).`
		-En este caso, verifica que reciba los 4 parametros correctamente y luego asigna a en este caso si la llamas como "color" .
7. ***Standard Modules***
   
	Aca se habla de los modulos que tiene el lenguaje para ayudar a hacer cosas. Como el clasico `io` que tiene para hacer inputs/outputs. Info basica de cada modulo con comando `erl -man` en el shell
	Para hacer el clasico print seria:
	```shell
	31> io:format("hello world~n", []).
	hello world
	ok
	32> io:format("this outputs one Erlang term: ~w~n", [hello]).
	this outputs one Erlang term: hello
	ok
	33> io:format("this outputs two Erlang terms: ~w~w~n", [hello, world]).
	this outputs two Erlang terms: helloworld
	ok
	34> io:format("this outputs two Erlang terms: ~w ~w~n", [hello, world]).
	this outputs two Erlang terms: hello world
	ok
	```
	Aca tenemos que `~w` es reemplazado por el segundo argumento de los dos que se le pasan a `format`, uno es casi siempre una lista escrita entre "" y el otro es un elemento que desees outputear y por el que se sustituira `~w`.
	Tabien tenemos `~n` que es para salto de linea
	Vaya a [[I - 2.9 Larger example]] para un ejemplo completo de esto ultimo.
8. ***Comments***
   
	Con `%%`, si, asi de facil.

9. ***Matching, Guards, and Scope of Variables***
    
	Primero esta es una funcion para hayar el valor max y min en una lista:
	```erlang
		-module(tut6).
		-export([list_max/1]).
		
		list_max([Head|Rest]) ->
		   list_max(Rest, Head).
		
		list_max([], Res) ->
		    Res;
		list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
		    list_max(Rest, Head);
		list_max([Head|Rest], Result_so_far)  ->
		    list_max(Rest, Result_so_far).
	```
	En el Shell:
	```powershell
		37> c(tut6).
		{ok,tut6}
		38> tut6:list_max([1,2,3,4,5,7,4,3,2,1]).
		7
	```
	
	 **Definición de la función `list_max`**
    
    - `list_max/1`: Recibe una lista y asume que el primer elemento es el máximo. Llama a `list_max/2` con el resto de la lista y ese valor inicial.
        
    - `list_max/2`: Compara cada elemento con el máximo encontrado hasta el momento (`Result_so_far`). Si el nuevo elemento es mayor, lo actualiza y sigue recorriendo la lista.
        
	**Diferencias entre `list_max/1` y `list_max/2`**
    
    - En Erlang, funciones con el mismo nombre pero diferente número de argumentos son tratadas como funciones distintas (`list_max/1` y `list_max/2`).
        
	 **Uso de "Guardas" (`when`)**
    
    - Se usa `when` para aplicar una condición antes de ejecutar una parte del código. Ejemplo:
	```erlang
	    -list_max([Head|Rest], Result_so_far) when Head > Result_so_far -> 
	    list_max(Rest, Head);
	```
	- Si la condición no se cumple, Erlang intenta la siguiente cláusula de la función.
	
	**Modificar la función para encontrar el mínimo**
    
    - Solo se necesita cambiar `>` por `<` y renombrar la función a `list_min`.
        
	**Alcance de variables en Erlang**
    
    - En cada llamada recursiva de `list_max/2`, `Result_so_far` es una nueva variable en un nuevo contexto.
        
    - Erlang no permite reasignar valores a una variable en el mismo ámbito:
	```erlang
		M = 5. % Correcto
		M = 6. % Error: M ya tiene un valor asignado
	
	```
	- Se pueden usar coincidencias de patrones para extraer valores de estructuras de datos:
	```erlang
		{X, Y} = {paris, {f, 28}}.
		% X = paris, Y = {f, 28}
	```
	- Intentar reasignar una variable con otro valor causa un error.
	
	**Mejorar legibilidad del código**
	
	- Se pueden definir variables intermedias para hacer el código más claro:
	```erlang
		list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
	    New_result_far = Head,
	    list_max(Rest, New_result_far);
	
	```
10. ***Mas sobre las listas!***
    
	Ahora un ejemplo de como hacer reverse una lista:
	```erlang
		-module(tut8).
		-export([reverse/1]).
		reverse(List) ->
		    reverse(List, []).
		reverse([Head | Rest], Reversed_List) ->
		    reverse(Rest, [Head | Reversed_List]);
		reverse([], Reversed_List) ->
		    Reversed_List.
	```
	
	```bash
	52> c(tut8).
	{ok,tut8}
	53> tut8:reverse([1,2,3]).
	[3,2,1]
	```
	
	El orden de los metodos iria mas o menos asi:
	
	```erlang
		reverse([1|2,3], []) =>
		    reverse([2,3], [1|[]])
		
		reverse([2|3], [1]) =>
		    reverse([3], [2|[1])
		
		reverse([3|[]], [2,1]) =>
		    reverse([], [3|[2,1]])
		
		reverse([], [3,2,1]) =>
		    [3,2,1]
	```
11. ***If and Case***
    
	Aquí tienes la traducción resumida y lista para copiar en Obsidian, conservando los  códigos de ejemplo y explicaciones clave:
	```erlang
	# Estructuras de control en Erlang: `if` y `case`
	
	## La función `if`
	
	Evalúa condiciones en orden y ejecuta la primera acción cuya condición sea verdadera.
	
	```erlang
	if
	    Condición 1 ->
	        Acción 1;
	    Condición 2 ->
	        Acción 2;
	    true ->  % Opcional: se ejecuta si ninguna condición anterior es verdadera
	        Acción_default
	end
	```
	
	**Ejemplo:**
	```erlang
	-module(tut9).
	-export([test_if/2]).
	
	test_if(A, B) ->
	    if 
	        A == 5 ->
	            io:format("A == 5~n", []),
	            a_equals_5;
	        B == 6 ->
	            io:format("B == 6~n", []),
	            b_equals_6;
	        A == 2, B == 3 ->
	            io:format("A == 2, B == 3~n", []),
	            a_equals_2_b_equals_3;
	        A == 1 ; B == 7 ->
	            io:format("A == 1 ; B == 7~n", []),
	            a_equals_1_or_b_equals_7
	    end.
	```
	
	**Salidas:**
	```erlang
	tut9:test_if(5,33).    % A == 5
	tut9:test_if(33,6).    % B == 6
	tut9:test_if(2,3).     % A == 2, B == 3
	tut9:test_if(33,33).   % Error: ninguna condición coincide
	```
	
	## La estructura `case`
	
	Evalúa un valor contra varios patrones:
	
	```erlang
	case Valor of
	    Patrón1 -> Acción1;
	    Patrón2 -> Acción2
	end
	```
	
	**Ejemplo (conversión de unidades):**
	```erlang
	-module(tut10).
	-export([convert_length/1]).
	
	convert_length(Length) ->
	    case Length of
	        {centimeter, X} -> {inch, X / 2.54};
	        {inch, Y} -> {centimeter, Y * 2.54}
	    end.
	```
	
	**Combinando `if` y `case` con guards:**
	```erlang
	-module(tut11).
	-export([month_length/2]).
	
	month_length(Year, Month) ->
	    Leap = if
	        trunc(Year / 400) * 400 == Year -> leap;
	        trunc(Year / 100) * 100 == Year -> not_leap;
	        trunc(Year / 4) * 4 == Year -> leap;
	        true -> not_leap
	    end,  
	    case Month of
	        feb when Leap == leap -> 29;
	        feb -> 28;
	        apr -> 30;
	        jun -> 30;
	        % ... otros meses
	    end.
	```
	
	**Notas clave:**
	1. `if` evalúa condiciones en orden hasta encontrar una verdadera
	2. `case` compara un valor contra múltiples patrones
	3. Ambas estructuras retornan valores
	4. Se pueden usar guards en ambas (`when`)
	5. Si no hay coincidencias, se produce un error en tiempo de ejecución

---
## Demos un respiro:
*Recupera el aliento por un rato, tomate una cerveza y luego de pasearte un rato por esta seccion, hacer las tareas y releerte un poco todo puedes ir al siguiente capitulo:*
[Programacion Concurrente](../II%20-%20Programacion%20Concurrente)
