# El shell de Erlang

  

Lo más básico y lo que realmente importa para comenzar:

  

1. Debes [descargar Erlang/OTP](https://www-erlang-org.translate.goog/downloads?_x_tr_sl=en&_x_tr_tl=es&_x_tr_hl=es&_x_tr_pto=tc) e instalarlo en tu equipo — hazlo rápido.

  

2. Luego debes asegurarte de que **Erlang** está en tus variables de entorno dentro de tu equipo. Puede ocurrir que al instalarlo no se haya definido automáticamente, así que ten cuidado.

  

3. Escribe `erl` en el `PowerShell`, en el `cmd` o en la terminal que uses y listo: habrás abierto la consola de **Erlang** en esa ruta. Puedes hacer sumas y operaciones básicas por ahora. Con `halt().` sales o también pulsando `Ctrl+C`.

  

---

  

# Módulos y funciones

  

Para empezar, los archivos de módulos en **Erlang** deben tener la extensión `.erl`.  

El módulo usado debe tener el mismo nombre del archivo. En este ejemplo el módulo se llama `tut` y lo especificamos dentro del archivo escribiendo `-module(nombre_modulo)`.

```erlang
-module(tut).
-export([double/1]).

double(X) ->
    2 * X.
```
>Si te fijas `-export(nombre_funcion).` se encarga de especificar que funciones dentro del modulo van a ser globales, al exportarlas podremos ser capaces de usarlas desde otros módulos, por ejemplo. Ah! si andas vago, también existe la opción de escribir `-compile(export_all).`, pero cuidado, eso es muy mala idea en producción.

En todo caso, `double/1` devuelve claramente el número multiplicado x 2.
Para compilar y ejecutar ve a tu consola de **Erlang** y escribe:
  
```powershell
3> c(tut).
{ok,tut}
```
>Con `c(nombre_del_modulo).` compila el módulo y si devuelve `{ok,tut}` todo salió bien

Para ejecutar la función:

```shell
4> tut:double(10).
20
``` 

***Notas importantes***:

- Cada archivo de Erlang suele empezar con el nombre del módulo: `-module(nombre).` (cada línea termina en `.`).

- Para llamar una función desde otro módulo: `module_name:function_name(arguments)`.

- `-export([double/1]).` indica que `double` es pública y tiene 1 argumento (aridad `/1`).

*Ejemplo*: factorial (archivo `tut1.erl`):  

```erlang
-module(tut1).
-export([fac/1]).

fac(1) ->
    1;
fac(N) ->
    N * fac(N - 1).

```

- La primera cláusula `fac(1) -> 1.` maneja el caso base.

- La segunda cláusula es recursiva `fac(N) -> N * fac(N - 1).`.

- Observa los `;` entre cláusulas y `.` al final de la función.

**Descomposición del cálculo de `fac(4)`**:  

```erlang
fac(4) ->

    4 * fac(3) ->

      4 * (3 * fac(2)) ->

      4 * (3 * (2 * fac(1))) ->

      4 * 3 * 2 * 1 = 24

```
>Porque **SI** mi querido amigo que le teme a la *recursividad*, en **Erlang** la mayoría de las cosas son recursivas y se asemejan un poco también a cierto lenguaje del mundo de la IA, que si lo conoces un poco ya habrás identificado(Si, *Prolog*).

*Otro ejemplo con multiplicación de dos argumentos*:
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
>Identar aca es una candela, y en cualquier caso, debes estar atento a el tema de los `.`, `,`, `;`, `->`. Tal vez mas adelante hable a fondo de eso, por ahora confórmate con irlo cogiendo al paso.

*Por cierto*, las variables empiezan con letra mayúscula en **Erlang**.

---
# Atoms

Los ***átomos*** son nombres literales: empiezan con letra minúscula y representan símbolos, no valores asignados. Se suelen usar para los mensajes que veras mas adelante o para identificar con mas detalle datos recibidos.
  
*Ejemplo: conversión entre pulgadas y centímetros:*
```erlang
-module(tut2).
-export([convert/2]).

convert(M, inch) ->
    M / 2.54;
convert(N, centimeter) ->
    N * 2.54.
```

**Pruebas en shell**:
```shell
10> tut2:convert(3, inch).
1.1811023622047243
11> tut2:convert(7, centimeter).
17.78

```

Si pasas un **átomo** no contemplado:
```shell
12> tut2:convert(3, miles).
** exception error: no function clause matching tut2:convert(3,miles) (tut2.erl, line 4)
```

- Cada cláusula de `convert/2` es una clause identificada por su **átomo**.
  
---
# Tuplas

Erlang usa **tuplas** para agrupar valores; se denotan con `{}`. Son útiles para aclarar parámetros ambiguos.

**Ejemplo (archivo `tut3.erl`):**
```erlang
-module(tut3).
-export([convert_length/1]).

convert_length({centimeter, X}) ->
    {inch, X / 2.54};
convert_length({inch, Y}) ->
    {centimeter, Y * 2.54}.

```

Uso en shell:
```shell
14> c(tut3).
{ok,tut3}
15> tut3:convert_length({inch, 5}).
{centimeter,12.7}
16> tut3:convert_length(tut3:convert_length({inch, 5})).
{inch,5.0}

```

- Las tuplas pueden anidar otras tuplas:
```erlang
{moscow, {c, -10}}

{cape_town, {f, 70}}

{paris, {f, 28}}
```

  

---
# Listas
  

Las **listas** se representan con `[]` y pueden contener tuplas u otros elementos. Ejemplo de lista de temperaturas:

```erlang
[{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}},
 {paris, {f, 28}}, {london, {f, 36}}]
```

*En Erlang los strings son internamente listas de códigos Unicode (por ejemplo, `"abc"` es `[97,98,99]`):*
```shell
30> [97,98,99].
"abc"
```
>Para más detalle sobre listas y operaciones ver: https://devdocs.io/erlang~26/doc/getting_started/seq_prog

  

> **Notita:** Erlang no tiene un tipo `char` separado; el compilador muestra cadenas como `"..."` aunque internamente sean listas de enteros.

  
---
# Maps

Los **mapas** en Erlang son como diccionarios y se escriben con `#{}`:
```erlang
#{"key" => 42 }.

#{"key" => 42}
```

**Ejemplo avanzado: cálculo de *alpha blending* con maps para colores**:
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
        red   := red(Src,Dst) / Alpha,
        green := green(Src,Dst) / Alpha,
        blue  := blue(Src,Dst) / Alpha,
        alpha := Alpha
    };
blend(_,Dst,_) ->
    Dst#{
        red   := 0.0,
        green := 0.0,
        blue  := 0.0,
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
>Aca lo que se hace es mezclar colores porsi no lo viste al principio(No debes entenderlo todo perfectamente ahora mismo, no te preocupes c:)

Puntos clave:

- Se define el macro `is_channel` para validar que R,G,B,A son floats entre `0.0` y `1.0`.

- Se usan guards (`when`) para validar parámetros antes de ejecutar la función.

- `Map#{} ` y `Map#{ key := value }` permiten actualizar/leer campos de maps.


---
# Standard Modules

Erlang trae módulos estándar para cosas comunes, por ejemplo `io` para entrada/salida. Usa `erl -man` en el shell para ver la documentación de los módulos.

**Ejemplos de `io:format`:**  

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
- `~w` se reemplaza por el elemento pasado en la lista de argumentos.
- `~n` es salto de línea.  

---
# Comments

Comentarios con `%%`. Simple y directo.
  

---
# Matching, Guards, and Scope of Variables

Ejemplo: encontrar máximo en una lista (`tut6.erl`):

  

```erlang

-module(tut6).

-export([list_max/1]).

  

list_max([Head|Rest]) ->

   list_max(Rest, Head).

  

list_max([], Res) ->

    Res;

  

list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->

    list_max(Rest, Head);

  

list_max([Head|Rest], Result_so_far)  ->

    list_max(Rest, Result_so_far).

```

  

En el shell:

  

```powershell

37> c(tut6).

{ok,tut6}

38> tut6:list_max([1,2,3,4,5,7,4,3,2,1]).

7

```

  

Explicación y puntos clave:

  

- `list_max/1` asume el primer elemento como máximo inicial y llama a `list_max/2`.

- `list_max/2` usa guards (`when`) para comparar y actualizar el resultado parcial.

- Funciones con la misma nombre pero distinta aridad son diferentes (`list_max/1` vs `list_max/2`).

- Los guards se prueban en orden; si uno falla, se intenta la siguiente cláusula.

- Las variables son inmutables: una vez asignadas no puedes reasignarlas en el mismo scope.

  

Ejemplo de coincidencia de patrones:

  

```erlang

{X, Y} = {paris, {f, 28}}.

% X = paris, Y = {f, 28}

```

  

Intentar reasignar una variable provoca error.

  

Para mayor legibilidad puedes usar variables intermedias:

  

```erlang

list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->

    New_result_far = Head,

    list_max(Rest, New_result_far);

```

  

---

  

# Más sobre las listas

  

Ejemplo: reverse (`tut8.erl`):

  

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

  

En shell:

  

```bash

52> c(tut8).

{ok,tut8}

53> tut8:reverse([1,2,3]).

[3,2,1]

```

  

Traza conceptual de la ejecución:

  

```

reverse([1,2,3], []) =>

    reverse([2,3], [1]) =>

    reverse([3], [2,1]) =>

    reverse([], [3,2,1]) =>

    [3,2,1]

```

  

---

  

# If y Case

  

## Estructura `if`

  

Evalúa condiciones en orden y ejecuta la primera verdadera.

  

```erlang

if

    Condición1 ->

        Acción1;

    Condición2 ->

        Acción2;

    true ->  % Opcional: default si ninguna coincide

        Acción_default

end

```

  

Ejemplo:

  

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

  

Salidas esperadas:

  

- `tut9:test_if(5,33).` → imprime `A == 5`

- `tut9:test_if(33,6).` → imprime `B == 6`

- `tut9:test_if(2,3).` → imprime `A == 2, B == 3`

- `tut9:test_if(33,33).` → error: ninguna condición coincide

  

## Estructura `case`

  

Compara un valor contra múltiples patrones:

  

```erlang

case Valor of

    Patrón1 -> Acción1;

    Patrón2 -> Acción2

end

```

  

Ejemplo (conversión de unidades):

  

```erlang

-module(tut10).

-export([convert_length/1]).

  

convert_length(Length) ->

    case Length of

        {centimeter, X} -> {inch, X / 2.54};

        {inch, Y} -> {centimeter, Y * 2.54}

    end.

```

  

Combinando `if` y `case` con guards:

  

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

  

Notas clave:

  

1. `if` evalúa condiciones en orden hasta encontrar una verdadera.  

2. `case` compara un valor contra múltiples patrones.  

3. Ambas estructuras retornan valores.  

4. Puedes usar guards (`when`) en ambas.  

5. Si no hay coincidencias se produce un error en tiempo de ejecución.

  

---

  

# Demos un respiro

  

Recupera el aliento por un rato, tómate una cerveza y luego de pasearte por esta sección, hacer las tareas y releerte un poco todo, puedes ir al siguiente capítulo:

  

[Programacion Concurrente](../II%20-%20Programacion%20Concurrente)