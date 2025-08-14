# Las tareas!

---

## **Ejercicio 1: Evaluación de Expresiones**

Escribe funciones para:

- Calcular el área de un círculo dado su radio.
    
- Convertir temperaturas entre Celsius y Fahrenheit.
    

**Objetivo:** Practicar el uso de funciones y expresiones básicas.

```erlang
-module(practica1).
-export([area_circulo/1, c_to_f/1, f_to_c/1]).

area_circulo(R) -> math:pi() * R * R.
c_to_f(C) -> C * 9 / 5 + 32.
f_to_c(F) -> (F - 32) * 5 / 9.
```

---

## **Ejercicio 2: Recursión con listas**

Implementa funciones que:

- Calculen la suma de los elementos de una lista.
    
- Filtren los elementos mayores que un valor dado.
    

**Objetivo:** Aplicar recursión básica sobre listas.

---

## **Ejercicio 3: Uso de `case` y `if`**

Escribe una función que tome un número y devuelva:

- `"positivo"` si es mayor que 0
    
- `"negativo"` si es menor que 0
    
- `"cero"` si es igual a 0
    

---

## Para quien vaya un poco mas rapido: Introducción a la Concurrencia**

---

### **Ejercicio 4: Spawning de procesos**

Crea un módulo que tenga una función `start/0` que cree tres procesos, cada uno imprimiendo un mensaje diferente.

**Objetivo:** Usar `spawn` para lanzar procesos concurrentes.

```erlang
start() ->
    spawn(fun() -> io:format("Soy el proceso 1~n") end),
    spawn(fun() -> io:format("Soy el proceso 2~n") end),
    spawn(fun() -> io:format("Soy el proceso 3~n") end).
```

---

### **Ejercicio 5: Comunicación entre procesos**

Implementa un proceso que reciba mensajes del tipo `{ping, From}` y responda con `{pong, self()}`.

---

### **Ejercicio 6: Procesos con estado**

Crea un proceso contador que reciba mensajes `{add, N}` y mantenga su suma interna, devolviendo el total acumulado al recibir `{total, From}`.

---
