

# Ejercicios de práctica 

Estos ejercicios están diseñados para practicar **timeouts**, **selective receive**, **ocultamiento de mensajes** y **manejo de referencias** en procesos Erlang.

---

## Ejercicio 1 — Eco con retardo (nivel fácil)

**Descripción:**
Implementa un proceso que reciba mensajes en la forma `{echo, Text}`. El proceso debe responder al remitente con `Text`.
Si no recibe ningún mensaje en 2 segundos, debe enviar de vuelta `timeout`.

**Objetivos de práctica:**

* Manejo de `receive ... after`
* Comprensión de timeouts en procesos concurrentes

---

## Ejercicio 2 — Filtrado de mensajes secretos (nivel fácil)

**Descripción:**
Crea un proceso que reciba mensajes `{msg, Text}` y `{secret, Text}`.

* Si recibe `{msg, Text}`, debe responder con `"received: Text"`.
* Si recibe `{secret, Text}`, no debe responder nada y debe continuar ejecutándose.

**Objetivos de práctica:**

* Ignorar mensajes en el mailbox
* Observar cómo se acumulan mensajes no procesados

---

## Ejercicio 3 — Selective Receive con prioridad (nivel medio)

**Descripción:**
Diseña un proceso “buzón” que maneje dos tipos de mensajes:

* `{priority, Msg}` → deben procesarse siempre antes que cualquier otro.
* `{normal, Msg}` → solo se procesan si no hay pendientes prioritarios.

**Objetivos de práctica:**

* Uso de `receive` selectivo
* Implementación de prioridades en un mailbox
* Reflexión sobre los riesgos de rendimiento al usar selective receive con alto volumen de mensajes

---

## Ejercicio 4 — Cliente-servidor con referencias (nivel difícil)

**Descripción:**
Implementa un servidor genérico que reciba mensajes de clientes en la forma `{Pid, Ref, Request}`.

* El servidor procesa `Request` y responde con `{Ref, Response}`.
* Cada cliente genera su propio `Ref` usando `make_ref()`.
* El cliente espera únicamente la respuesta que contiene su `Ref`, descartando cualquier otro mensaje en su mailbox.

**Objetivos de práctica:**

* Uso de `make_ref/0` para identificar respuestas únicas
* Implementación de selective receive seguro con referencias
* Comprender la optimización automática del compilador para este patrón

---

## Preguntas de reflexión

Al terminar los ejercicios, analiza lo siguiente:

* ¿Qué sucede con los mensajes ignorados?
* ¿Cómo crece el mailbox si no se procesan todos los mensajes?
* ¿Qué problemas de rendimiento pueden aparecer en sistemas con miles de mensajes concurrentes?

## Curiosidad

Resulta que mientras resolvia algunos ejercicios yo mismo, me percate de algo curioso:

 Cuando usamos `spawn/3`, y la usamos de la forma `spawn(?MODULE, init, [Cosas])`, si no usamos esa funcion `init` no compila el modulo bien y jamas hace el *spawn*. Se solventa usando la pesima practica `-compile(export_all).` o haciendo `spawn/1` con `spawn(fun init/1)` y haciendo el `-export([init/1])`.