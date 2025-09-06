# Ejercicios de práctica

Estos ejercicios están diseñados para practicar:

- **Links entre procesos**
- **Trampas de salida (`trap_exit`)**
- **Señales de terminación (`exit/2`)**
- **Muerte controlada vs forzada**
- **Monitores (`erlang:monitor/2`)**
- **Procesos con nombre (`register/2`)**
- **Construcción de mini-supervisores**

Los ejercicios están organizados en **niveles de dificultad** progresivos.

---

## Nivel 1 – Fundamentos

### Ejercicio 1: Crear y enlazar procesos
1. Crea un módulo que genere dos procesos: `proc_a` y `proc_b`.  
2. Haz que `proc_a` se enlace (`link`) con `proc_b`.  
3. Provoca que `proc_b` falle con una excepción (`1 div 0`).  
4. Observa qué ocurre con `proc_a`.

**Pregunta:** ¿Por qué ocurre ese comportamiento?

---

### Ejercicio 2: Atrapar una salida con `trap_exit`
1. Modifica el ejercicio anterior para que `proc_a` ejecute `process_flag(trap_exit, true)` antes de enlazarse.  
2. Haz que `proc_a` imprima el mensaje `{'EXIT', Pid, Reason}` recibido cuando `proc_b` muera.  

**Pregunta:** ¿En qué se diferencia este caso del Ejercicio 1?

---

## Nivel 2 – Señales y control de fallos

### Ejercicio 3: Terminación controlada con `exit/2`
1. Implementa un proceso `server` que entre en un bucle recibiendo mensajes.  
2. Desde el shell, envía `exit(ServerPid, shutdown)`.  
3. Haz que `server` capture ese mensaje si tiene `trap_exit` activado.  

**Pregunta:** ¿Qué diferencia observas entre `exit(ServerPid, shutdown)` y `exit(ServerPid, kill)`?

---

### Ejercicio 4: Muerte “suave” vs “dura”
1. Implementa un supervisor que monitorice a un `worker`.  
2. Envía `exit(WorkerPid, normal)` y observa el resultado.  
3. Envía `exit(WorkerPid, kill)` y compara el comportamiento.  

**Pregunta:** ¿Por qué el supervisor actúa de forma distinta en cada caso?

**Solucion:** Usando el shell de Erlang solamente:

---

```Erlang
16> spawn_monitor(fun() -> receive _ -> ok end end).
{<0.120.0>,#Ref<0.1060326022.3958374401.235265>}
17> exit(<0.120.0>, normal).
true
18> flush().
ok
19> spawn_monitor(fun() -> receive _ -> ok end end).
{<0.125.0>,#Ref<0.1060326022.3958374401.235296>}
20> exit(<0.125.0>, kill).
true
21> flush().
Shell got {'DOWN',#Ref<0.1060326022.3958374401.235296>,process,<0.125.0>,
                  killed}
ok
```

---

## Nivel 3 – Monitores y procesos con nombre

### Ejercicio 5: Usar monitores en lugar de links
1. Implementa un proceso `monitoring_proc` que llame a `erlang:monitor(process, WorkerPid)`.  
2. Haz que `WorkerPid` falle con un error.  
3. Captura e imprime el mensaje de monitor recibido.  

**Pregunta:** ¿Qué diferencia existe respecto al uso de un link?

---

### Ejercicio 6: Registrar procesos
1. Crea un proceso llamado `logger` y regístralo con `register(logger, Pid)`.  
2. Desde otros procesos, envíale mensajes usando el átomo `logger` en lugar de su `Pid`.  

**Pregunta:** ¿Qué problemas pueden surgir si intentas registrar dos procesos con el mismo nombre?

---

## Nivel 4 – Retos combinados

### Ejercicio 7: Mini-supervisor con links y traps
1. Implementa un proceso `sup` que:
   - Cree un worker.  
   - Lo enlace (`link`).  
   - Active `trap_exit`.  
   - Cuando el worker muera, imprima la razón y cree un nuevo worker.  
2. Haz que el worker falle aleatoriamente con:  
   ```erlang
   if random:uniform(2) =:= 1 -> exit(crash); true -> ok end.
```

---

### Ejercicio 8: Monitores y procesos con nombre

1. Implementa un proceso central llamado `dispatcher`, registrado con `register(dispatcher, Pid)`.
    
2. Cada cliente que se conecte debe registrarse temporalmente en el `dispatcher`.
    
3. El `dispatcher` debe monitorizar a cada cliente.
    
4. Cuando un cliente muera, el `dispatcher` debe imprimir un log:
    
    ```
    client <Pid> died with reason <Reason>
    ```
    

**Pregunta adicional:** ¿Qué ventaja aporta el uso de **monitores** en lugar de **links** en este caso?

---

## 📌 Recomendaciones

- Resuelve cada ejercicio en un módulo `errors_exercises.erl`.
    
- Compila con `c(errors_exercises).` en el shell.
    
- Prueba cada escenario paso a paso.
    
- Reflexiona sobre las diferencias entre:
    
    - Links y Monitores
        
    - Muertes normales vs anormales
        
    - Trampas activadas vs desactivadas
        

---
