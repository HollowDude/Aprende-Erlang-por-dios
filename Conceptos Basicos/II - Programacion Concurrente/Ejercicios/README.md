
# Ejercicios de Robustez y Concurrencia

---

### 📌 Ejercicio 1: **Eco Mejorado**

**Objetivo:** practicar `receive`, `spawn`, `register`, y `whereis/1`.

**Descripción:**  
Crea un módulo llamado `eco_server` con:

- Un proceso llamado `eco_server` que:
    
    - Reciba mensajes `{echo, From, Msg}`.
        
    - Responda al proceso `From` con `{echo_reply, Msg}`.
        
- Una función `start()` que registre este proceso si no existe.
    
- Una función `enviar(Mensaje)` que:
    
    - Verifique si el servidor está corriendo (con `whereis/eco_server`).
        
    - Si está, le envíe un mensaje y reciba la respuesta.
        

👉 Extensión: que se imprima `timeout` si no responde en 3 segundos.

---

### 📌 Ejercicio 2: **Ping-Pong con Timeouts**

**Objetivo:** reforzar `after`, `receive`, `spawn`.

**Descripción:**

- Crea `ping(N, PongPid)`.
    
- Crea `pong()`, con un `after` de 2000 ms que imprime `Pong: me canse de esperar`.
    
- Que `ping` y `pong` se manden mensajes `{ping, Pid}` y `pong` como en los ejemplos, pero con control de tiempo.
    

---

### 📌 Ejercicio 3: **Contador de Vidas**

**Objetivo:** manejar estado mutable con recursión.

**Descripción:**

- Un proceso llamado `vidas` que:
    
    - Inicie con `N` vidas.
        
    - Reciba `{perder, Cantidad}` y reste vidas.
        
    - Si llega a 0, imprima `Muerto` y termine.
        
    - Reciba `{ganar, Cantidad}` y sume vidas.
        
    - Responda con `{vidas, N}` si recibe `consulta`.
        

👉 Extensión: implementar `timeout` para autodestruirse si no recibe mensajes en 10 segundos.

---

### 📌 Ejercicio 4: **Cliente-Servidor con Login**

**Objetivo:** practicar `register`, `whereis`, `spawn`, `case`.

**Descripción:**

- Un servidor que mantenga una lista de clientes `{Name, Pid}`.
    
- Funciones:
    
    - `login(Name)`
        
    - `logout(Name)`
        
    - `listar_usuarios()`
        
- Si alguien trata de hacer login dos veces, responder `ya_logeado`.
    
- Si alguien envía un mensaje a un usuario que no existe, responder `usuario_no_existe`.
    

👉 Extensión: desconectar automáticamente usuarios inactivos en 30 segundos usando `after`.

---

### 📌 Ejercicio 5: **Sistema de Enlaces**

**Objetivo:** practicar `link/1`, `spawn_link`, `trap_exit`.

**Descripción:**

- Un proceso `padre` que cree 3 procesos `hijo` con `spawn_link`.
    
- Cada `hijo` espera un mensaje `{matar}` y hace `exit(razon)`.
    
- El `padre` debe capturar las salidas con `trap_exit` y reportar qué hijo murió y por qué.
    

👉 Extensión:  
Si un hijo muere, el padre decide si reiniciarlo o no.

---

### 📌 Ejercicio 6: **Pequeña Red Social**

**Objetivo:** integrar todo.

**Descripción:**

- Un servidor `social_server` con:
    
    - Registro de usuarios.
        
    - Envío de mensajes `{de, Para, Contenido}`.
        
    - Listado de usuarios conectados.
        
    - Desconexión automática con `after` si no hay actividad en 60s.
        
- Clientes que se loguean, envían mensajes y pueden recibirlos en su buzón.
    

👉 Extensión:  
Que se pueda enviar un mensaje a todos los usuarios conectados.

---

## ✅ Sugerencias

- Haz **una versión básica** primero, luego ve agregando:
    
    - `after` para timeouts.
        
    - `trap_exit` para manejar caídas.
        
    - `link` para controlar dependencias.
        
- No te compliques con nodos distribuidos aún — mantén todo local.
    
- Cuando todo te funcione, experimenta corriéndolo en nodos diferentes.
    
