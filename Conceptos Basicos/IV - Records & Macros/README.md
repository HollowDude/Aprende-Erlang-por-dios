# IV - Records & Macros

---

## 1.0 Records y Macros

Los programas Erlang medianamente grandes se estructuran dividiéndolos en **múltiples archivos**, cada uno con una interfaz bien definida que permite modularizar el sistema.

---

### 1.1 División de un Ejemplo en Varios Archivos

Para ilustrarlo, se toma como referencia un **sistema de mensajería** dividido en **cinco archivos**:

|Archivo|Descripción|
|:--|:--|
|`mess_config.hrl`|Configuración del nodo servidor (macro).|
|`mess_interface.hrl`|Definiciones de **records** (estructuras) para la interfaz cliente-servidor.|
|`user_interface.erl`|Funciones de la interfaz de usuario.|
|`mess_client.erl`|Lógica del cliente en el nodo usuario.|
|`mess_server.erl`|Lógica del servidor de mensajería.|

Durante esta división:

- Se formaliza la **interfaz entre shell, cliente y servidor** mediante **records**.
    
- Se introducen **macros** para facilitar configuraciones reutilizables.
    

---

### 📑 `mess_config.hrl`

Define la ubicación del servidor mediante macro:

```erlang
-define(server_node, messenger@super).
```

Esto permite usar `?server_node` como una constante, y cambiar su valor desde un solo lugar.

---

### 📑 `mess_interface.hrl`

Contiene los **records** que definen los mensajes entre:

- **Cliente ⇄ Servidor**
    
- **Cliente ⇄ Shell**
    
- **Servidor ⇄ Cliente**
    

Ejemplo:

```erlang
-record(logon, {client_pid, username}).
-record(message, {client_pid, to_name, message}).
-record(server_reply, {message}).
```

👉 Esta práctica formaliza la comunicación entre procesos, usando estructuras bien definidas.

---

### 📑 `user_interface.erl`

Implementa la **API pública** para los usuarios:

- `logon/1` → Loguear un usuario.
    
- `logoff/0` → Cerrar sesión.
    
- `message/2` → Enviar mensajes.
    

Ejemplo:

```erlang
logon(Name) ->
    case whereis(mess_client) of 
        undefined ->
            register(mess_client, spawn(mess_client, client, [?server_node, Name]));
        _ -> already_logged_on
    end.
```

---

### 📑 `mess_client.erl`

Implementa el **cliente** que se comunica con el servidor:

- Se conecta usando `#logon`.
    
- Envía `#message_to`.
    
- Escucha mensajes de:
    
    - Shell (`message_to`, `logoff`)
        
    - Servidor (`server_reply`, `message_from`)
        

Ejemplo de patrón `receive`:

```erlang
receive
    logoff -> exit(normal);
    #message_to{to_name=To, message=Message} -> ...
end.
```

---

### 📑 `mess_server.erl`

Implementa el **servidor** que:

- Gestiona usuarios conectados (`[{Pid, Name}]`).
    
- Maneja logins, logoffs, y transferencia de mensajes.
    

Funciones clave:

- `start_server/0` → Inicia servidor.
    
- `server/1` → Bucle de espera de mensajes.
    
- `server_logon/3` → Añade usuario.
    
- `server_logoff/2` → Elimina usuario.
    
- `server_transfer/4` → Envía mensajes.
    

---

### 1.2 Header Files (`.hrl`)

Son **archivos cabecera** que se incluyen en los `.erl` con:

```erlang
-include("archivo.hrl").
```

Normalmente contienen:

- **Macros**
    
- **Definiciones de Records**
    

Esto permite mantener interfaces centralizadas y consistentes.

---

### 2.0 Records

Son una forma **estructurada y con nombre** de manejar datos complejos.

Definición:

```erlang
-record(message_to, {to_name, message}).
```

Equivale a la tupla:

```erlang
{message_to, ToName, Message}
```

Creación:

```erlang
#message_to{message="hola", to_name=fred}
```

Produce:

```erlang
{message_to, fred, "hola"}
```

**Ventajas**:

- Los campos pueden asignarse en cualquier orden.
    
- Si omites un campo, toma `undefined`.
    
- Cambiar la estructura se hace **en un solo lugar**.
    

Uso en `receive`:

```erlang
#message_to{to_name=To, message=Msg} -> ...
```

---

### 3.0 Macros

Permiten definir **valores o expresiones simbólicas** que se reemplazan en tiempo de compilación.

Ejemplo:

```erlang
-define(server_node, messenger@super).
```

Uso:

```erlang
?server_node
```

Macro especial:

```erlang
?MODULE
```

Equivale al nombre del módulo actual.

**Ventajas**:

- Centralizan configuraciones.
    
- Evitan repeticiones.
    
- Facilitan cambios sin modificar todo el código.
    

---

## ✅ Conclusión

Este ejemplo deja claro:

- **Records** → Definen estructuras de datos legibles y mantenibles.
    
- **Macros** → Centralizan valores reutilizables.
    
- **Archivos `.hrl`** → Unifican interfaces entre módulos.
    

Esto da lugar a sistemas **modulares**, **claros** y **fáciles de mantener**, ya que cualquier cambio en la interfaz o la configuración se hace en un solo sitio.

---
  *!!!Resumen de GPT*