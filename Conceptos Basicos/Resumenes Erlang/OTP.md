
---

## 📚 **Resumen: Erlang/OTP**

### 📌 **¿Qué es esto?**

Son un conjunto de principios que definen cómo estructurar código en Erlang, organizando procesos, módulos y directorios de forma coherente, modular y tolerante a fallos.

---

## 🌳 **Supervision Trees (Árboles de supervisión)**

- Modelo jerárquico para organizar procesos.
    
- **Supervisores** → vigilan a otros procesos (workers).  
    Si algo falla, los reinician.
    
- **Workers** → hacen el trabajo real (cálculos, tareas).
    
- Los procesos se organizan como un árbol donde:
    
    - 🟦 Supervisores = nodos cuadrados
        
    - ⚪ Workers = nodos circulares
        

**Ventaja**: estructura robusta y fácil de recuperar de errores.

---

## 📐 **Behaviours**

- Son **patrones de diseño formales**.
    
- Permiten dividir un proceso en:
    
    - **Parte genérica (behaviour module)** → proporcionada por Erlang.
        
    - **Parte específica (callback module)** → tú defines qué hace.
        

Ejemplos:

- `gen_server` para servidores.
    
- `gen_statem` para máquinas de estado.
    
- `gen_event` para manejar eventos.
    
- `supervisor` para supervisores.
    

**Ventaja**: código más reutilizable, organizado y mantenible.

### 📖 **Ejemplo resumido**

- `server.erl` → servidor genérico
    
- `ch2.erl` → implementación específica  
    → el servidor maneja mensajes `call` y `cast`, y la lógica se delega al módulo callback.
    

### ✅ Ventajas:

- Reutilizas `server` para varios casos.
    
- Ocultas detalles del protocolo.
    
- Facilita modificar, mantener y entender el código.
    

---

## 📦 **Applications**

- Una **aplicación** en Erlang es un conjunto de módulos que implementan una funcionalidad.
    
- Tipos:
    
    - **Library application** → sin procesos (ej: `stdlib`).
        
    - **Aplicación con procesos** → estructurada como **supervision tree**.
        

Ejemplos:

- `kernel` → núcleo de Erlang
    
- `stdlib` → librerías estándar
    
- `mnesia` → base de datos distribuida
    
- `debugger` → depuración
    

---

## 📦 **Releases**

- Una **release** es un sistema completo armado a partir de:
    
    - Aplicaciones OTP
        
    - Aplicaciones propias
        

Se puede:

- Instalar en sistemas destino
    
- Actualizar o degradar versiones en caliente (**Release Handling**)
    

---

## 📑 **Notas**

- El compilador detecta módulos `-behaviour(...)` y avisa si faltan callbacks.
    
- Aunque no uses behaviours, puedes programar más rápido, pero:
    
    - Pierdes claridad, organización y tolerancia a fallos.
        
    - Se vuelve difícil de mantener o que otro lo entienda.
        
---
  *!!!Resumen de GPT*