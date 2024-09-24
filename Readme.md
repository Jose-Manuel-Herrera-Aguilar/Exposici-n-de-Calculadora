# Portada

<pre>

  <p align=center>
    
Tecnológico Nacional de México  
Instituto Tecnológico de Tijuana  

Departamento de Sistemas y Computación  
Ingeniería en Sistemas Computacionales  

Semestre:  
Agosto - Diciembre 2024  

Materia:  
Programación Lógica y Funcional  

Docente:  
M.C. Rene Solis Reyes  

Unidad:  
1  

Título del trabajo:  
Exposición Calculadora en Erlang  

Estudiantes:  
Corrales Quintero Erick Roberto - 23211004  
Herrera Aguilar José Manuel - 20212410
Arenas Herrera Josue - 22210282
Garcia Bagnis Diego Samuel - 20211778
  </p>

</pre>

# Proyecto 3: Calculadora Distribuida en Erlang

#### Objetivo del Proyecto

El objetivo de este proyecto es desarrollar una calculadora distribuida utilizando Erlang, donde los cálculos se realizan en diferentes nodos. Esta implementación demostrará la capacidad de Erlang para manejar la distribución de tareas y la comunicación entre nodos en un sistema distribuido.

#### Descripción del Proyecto

El proyecto consistirá en configurar cuatro nodos de Erlang, cada uno responsable de una operación aritmética básica: suma, resta, multiplicación y división. Los nodos se comunicarán entre sí utilizando el paso de mensajes para realizar los cálculos. Se implementará un cliente que enviará solicitudes de cálculo a los nodos apropiados y recibirá los resultados.

#### Requisitos del Proyecto

1. **Configuración de Nodos**: Se deben configurar cuatro nodos de Erlang:
   - Nodo de Suma: `nodo_suma@nombre_host`
   - Nodo de Resta: `nodo_resta@nombre_host`
   - Nodo de Multiplicación: `nodo_multiplicacion@nombre_host`
   - Nodo de División: `nodo_division@nombre_host`

2. **Comunicación entre Nodos**: Los nodos deben poder comunicarse entre sí. Esto se puede verificar utilizando el comando `net_adm:ping/1` para asegurarse de que los nodos estén conectados.

3. **Implementación de Funciones de Calculadora**: Cada nodo debe implementar una función para realizar su operación específica. Las operaciones deben ser manejadas utilizando un proceso servidor que escuche solicitudes y responda con resultados.

4. **Cliente de Cálculo**: Se debe implementar un cliente que envíe solicitudes de cálculo a los nodos correspondientes y maneje las respuestas.

#### Implementación Detallada

1. **Configuración de los Nodos de Erlang**:
   - Iniciar los nodos con los siguientes comandos en diferentes terminales:
     ```shell
     $ erl -sname nodo_suma
     $ erl -sname nodo_resta
     $ erl -sname nodo_multiplicacion
     $ erl -sname nodo_division
     ```

   - Asegurarse de que los nodos puedan comunicarse entre sí:
     ```erlang
     % En la shell de cada nodo
     net_adm:ping('nodo_suma@nombre_host').
     net_adm:ping('nodo_resta@nombre_host').
     net_adm:ping('nodo_multiplicacion@nombre_host').
     net_adm:ping('nodo_division@nombre_host').
     ```

2. **Definición del Protocolo de Comunicación**:
   - Se utilizará un protocolo simple de paso de mensajes donde el cliente envía una tupla `{operacion, A, B, From}` a un nodo, y el nodo responde con `{resultado, Resultado}`.

# Código

## nodo_suma.erl

```erlang
-module(nodo_suma).
% Define el módulo llamado nodo_suma.

-export([start/0, loop/0]).
% Exporta las funciones start/0 y loop/0 para que puedan ser llamadas desde otros módulos.

start() ->
    % Define la función start/0, que inicia el proceso y lo registra con un nombre.
    register(nodo_suma, spawn(fun loop/0)).
    % spawn(fun loop/0) crea un nuevo proceso que ejecuta la función loop/0.
    % register(nodo_suma, ...) registra este proceso con el nombre 'nodo_suma',
    % permitiendo que otros procesos se refieran a él por nombre.

loop() ->
    % Define la función loop/0, que se encarga de recibir y procesar mensajes.
    receive
        % Espera y recibe mensajes que coincidan con el patrón especificado.
        {suma, A, B, From} ->
            % Cuando recibe un mensaje con el patrón {suma, A, B, From}, 
            % donde 'A' y 'B' son los operandos y 'From' es el PID del remitente:
            io:format("Recibido mensaje de suma: ~p + ~p~n", [A, B]),
            % Imprime en la consola el mensaje recibido, mostrando los operandos.
            Resultado = A + B,
            % Calcula el resultado de la suma.
            From ! {resultado, Resultado},
            % Envía el resultado de vuelta al proceso que envió el mensaje.
            loop();
            % Llama a loop/0 nuevamente para seguir recibiendo mensajes.
        _ ->
            % Si el mensaje no coincide con el patrón esperado:
            io:format("Mensaje desconocido~n"),
            % Imprime un mensaje indicando que se recibió un mensaje desconocido.
            loop()
            % Llama a loop/0 nuevamente para seguir recibiendo mensajes.
    end.
```

## nodo_resta.erl

```erlang
-module(nodo_resta).
% Define el módulo llamado nodo_resta.

-export([start/0, loop/0]).
% Exporta las funciones start/0 y loop/0 para que puedan ser llamadas desde otros módulos.

start() ->
    % Define la función start/0, que inicia el proceso y lo registra con un nombre.
    register(nodo_resta, spawn(fun loop/0)).
    % spawn(fun loop/0) crea un nuevo proceso que ejecuta la función loop/0.
    % register(nodo_resta, ...) registra este proceso con el nombre 'nodo_resta',
    % permitiendo que otros procesos se refieran a él por nombre.

loop() ->
    % Define la función loop/0, que se encarga de recibir y procesar mensajes.
    receive
        % Espera y recibe mensajes que coincidan con el patrón especificado.
        {resta, A, B, From} ->
            % Cuando recibe un mensaje con el patrón {resta, A, B, From}, 
            % donde 'A' y 'B' son los operandos y 'From' es el PID del remitente:
            io:format("Recibido mensaje de resta: ~p - ~p~n", [A, B]),
            % Imprime en la consola el mensaje recibido, mostrando los operandos.
            Resultado = A - B,
            % Calcula el resultado de la resta.
            From ! {resultado, Resultado},
            % Envía el resultado de vuelta al proceso que envió el mensaje.
            loop();
            % Llama a loop/0 nuevamente para seguir recibiendo mensajes.
        _ ->
            % Si el mensaje no coincide con el patrón esperado:
            io:format("Mensaje desconocido~n"),
            % Imprime un mensaje indicando que se recibió un mensaje desconocido.
            loop()
            % Llama a loop/0 nuevamente para seguir recibiendo mensajes.
    end.
```

## nodo_multiplicacion.erl

```erlang
-module(nodo_multiplicacion).
% Define el módulo llamado nodo_multiplicacion.

-export([start/0, loop/0]).
% Exporta las funciones start/0 y loop/0 para que puedan ser llamadas desde otros módulos.

start() ->
    % Define la función start/0, que inicia el proceso y lo registra con un nombre.
    register(nodo_multiplicacion, spawn(fun loop/0)).
    % spawn(fun loop/0) crea un nuevo proceso que ejecuta la función loop/0.
    % register(nodo_multiplicacion, ...) registra este proceso con el nombre 'nodo_multiplicacion',
    % permitiendo que otros procesos se refieran a él por nombre.

loop() ->
    % Define la función loop/0, que se encarga de recibir y procesar mensajes.
    receive
        % Espera y recibe mensajes que coincidan con el patrón especificado.
        {multiplicacion, A, B, From} ->
            % Cuando recibe un mensaje con el patrón {multiplicacion, A, B, From}, 
            % donde 'A' y 'B' son los operandos y 'From' es el PID del remitente:
            io:format("Recibido mensaje de multiplicacion: ~p * ~p~n", [A, B]),
            % Imprime en la consola el mensaje recibido, mostrando los operandos.
            Resultado = A * B,
            % Calcula el resultado de la multiplicación.
            From ! {resultado, Resultado},
            % Envía el resultado de vuelta al proceso que envió el mensaje.
            loop();
            % Llama a loop/0 nuevamente para seguir recibiendo mensajes.
        _ ->
            % Si el mensaje no coincide con el patrón esperado:
            io:format("Mensaje desconocido~n"),
            % Imprime un mensaje indicando que se recibió un mensaje desconocido.
            loop()
            % Llama a loop/0 nuevamente para seguir recibiendo mensajes.
    end.
```

## nodo_division.erl

```erlang
-module(nodo_division).
% Define el módulo llamado nodo_division.

-export([start/0, loop/0]).
% Exporta las funciones start/0 y loop/0 para que puedan ser llamadas desde otros módulos.

start() ->
    % Define la función start/0, que inicia el proceso y lo registra con un nombre.
    register(nodo_division, spawn(fun loop/0)).
    % spawn(fun loop/0) crea un nuevo proceso que ejecuta la función loop/0.
    % register(nodo_division, ...) registra este proceso con el nombre 'nodo_division',
    % permitiendo que otros procesos se refieran a él por nombre.

loop() ->
    % Define la función loop/0, que se encarga de recibir y procesar mensajes.
    receive
        {division, A, B, From} ->
            % Cuando recibe un mensaje con el patrón {division, A, B, From}, 
            % donde 'A' y 'B' son los operandos y 'From' es el PID del remitente:
            io:format("Recibido mensaje de division: ~p / ~p~n", [A, B]),
            % Imprime en la consola el mensaje recibido, mostrando los operandos.
            case B of
                0 ->
                    % Si el divisor 'B' es 0 (lo que causaría una división por cero):
                    From ! {error, "División por cero no permitida"},
                    % Envía un mensaje de error al proceso que envió el mensaje.
                    loop();
                _ ->
                    % Si el divisor no es 0:
                    Resultado = A / B,
                    % Calcula el resultado de la división.
                    From ! {resultado, Resultado},
                    % Envía el resultado de vuelta al proceso que envió el mensaje.
                    loop()
            end;
        _ ->
            % Si el mensaje no coincide con el patrón esperado:
            io:format("Mensaje desconocido~n"),
            % Imprime un mensaje indicando que se recibió un mensaje desconocido.
            loop()
    end.
```

## cliente_calculadora.erl

```erlang
-module(cliente_calculadora).
% Define el módulo llamado cliente_calculadora.

-export([enviar_solicitud/3, recibir_respuesta/0]).
% Exporta las funciones enviar_solicitud/3 y recibir_respuesta/0 para que puedan ser llamadas desde otros módulos.

enviar_solicitud(Operacion, A, B) ->
    % Define la función enviar_solicitud/3, que envía una solicitud de operación a un nodo específico.
    io:format("Enviando solicitud de ~p con ~p y ~p~n", [Operacion, A, B]),
    % Imprime en la consola un mensaje indicando el tipo de operación y los operandos.

    Nodo = case Operacion of
        suma -> {nodo_suma, 'nodo_suma@ip-172-31-86-126'};
        % Si la operación es suma, el nodo de destino es 'nodo_suma@ip-172-31-86-12'.
        resta -> {nodo_resta, 'nodo_resta@ip-172-31-86-12'};
        % Si la operación es resta, el nodo de destino es 'nodo_resta@ip-172-31-86-12'.
        multiplicacion -> {nodo_multiplicacion, 'nodo_multiplicacion@ip-172-31-86-12'};
        % Si la operación es multiplicación, el nodo de destino es 'nodo_multiplicacion@ip-172-31-86-12'.
        division -> {nodo_division, 'nodo_division@ip-172-31-86-12'}
        % Si la operación es división, el nodo de destino es 'nodo_division@ip-172-31-86-12'.
    end,

    % Envia el mensaje de operación al nodo especificado.
    erlang:send(Nodo, {Operacion, A, B, self()}),
    % El mensaje tiene el formato {Operacion, A, B, self()}, donde 'self()' es el PID del proceso actual.

    recibir_respuesta().
    % Llama a la función recibir_respuesta/0 para recibir y procesar la respuesta del nodo.

recibir_respuesta() ->
    % Define la función recibir_respuesta/0, que se encarga de recibir y manejar la respuesta del nodo.
    receive
        {resultado, Resultado} ->
            % Cuando recibe un mensaje con el patrón {resultado, Resultado}:
            io:format("El resultado es: ~p~n", [Resultado]);
            % Imprime en la consola el resultado de la operación.
        {error, Motivo} ->
            % Cuando recibe un mensaje con el patrón {error, Motivo}:
            io:format("Error: ~p~n", [Motivo])
            % Imprime en la consola el motivo del error.
        after 5000 ->
            % Si no se recibe un mensaje dentro del tiempo límite de 5000 milisegundos (5 segundos):
            io:format("No se recibió respuesta en el tiempo límite~n")
            % Imprime un mensaje indicando que no se recibió respuesta a tiempo.
    end.
```
# Implementación

## Links a Asciinema

### Nodo Suma

https://asciinema.org/a/vBKMfnRxHSclt180Vly3KLLbR

### Nodo Resta

https://asciinema.org/a/FKoAVLtrC2nTeTm6MZy5i4Ore

### Nodo Multiplicación

https://asciinema.org/a/4gu9mEldPzeesuMjSfBMaHqy9

### Nodo Division

https://asciinema.org/a/IQvW1wSo4alecX3X5mX5LzFPX

### Calculadora

https://asciinema.org/a/BnqvDnT5KgaEY1crI3nATH8ec




