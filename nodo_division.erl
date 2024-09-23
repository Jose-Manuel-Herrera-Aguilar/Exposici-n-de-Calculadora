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
