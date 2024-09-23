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
