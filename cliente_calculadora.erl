-module(cliente_calculadora).
% Define el módulo llamado cliente_calculadora.

-export([enviar_solicitud/3, recibir_respuesta/0]).
% Exporta las funciones enviar_solicitud/3 y recibir_respuesta/0 para que puedan ser llamadas desde otros módulos.

enviar_solicitud(Operacion, A, B) ->
    % Define la función enviar_solicitud/3, que envía una solicitud de operación a un nodo específico.
    io:format("Enviando solicitud de ~p con ~p y ~p~n", [Operacion, A, B]),
    % Imprime en la consola un mensaje indicando el tipo de operación y los operandos.

    Nodo = case Operacion of
        suma -> {nodo_suma, 'nodo_suma@LAPTOP-3Q6DAUQ8'};
        % Si la operación es suma, el nodo de destino es 'nodo_suma@LAPTOP-3Q6DAUQ8'.
        resta -> {nodo_resta, 'nodo_resta@LAPTOP-3Q6DAUQ8'};
        % Si la operación es resta, el nodo de destino es 'nodo_resta@LAPTOP-3Q6DAUQ8'.
        multiplicacion -> {nodo_multiplicacion, 'nodo_multiplicacion@LAPTOP-3Q6DAUQ8'};
        % Si la operación es multiplicación, el nodo de destino es 'nodo_multiplicacion@LAPTOP-3Q6DAUQ8'.
        division -> {nodo_division, 'nodo_division@LAPTOP-3Q6DAUQ8'}
        % Si la operación es división, el nodo de destino es 'nodo_division@LAPTOP-3Q6DAUQ8'.
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
