% Práctica No 8 - Programación lógica
% El motor de búsqueda de Prolog
% Ejercicio 1 - Considerar la siguiente base de conocimiento.

padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).

% i. ¿Cuál el resultado de la consulta abuelo(X, manuel)?
% ii. A partir del predicado binario padre, definir en Prolog los predicados binarios: hijo, hermano y
descendiente.
% iii. Dibujar el árbol de búsqueda de Prolog para la consulta descendiente(Alguien, juan).
% iv. ¿Qué consulta habría que hacer para encontrar a los nietos de juan?
% v. ¿Cómo se puede definir una consulta para conocer a todos los hermanos de pablo?
% vi. Considerar el agregado del siguiente hecho y regla y la base de conocimiento del ítem anterior.

ancestro(X, X).
ancestro(X, Y) :- ancestro(Z, Y), padre(X, Z).

% vii. Explicar la respuesta a la consulta ancestro(juan, X). ¿Qué sucede si se pide más de un resultado?
% viii. Sugerir un solución al problema hallado en los puntos anteriores reescribiendo el programa de ancestro.


