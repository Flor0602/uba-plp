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

% Operaciones sobre listas
% Ejercicio 4 - Definir el pred juntar(?L1,?L2,?L3), que tiene exito si L3 es la concatenación de L1 y L2. (Este pred ya está definido como append)

juntar([],Ys,Ys).
juntar([X|Xs],Ys,[X|Zs]) :- juntar(Xs,Ys,Zs).

% Ejercicio 5 - Definir los predicados usando append.
% last(?L,?U)
last(L,U) :- append(_,[U],L).

% reverse(+L,-L1)
reverse([],[]).
reverse([X|Xs],L1) :- reverse(Xs,S), append(S,[X],L1).

% prefijo(?P, +L)
prefijo(P,L) :- append(P,_,L).

% sufijo(?S, +L)
sufijo(S,L) :- append(_,S,L).

% sublista(?S, +L)
sublista(SL,L) :- sufijo(SL,R), prefijo(R,L).

% pertenece(?X, +L)
pertenece(X,L) :- sublista([X],L).

/*
Ejercicio 6 - Denir el predicado aplanar(+Xs, -Ys), que es verdadero sii Ys contiene los elementos de todos los niveles de
Xs, en el mismo orden de aparición. Los elementos de Xs son enteros, átomos o nuevamente listas, de modo que
Xs puede tener una profundidad arbitraria. Por el contrario, Ys es una lista de un solo nivel de profundidad.
Ejemplos:
?- aplanar([a, [3, b, []], [2]], L).→ L=[a, 3, b, 2]
?- aplanar([[1, [2, 3], [a]], [[[]]]], L).→ L=[1, 2, 3, a]
Nota: este predicado ya está denido en prolog con el nombre flatten.
*/

aplanar([],[]).
aplanar().
