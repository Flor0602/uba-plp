% ============================ PRACTICA 8 ===========================
%                 🌟🌟🌟 PROGRAMACIÓN LÓGICA 🌟🌟🌟
% ====================================================================
%                 El motor de búsqueda de Prolog
% ====================================================================

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

% X = juan.

% ii. A partir del predicado binario padre, definir en Prolog los predicados binarios: hijo, hermano y descendiente.

% X es hijo de Y si Y es padre de X.
hijo(X, Y) :- padre(Y, X).

% X es hermano de Y si tienen el mismo padre y no son la misma persona.
hermano(X, Y) :- padre(Z, X), padre(Z, Y), X \= Y.

% X es descendiente de Y si Y es su padre, o si X es descendiente de un hijo de Y.
descendiente(X, Y) :- padre(Y, X).
descendiente(X, Y) :- padre(Z, X), descendiente(Z, Y).

% iii. Dibujar el árbol de búsqueda de Prolog para la consulta descendiente(Alguien, juan).

% El árbol de búsqueda sería:
% juan
% ├── carlos
% │   ├── daniel
% │   └── diego
% └── luis
%     ├── pablo
%     ├── manuel
%     └── ramiro

% iv. ¿Qué consulta habría que hacer para encontrar a los nietos de juan?

% abuelo(juan, X).

% v. ¿Cómo se puede definir una consulta para conocer a todos los hermanos de pablo?

% hermano(pablo, X).

% vi. Considerar el agregado del siguiente hecho y regla y la base de conocimiento del ítem anterior.

ancestro(X, X).
ancestro(X, Y) :- ancestro(Z, Y), padre(X, Z).

% vii. Explicar la respuesta a la consulta ancestro(juan, X). ¿Qué sucede si se pide más de un resultado?

% La consulta ancestro(juan, X) devolverá todos los descendientes de juan (incluyendo a él mismo).
% Si se pide más de un resultado, Prolog buscará todos los resultados posibles. Sin embargo, en este caso, la recursión podría ir infinitamente debido a la forma en que está definida.

% viii. Sugerir un solución al problema hallado en los puntos anteriores reescribiendo el programa de ancestro.

ancestro(X, X).
ancestro(X, Y) :- padre(X, Z), ancestro(Z, Y).


% Ejercicio 2

% Sea el siguiente programa lógico:

% vecino(X, Y, [X|[Y|Ls]]).
% vecino(X, Y, [W|Ls]) :- vecino(X, Y, Ls).

% i. Mostrar el árbol de búsqueda en Prolog para resolver vecino(5, Y, [5,6,5,3]), devolviendo todos los valores de Y que hacen que la meta se deduzca lógicamente del programa.

% Árbol de búsqueda:

% vecino(5, Y, [5,6,5,3])
% ├── Y = 6 (primer caso)
% └── vecino(5, Y, [6,5,3])
%     ├── Y = 3 (segundo caso)
%     └── false (sin más elementos)


% Ejercicio 3

% Considerar las siguientes definiciones:

natural(0).
natural(suc(X)) :- natural(X).

menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).
menorOIgual(X,X) :- natural(X).

% i. Explicar qué sucede al realizar la consulta menorOIgual(0, X).

% La consulta menorOIgual(0, X) devuelve todos los números mayores o iguales a 0, es decir:
% X = 0 ; X = suc(0) ; X = suc(suc(0)) ; etc. hasta el infinito.

% ii. Describir las circunstancias en las que puede colgarse un programa en Prolog.

% Un programa en Prolog puede colgarse (entrar en recursión infinita) cuando no tiene un caso base adecuado para detener la recursión o si las reglas están mal definidas. 
% Ejemplo: 
% p(X) :- p(X).
% Esto causaría que Prolog entre en un ciclo infinito sin devolver resultados.

% iii. Corregir la definición de menorOIgual para que funcione adecuadamente.

menorOIgual(X, X) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).

% ===========================================================
%%                  Operaciones sobre listas
% ===========================================================

% Ejercicio 4 - Definir el predicado juntar(?L1,?L2,?L3), que tiene éxito si L3 es la concatenación de L1 y L2. Al igual que la mayoría de los predicados, puede dar false después de agotar los resultados. (Este pred ya está definido como append)

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


% Ejercicio 6 - Definir el predicado aplanar(+Xs, -Ys), que es verdadero sii Ys contiene los elementos de todos los niveles de Xs, 
% en el mismo orden de aparición. Los elementos de Xs son enteros, átomos o nuevamente listas, de modo que
% Xs puede tener una profundidad arbitraria. Por el contrario, Ys es una lista de un solo nivel de profundidad.
% Ejemplos:
% ?- aplanar([a, [3, b, []], [2]], L).→ L=[a, 3, b, 2]
% ?- aplanar([[1, [2, 3], [a]], [[[]]]], L).→ L=[1, 2, 3, a]
% Nota: este predicado ya está definido en prolog con el nombre flatten.

aplanar([],[]).
aplanar([X|Xs], L) :- not(is_list(X)), aplanar(Xs, Ys), append([X],Ys,L).
aplanar([X|Xs], Ys) :- is_list(X), aplanar(X, Z), aplanar(Xs, Ws), append(Z, Ws, Ys).


% Ejercicio 7 - Definir los siguientes predicados, usando member y/o append según sea conveniente.

% i. intersección(+L1, +L2, -L3) tal que L3 es la intersección sin repeticiones de las listas L1 y L2, respetando en L3 el orden en que aparecen los elementos en L1.

interseccion([], _, []).
interseccion([X|Xs], L2, [X|L3]) :- member(X, L2), not(member(X, Xs)), interseccion(Xs, L2, L3).
interseccion([_|Xs], L2, L3) :- interseccion(Xs, L2, L3).

% ii. partir(N, L, L1, L2), donde L1 tiene los N primeros elementos de L, y L2 el resto. 
% Si L tiene menos de N elementos el predicado debe fallar. ¿Cuán reversible es este predicado? Es decir, ¿qué parámetros pueden estar indefinidos al momento de la invocación?

partir(0, L, [], L). 
partir(N, [X|Xs], [X|L1], L2) :-
    N > 0,
    N1 is N - 1,
    partir(N1, Xs, L1, L2).

% Ejemplo:
% ?- partir(2, [a, b, c, d], L1, L2).
% L1 = [a, b], L2 = [c, d].

% Reversibilidad: Este predicado no es completamente reversible, porque si L o N están indefinidos, y Prolog no puede generar todas las posibilidades.

% iii. borrar(+ListaOriginal, +X, -ListaSinXs), que elimina todas las ocurrencias de X de la lista ListaOriginal.

borrar([], _, []). 
borrar([X|Xs], X, Ys) :- borrar(Xs, X, Ys).
borrar([Z|Xs], X, [Z|Ys]) :- Z \= X, borrar(Xs, X, Ys).

% iv. sacarDuplicados(+L1, -L2), que saca todos los elementos duplicados de la lista L1. 

sacarDuplicados([], []). 
sacarDuplicados([X|Xs], [X|Ys]) :- not(member(X, Xs)), sacarDuplicados(Xs, Ys).
sacarDuplicados([X|Xs], Ys) :- member(X, Xs), sacarDuplicados(Xs, Ys).

% iv. permutación(+L1, ?L2), que tiene éxito cuando L2 es permutación de L1. 
% ¿Hay una manera más eficiente de definir este predicado para cuando L2 está instanciada?

permutacion([], []).
permutacion([X|Xs], Ys) :- permutacion(Xs, Zs), insertar(X, Zs, Ys).

insertar(X, L, Z) :- append(P, S, L), append(P, [X|S], Z).

% v. reparto(+L, +N, -LListas) que tenga éxito si LListas es una lista de N listas (N ≥ 1) de cualquier longitud - incluso vacías - tales que al concatenarlas se obtiene la lista L.

reparto(_, 0, []).
reparto(L, N, [L1|Resto]) :- N > 0, append(L1, L2, L), N1 is N - 1, reparto(L2, N1, Resto).

% vi. repartoSinVacías(+L, -LListas) similar al anterior, pero ninguna de las listas de LListas puede ser vacía, y la longitud de LListas puede variar.

repartoSinVacias([], []).
repartoSinVacias(L, [L1|Resto]) :- L1 \= [], append(L1, L2, L), repartoSinVacias(L2, Resto).


% Ejercicio 8
% Definir el predicado parteQueSuma(+L,+S,-P) que es verdadero cuando P es una lista con elementos de L que suman S. 

parteQueSuma([], 0, []).
parteQueSuma([X|Xs], S, [X|P]) :- S1 is S - X, parteQueSuma(Xs, S1, P).
parteQueSuma([_|Xs], S, P) :- parteQueSuma(Xs, S, P).

% Otra forma de hacerlo:
parteQueSuma(L, S, P) :- partes(L, P), sum_list(P, S).

partes([], []).
partes([X|Xs], [X|Ps]) :- partes(Xs, Ps). 
partes([_|Xs], Ps) :- partes(Xs, Ps).  


% Ejercicio 9 ⋆
% Considerar el siguiente predicado:

desde(X, X).
desde(X, Y) :- N is X + 1, desde(N, Y).

% i. ¿Cómo deben instanciarse los parámetros para que el predicado funcione? (Es decir, para que no se cuelgue ni produzca un error). ¿Por qué?

% La instanciación de los parámetros debería ser desde(+X,-Y) ya que en el caso en el que Y esté instanciado y sea menor que X se va a colgar.

% ii. Dar una nueva versión del predicado que funcione con la instanciación desdeReversible(+X,?Y), tal que si Y está instanciada, sea verdadero si Y es mayor o igual que X, y si no lo está genere todos los Y de X en adelante.

desdeReversible(X, X).
desdeReversible(X, Y) :- var(Y), N is X + 1, desde(N, Y).
desdeReversible(X, Y) :- nonvar(Y), X < Y.


% Ejercicio 10
% Definir el predicado intercalar(L1, L2, L3), donde L3 es el resultado de intercalar uno a uno los elementos de las listas L1 y L2. Si una lista tiene longitud menor, entonces el resto de la lista más larga es pasado sin cambiar. 

intercalar([], L, L).
intercalar(L, [], L).
intercalar([X|Xs], [Y|Ys], [X,Y|Zs]) :- intercalar(Xs, Ys, Zs).


% Ejercicio 11 ⋆
% Un árbol binario se representará en Prolog con:
% nil, si es vacío.
% bin(izq, v, der), donde v es el valor del nodo, izq es el subárbol izquierdo y der es el subárbol derecho.
% Definir predicados en Prolog para las siguientes operaciones: vacío, raiz, altura y cantidadDeNodos.

vacio(nil).

raiz(bin(_, V, _), V).

altura(nil, 0).
altura(bin(Izq, _, Der), A) :- altura(Izq, AI), altura(Der, AD), A is max(AI, AD) + 1.

cantidadDeNodos(nil, 0).
cantidadDeNodos(bin(Izq, _, Der), C) :- cantidadDeNodos(Izq, CI), cantidadDeNodos(Der, CD), C is CI + CD + 1.


% Ejercicio 12 ⋆
% Definir los siguientes predicados, utilizando la representación de árbol binario definida en el ejercicio 11:

% i. inorder(+AB,-Lista), que tenga éxito si AB es un árbol binario y Lista la lista de sus nodos según el recorrido inorder.
inorder(nil, []).
inorder(bin(Izq, V, Der), Lista) :- inorder(Izq, LI), inorder(Der, LD), append(LI, [V|LD], Lista).

% ii. arbolConInorder(+Lista,-AB), versión inversa del predicado anterior.
arbolConInorder([], nil).
arbolConInorder([X|Xs], bin(Izq, X, Der)) :- append(L1, L2, Xs), arbolConInorder(L1, Izq), arbolConInorder(L2, Der).

% iii. aBB(+T), que será verdadero si T es un árbol binario de búsqueda.
aBB(nil).
aBB(bin(Izq, V, Der)) :- aBB(Izq), aBB(Der), maximo(Izq, MaxIzq), minimo(Der, MinDer),
    MaxIzq < V, V < MinDer.

maximo(nil, -inf).
maximo(bin(_, V, nil), V).
maximo(bin(_, _, Der), Max) :- maximo(Der, Max).

minimo(nil, inf).
minimo(bin(nil, V, _), V).
minimo(bin(Izq, _, _), Min) :- minimo(Izq, Min).

% iv. aBBInsertar(+X, +T1, -T2), donde T2 resulta de insertar X en orden en el árbol T1. Este predicado ¿es reversible en alguno de sus parámetros? Justificar.

aBBInsertar(X, nil, bin(nil, X, nil)).
aBBInsertar(X, bin(Izq, V, Der), bin(Izq2, V, Der)) :- X =< V, aBBInsertar(X, Izq, Izq2).
aBBInsertar(X, bin(Izq, V, Der), bin(Izq, V, Der2)) :- X > V, aBBInsertar(X, Der, Der2).

% Este predicado no es completamente reversible porque la inserción depende de la comparación.

% ===========================================================
%%                  Generate & test
% ===========================================================

% Ejercicio 13 ⋆
% Definir el predicado coprimos(-X,-Y), que genere uno a uno todos los pares de números naturales coprimos (es decir, cuyo máximo común divisor es 1), sin repetir resultados. Usar la función gcd del motor aritmético.

paresQueSuman(S,X,Y) :- S1 is S - 1, between(1,S1,X), Y is S - X.
% Dado un valor S encuentra todos los pares (X,Y) asegurando que S = X + Y.

generarPares(X,Y) :- desdeReversible(2,S), paresQueSuman(S,X, Y).
% Primero va generando valores de S con un generador infinito y luego usa paresQueSuman para cada S obtenido.

coprimos(X,Y) :- generarPares(X,Y), gcd(X,Y) =:= 1.
% Primero genera todos los pares y después filtra los pares con la condición.


% Ejercicio 14 ⋆
% i. Definir el predicado cuadradoSemiMágico(+N, -XS). El predicado debe ir devolviendo matrices (utilizando la representación antes mencionada), que sean cuadrados semi-mágicos de dimensión N*N. Dichas matrices deben devolverse de manera ordenada: primero aquellas cuyas filas suman 0, luego 1, luego 2, etc. No es necesario utilizar la técnica Generate & Test.

% Ejemplo: cuadradoSemiMágico(2,X). devuelve:
% X = [[0, 0], [0, 0]] ;
% X = [[0, 1], [0, 1]] ;
% X = [[0, 1], [1, 0]] ;
% X = [[1, 0], [0, 1]] ;
% X = [[1, 0], [1, 0]] ;
% X = [[0, 2], [0, 2]] ;
% etc.

cuadradoSemiMagico(N, XS) :- sumaTotal(S), matriz(N, N, S, XS). 

sumaTotal(0).
sumaTotal(S) :- sumaTotal(S1), S is S1 + 1.

matriz(0, _, _, []).
matriz(Filas, Columnas, Suma, [Fila|Resto]) :- 
    Filas > 0,
    fila(Columnas, Suma, Fila),
    Filas1 is Filas - 1,
    matriz(Filas1, Columnas, Suma, Resto).

fila(0, 0, []).
fila(Columnas, Suma, [X|Xs]) :-
    Columnas > 0,
    between(0, Suma, X),      
    RestoSuma is Suma - X,   
    Columnas1 is Columnas - 1,
    fila(Columnas1, RestoSuma, Xs).

% ii. Definir utilizando Generate & Test el predicado cuadradoMagico(+N, -XS), que instancia XS con cuadrados cuyas filas y columnas suman todas un mismo valor.

% No me salió 😔

% Ejercicio 15
% En este ejercicio trabajaremos con triángulos. La expresión tri(A,B,C) denotará el triángulo cuyos lados tienen
% longitudes A, B y C respectivamente. Se asume que las longitudes de los lados son siempre números naturales.
% Implementar los siguientes predicados:

% i. esTriángulo(+T) que, dada una estructura de la forma tri(A,B,C), indique si es un triángulo válido.
% En un triángulo válido, cada lado es menor que la suma de los otros dos, y mayor que su diferencia (y obviamente mayor que 0).

esTriangulo(tri(A,B,C)) :- A < B + C, B < A + C, C < A + B.

% ii. perímetro(?T,?P), que es verdadero cuando T es un triángulo (válido) y P es su perímetro. No
% se deben generar resultados repetidos (no tendremos en cuenta la congruencia entre triángulos: si
% dos triángulos tienen las mismas longitudes, pero en diferente orden, se considerarán diferentes entre sí). El predicado debe funcionar para cualquier instanciación de T y P (ambas instanciadas, ambas sin instanciar, una instanciada y una no; no es necesario que funcione para triángulos parcialmente instanciados), debe generar todos los resultados válidos (sean finitos o infinitos), y no debe colgarse (es decir, no debe seguir ejecutando infinitamente sin producir nuevos resultados). Por ejemplo:
% ?- perímetro(tri(3,4,5),12). → true.
% ?- perímetro(T,5). → T = tri(1, 2, 2) ; T = tri(2, 1, 2) ; T = tri(2, 2, 1) ; false.
% ?- perímetro(tri(2,2,2),P). → P = 6.
% ?- perímetro(T,P). → T = tri(1, 1, 1), P = 3 ; T = tri(1, 2, 2), P = 5 ; . . .

perimetro(tri(A,B,C), P) :- ground(tri(A,B,C)), esTriangulo(tri(A,B,C)), P is A + B + C.
perimetro(tri(A,B,C), P) :- not(ground(tri(A,B,C))), armarTriplas(P,A,B,C), esTriangulo(tri(A,B,C)).

% RECORDATORIO: ground verifica si un término está completamente instanciado, es decir, que no contiene ninguna variable libre dentro de su estructura.

% Versión cuadrática 
armarTriplas(P,A,B,C) :- desdeReversible(3,P), between(0,P,A), S is P - A, between(0,S,B), C is S - B. 

% Versión cúbica, puede haber repetidos y es muy ineficiente.
armarTriplas2(P,A,B,C) :- desdeReversible(3,P), between(1,P,A), between(1,P,B), between(1,P,C), P =:= A + B + C.

% En las 2 versiones genera todas las posibles combinaciones que sumen P.

% iii. triángulo(-T), que genera todos los triángulos válidos, sin repetir resultados.

triangulo(T) :- perimetro(T, _).

% ===========================================================
%%               Negación por falla y cut
% ===========================================================

% Ejercicio 16 ⋆
% A Ana le gustan los helados que sean a la vez cremosos y frutales. En una heladería de su barrio, se encontró
% con los siguientes sabores:

frutal(frutilla).
frutal(banana).
frutal(manzana).
cremoso(banana).
cremoso(americana).
cremoso(frutilla).
cremoso(dulceDeLeche).

% Ana desea comprar un cucurucho con sabores que le gustan. El cucurucho admite hasta 2 sabores. Los siguientes
% predicados definen las posibles maneras de armar el cucurucho.

leGusta(X) :- frutal(X), cremoso(X).
cucurucho(X,Y) :- leGusta(X), leGusta(Y).

% i. Escribir el árbol de búsqueda para la consulta ?- cucurucho(X,Y).

% cucurucho(X, Y) 
%    |
%   leGusta(X),             leGusta(Y)
%       |                       |
% frutal(X), cremoso(X)  frutal(Y), cremoso(Y)
%    |                      |
% X = banana        Y = banana, Y = frutilla
% X = frutilla      Y = banana, Y = frutilla
%
% % El árbol se explora combinando los sabores que son tanto frutales como cremosos
% % (frutal(banana), cremoso(banana), etc.)


% ii. Indicar qué partes del árbol se podan al colocar un ! en cada ubicación posible en las definiciones de cucurucho y leGusta.

% En leGusta(X) :- frutal(X), cremoso(X)., si colocamos un ! después de la comprobación de frutal(X) o de cremoso(X), la búsqueda se detendría si uno de estos predicados no es verdadero, y no se intentarían más soluciones para X.

% En cucurucho(X,Y) :- leGusta(X), leGusta(Y)., si colocamos un ! después de leGusta(X), el programa dejaría de intentar buscar más combinaciones de X una vez que encuentre un X válido. Lo mismo ocurriría con leGusta(Y).


% Ejercicio 17 ⋆
% i. Sean los predicados P(?X) y Q(?X), ¿qué significa la respuesta a la siguiente consulta? La consulta: ?- P(Y), not(Q(Y)).

% Busca un valor Y tal que:
% % - P(Y) sea verdadero
% % - Q(Y) sea falso (es decir, no hay ningún Y tal que Q(Y) sea verdadero).

% ii. ¿Qué pasaría si se invirtiera el orden de los literales en la consulta anterior?

% Si se invierte el orden a ?- not(Q(Y)), P(Y).
% Primero busca un Y tal que no exista Q(Y), luego verifica si P(Y) es verdadero.
% El orden puede cambiar los resultados porque Prolog evalúa los predicados en el orden dado.

% iii. Sea el predicado P(?X), ¿Cómo se puede usar el not para determinar si existe una única Y tal que P(?Y) es verdadero?

% unico(Y) :- P(Y), not((P(Y2), Y2 \= Y)).
% % Esto asegura que no haya más de un Y tal que P(Y) sea verdadero.


% Ejercicio 18

% Definir el predicado corteMásParejo(+L, -L1, -L2) que divide la lista en dos partes con la suma más parecida posible. (puede haber más de un resultado). Por ejemplo:
% ?- corteMásParejo([1,2,3,4,2],L1,L2). → L1 = [1, 2, 3], L2 = [4, 2] ; false.
% ?- corteMásParejo([1,2,1],L1,L2). → L1 = [1], L2 = [2, 1] ; L1 = [1, 2], L2 = [1] ; false.

unCorte(L,L1,L2,D) :- append(L1,L2,L), sum_list(L1,S1), sum_list(L2,S2), D is abs(S1-S2).

corteMasParejo(L,L1,L2) :- unCorte(L,L1,L2,D), not((unCorte(L,_,_,D2), D2 < D)).
% Es generate and test.

% Ejercicio 19
% Dado un predicado unario P sobre números naturales, definir un predicado que determine el mínimo X que satisfaga P(X).

% minimo(P, X) :- P(X), not((P(Y), Y < X)).

% Ejercicio 20 ⋆
% Un número poderoso es un número natural m tal que por cada número primo p que divide a m, p^2 también divide a m. Definir el predicado próximoNumPoderoso(+X,-Y) que instancie en Y el siguiente número poderoso a partir de X. Por ejemplo:
% ?- próximoNumPoderoso(20,Y).
% Y = 25;
% false.
% ?- próximoNumPoderoso(8,Y).
% Y = 9;
% false.
% Notar que, como en el último caso, si X ya es un número poderoso, Y no debe instanciarse con el valor de X, sino
% con el siguiente número poderoso.

esPrimo(2).
esPrimo(N) :- N > 2, not(tieneDivisor(N, 2)).

tieneDivisor(N, Div) :- Div * Div =< N, N mod Div =:= 0. 
tieneDivisor(N, Div) :- Div * Div =< N, Div2 is Div + 1, tieneDivisor(N, Div2).

esPoderoso(X) :- not((between(2, X, Z), esPrimo(Z), mod(X, Z) =:= 0, Z1 is Z * Z, not((mod(X, Z1) =:= 0)))).

proximoNumPoderoso(X, Y) :- Y is X + 1, esPoderoso(Y).
proximoNumPoderoso(X, Y) :- N is X + 1, not(esPoderoso(N)), proximoNumPoderoso(N, Y).


% Ejercicio 21
% Contamos con una representación de conjuntos desconocida, que permite enumerar un conjunto mediante el
% predicado pertenece(?Elemento, +Conjunto). Dado el siguiente predicado:

naturalv2(cero).
naturalv2(suc(X)) :- naturalv2(X).

% i. Definir el predicado conjuntoDeNaturales(X) que sea verdadero cuando todos los elementos de X son naturales (se asume que X es un conjunto).

conjuntoDeNaturales([]).
conjuntoDeNaturales([E|Resto]) :- naturalv2(E), conjuntoDeNaturales(Resto).

% Ejemplo:

pertenece2(E, [E|_]).
pertenece2(E, [_|Resto]) :- pertenece2(E, Resto).

% ?- pertenece2(E, [cero, suc(cero)]), conjuntoDeNaturales([cero, suc(cero)]).
% E = cero ;
% E = suc(cero) ;
% false.

% ii. ¿Con qué instanciación de X funciona bien el predicado anterior? Justificar.

% El predicado solo está diseñado para verificar listas explícitas de elementos ya dados. Cuando X no está completamente instanciado o contiene estructuras indefinidas, Prolog intenta generar infinitas soluciones, lo que puede causar loops o comportamiento inesperado.

% iii. Indicar el error en la siguiente definición alternativa, justificando por qué no funciona correctamente:
% conjuntoDeNaturalesMalo(X) :- not( (not(natural(E)), pertenece(E,X)) ).

% El error en conjuntoDeNaturalesMalo ocurre porque la variable E no está conectada directamente con los elementos del conjunto X, y Prolog intenta generar valores arbitrarios para E, causando bucles infinitos. Además, el uso de negaciones anidadas complica la lógica y no funciona bien si el conjunto está indefinido o contiene elementos no naturales.


% ===================================================
% Continuará ...
% ===================================================