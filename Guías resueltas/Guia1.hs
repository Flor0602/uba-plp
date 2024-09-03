-- Práctica N° 1 - Programación Funcional
-- Currificación y tipos
-- Ejercicio 1 - Considerar las siguientes definiciones de funciones:
-- i. ¿Cuál es el tipo de cada función? (Suponer que todos los números son de tipo Float).

max2 :: (Float, Float) -> Float
max2 (x, y) | x >= y = x
            | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial  (x, y) = sqrt (x ** 2 + y ** 2)

subtract2 :: Float -> Float -> Float
subtract2 = flip (-)

predecesor :: Float -> Float
predecesor = subtract2 1

evaluarEnCero :: (Float -> a) -> a
evaluarEnCero f = f 0

dosVeces :: (a -> a) -> a -> a
dosVeces f = f . f

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip

flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip
-- Ojo!!! No es flip . flip.
-- Ver https://stackoverflow.com/questions/54428941/haskell-applying-flip-twice-type-of-flip-flip

-- ii. Indicar cuáles de las funciones anteriores no están currificadas. Para cada una de ellas, definir la función currificada correspondiente. Recordar dar el tipo de la función.
-- Las funciones no currificadas son max2 y normaVectorial.
-- Versiones currificadas:
max2v2 :: Float -> Float -> Float
max2v2 x y | x >= y = x
           | otherwise = y

normaVectorialv2 :: Float -> Float -> Float
normaVectorialv2 x y = sqrt (x ** 2 + y ** 2)

-- Ejercicio 2 
-- i. Definir la función curry, que dada una función de dos argumentos, devuelve su equivalente currificada.

curry :: ((a,b) -> c) -> (a -> (b -> c))
curry f a b = f (a,b)

-- ii. Definir la función uncurry, que dada una función currificada de dos argumentos, devuelve su versión no currificada equivalente. Es la inversa de la anterior.

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (a,b) = f a b

-- iii. ¿Se podría definir una función curryN, que tome una función de un número arbitrario de argumentos y devuelva su versión currificada?
-- Sugerencia: pensar cuál sería el tipo de la función.
-- Si quisiéramos una función curryN que funcione para cualquier número de argumentos, nos enfrentaríamos a la limitación de que Haskell requiere que el número de argumentos de una función esté determinado en tiempo de compilación. 
-- Es decir, no se puede definir una función curryN con un tipo genérico como: curryN :: ((a, b, c, ...) -> z) -> a -> b -> c -> ... -> z

-- Esquemas de recursion
-- Ejercicio 3
-- i. Redefinir usando foldr las funciones sum, elem, (++), filter y map.
sum' :: [Int] -> Int
sum' = sum

elem' :: Eq a => a -> [a] -> Bool
elem' n = foldr (\x rec -> x == n || rec) False


filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x rec -> if p x then x:rec else rec) []

map' :: (a -> b) -> [a] -> [b]
map' f = map (\x -> f x)

-- ii. Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento de la lista según una función de comparación, utilizando foldr1. 
-- Por ejemplo, maximum = mejorSegún (>).

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

-- iii. Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original desde la cabeza hasta la posición actual. 
-- Por ejemplo, sumasParciales [1,4,-1,0,5] -> [1,5,4,4,9].

sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = sumaAcc xs 0

sumaAcc :: Num a => [a] -> a -> [a]
sumaAcc [] _ = []
sumaAcc (x:xs) acc = (x + acc) : sumaAcc xs (x + acc)

-- iv. Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.

sumaAlt :: [Int] -> Int
sumaAlt = foldr (-) 0

-- v. Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo, etc.). Pensar qué esquema de recursión conviene usar en este caso.

sumaAltreverso :: [Int] -> Int
sumaAltreverso = foldl (flip (-)) 0

-- Ejercicio 4
-- i. Definir la función permutaciones :: [a] -> [[a]], que dada una lista devuelve todas sus permutaciones. 
-- Se recomienda utilizar concatMap :: (a -> [b]) -> [a] -> [b], y también take y drop.



-- ii. Definir la función partes, que recibe una lista L y devuelve la lista de todas las listas formadas por los mismos elementos de L, en su mismo orden de aparición.
-- Ejemplo: partes [5, 1, 2] → [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]](en algún orden).



-- iii. Definir la función prefijos, que dada una lista, devuelve todos sus prefijos.
-- Ejemplo: prefijos [5, 1, 2] → [[], [5], [5, 1], [5, 1, 2]]



-- iv. Definir la función sublistas que, dada una lista, devuelve todas sus sublistas (listas de elementos que aparecen consecutivos en la lista original).
-- Ejemplo: sublistas [5, 1, 2] → [[], [5], [1], [2], [5, 1], [1, 2], [5, 1, 2]] (en algún orden).

{-
Ejercicio 5 ⋆
Considerar las siguientes funciones.
Indicar si la recursión utilizada en cada una de ellas es o no estructural. Si lo es, reescribirla utilizando foldr.
En caso contrario, explicar el motivo.

5a. No es recursion estructural porque utiliza xs en el if y al final con tail xs,
    lo que no garantiza que esté operando directamente sobre la estructura de la lista de forma uniforme en cada paso.
-}

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs then [x] else x : elementosEnPosicionesPares (tail xs)

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys then x : entrelazar xs [] else x : head ys : entrelazar xs (tail ys)

-- Solucion con foldr:
entrelazar2 :: [a] -> [a] -> [a]
entrelazar2 = foldr (\ x rec ys -> if null ys then x : rec [] else x : head ys : rec (tail ys)) id

-- Ejercicio 6 ⋆
-- El siguiente esquema captura la recursión primitiva sobre listas.
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)
-- a. Definir la función sacarUna :: Eq a => a -> [a] -> [a], que dados un elemento y una lista devuelve el resultado de eliminar de la lista la primera aparición del elemento (si está presente).

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna y = recr (\x xs rec -> if x == y then xs else x: rec) []

-- b. Explicar por qué el esquema de recursión estructural (foldr) no es adecuado para implementar la función sacarUna del punto anterior.

-- Porque quiero trabajar con xs, con foldr o foldl no tengo forma de referenciar a xs.

-- c. Definr la función insertarOrdenado :: Ord a => a -> [a] -> [a] que inserta un elemento en una lista ordenada (de manera creciente), de manera que se preserva el ordenamiento.
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado elemento = recr (\x xs rec -> if elemento < x then elemento:x:xs else x: rec) []

{-
Ejercicio 7
i. Definir la función genLista :: a -> (a -> a) -> Integer -> [a], que genera una lista de una cantidad dada de elementos, a partir de un elemento inicial y de una función de incremento entre los elementos
de la lista. Dicha función de incremento, dado un elemento de la lista, devuelve el elemento siguiente.
-}
genLista :: a -> (a -> a) -> Int -> [a]
genLista _ _ 0           = []
genLista inicio sig cont = inicio : genLista (sig inicio) sig (cont-1)

{-
ii. Usando genLista, definir la función desdeHasta, que dado un par de números (el primero menor que el
segundo), devuelve una lista de números consecutivos desde el primero hasta el segundo.
-}

desdeHasta :: Int -> Int -> [Int]
desdeHasta i j = genLista i (+1) (j-i-1)

{-
Ejercicio 8 ⋆
Definir las siguientes funciones para trabajar sobre listas, y dar su tipo. Todas ellas deben poder aplicarse a listas finitas e infinitas.

i. mapPares, una versión de map que toma una función currificada de dos argumentos y una lista de pares
de valores, y devuelve la lista de aplicaciones de la función a cada par. Pista: recordar curry y uncurry.

ii. armarPares, que dadas dos listas arma una lista de pares que contiene, en cada posición, el elemento correspondiente a esa posición en cada una de las listas. Si una de las listas es más larga que la otra,
ignorar los elementos que sobran (el resultado tendrá la longitud de la lista más corta). Esta función en Haskell se llama zip. Pista: aprovechar la currificación y utilizar evaluación parcial.

iii. mapDoble, una variante de mapPares, que toma una función currificada de dos argumentos y dos listas
(de igual longitud), y devuelve una lista de aplicaciones de la función a cada elemento correspondiente de
las dos listas. Esta función en Haskell se llama zipWith.
-}

mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f =  map (uncurry' f)

armarPares :: [a] -> [b] -> [(a,b)]
armarPares _ []          = []
armarPares [] _          = []
armarPares (x:xs) (y:ys) = (x, y) : armarPares xs ys

armarPares2 :: [a] -> [b] -> [(a,b)]
armarPares2 = foldr (\ x rec ys -> if null ys then [] else (x, head ys) : rec (tail ys)) (const [])

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
-- mapDoble f (x:xs) (y:ys) = f x y : mapDoble f xs ys
mapDoble f = foldr (\ x rec ys -> if null ys then [] else f x (head ys) : rec (tail ys)) (const [])

{-
Ejercicio 9
i. Escribir la función sumaMat, que representa la suma de matrices, usando zipWith. Representaremos una
matriz como la lista de sus filas. Esto quiere decir que cada matriz será una lista finita de listas finitas,
todas de la misma longitud, con elementos enteros. Recordamos que la suma de matrices se define como
la suma celda a celda. Asumir que las dos matrices a sumar están bien formadas y tienen las mismas
dimensiones.
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]

ii. Escribir la función trasponer, que, dada una matriz como las del ítem i, devuelva su traspuesta. Es decir,
en la posición i, j del resultado está el contenido de la posición j, i de la matriz original. Notar que si la
entrada es una lista de N listas, todas de longitud M, la salida debe tener M listas, todas de longitud N.
trasponer :: [[Int]] -> [[Int]]
-}

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (zipWith (+))


-- Otras estructuras de datos
{-
En esta sección se permite (y se espera) el uso de recursión explícita únicamente para la definición de esquemas
de recursión.

Ejercicio 10 ⋆
i. Definir y dar el tipo del esquema de recursión foldNat sobre los naturales. Utilizar el tipo Integer de Haskell (la función va a estar definida sólo para los enteros mayores o iguales que 0).

ii. Utilizando foldNat, definir la función potencia.
-}

foldNat :: b -> (b -> b) -> Integer -> b
foldNat cZero cSuc n
    | n == 0    = cZero
    | otherwise = cSuc (foldNat cZero cSuc (n - 1))

potencia :: Integer -> Integer -> Integer
potencia n = foldNat 1 (* n) -- Es n^k

{-
Ejercicio 11
Definir el esquema de recursión estructural para el siguiente tipo:
Luego usar el esquema definido para escribir la función evaluar :: Num a => a -> Polinomio a -> a
que, dado un número y un polinomio, devuelve el resultado de evaluar el polinomio dado en el número dado.
-}

data Polinomio a = X | Cte a | Suma (Polinomio a) (Polinomio a) | Prod (Polinomio a) (Polinomio a)
evaluar :: Num a => a -> Polinomio a -> a
evaluar n X = n
evaluar _ (Cte k) = k
evaluar n (Suma p q) = evaluar n p + evaluar n q
evaluar n (Prod p q) = evaluar n p * evaluar n q

foldPoli :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli cX cCte cSuma cProd poli = case poli of
    X -> cX
    Cte c -> cCte c
    Suma p q -> cSuma (rec p) (rec q)
    Prod p q -> cProd (rec p) (rec q)
    where rec = foldPoli cX cCte cSuma cProd

evaluar2 :: Num a => a -> Polinomio a -> a
evaluar2 n = foldPoli n id (+) (-)

{-
Ejercicio 12 ⋆
Considerar el siguiente tipo, que representa a los árboles binarios:
data AB a = Nil | Bin (AB a) a (AB a)
i. Usando recursión explícita, definir los esquemas de recursión estructural (foldAB) y primitiva (recAB), y
dar sus tipos.
ii. Definir las funciones esNil, altura y cantNodos (para esNil puede utilizarse case en lugar de foldAB
o recAB).
iii. Definir la función mejorSegún :: (a -> a -> Bool) -> AB a -> a, análoga a la del ejercicio 3, para árboles.
Se recomienda definir una función auxiliar para comparar la raíz con un posible resultado de la recursión
para un árbol que puede o no ser Nil.
iv. Definir la función esABB :: Ord a => AB a -> Bool que chequea si un árbol es un árbol binario de búsqueda.
Recordar que, en un árbol binario de búsqueda, el valor de un nodo es mayor o igual que los valores que
aparecen en el subárbol izquierdo y es estrictamente menor que los valores que aparecen en el subárbol
derecho.
v. Justificar la elección de los esquemas de recursión utilizados para los tres puntos anteriores.
-}

data AB a = Nil | Bin (AB a) a (AB a) deriving (Show, Eq)
foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin arbol = case arbol of
    Nil -> cNil
    Bin i r d -> cBin (rec i) r (rec d)
    where rec = foldAB cNil cBin

recAB :: b -> (AB a -> a -> AB a -> b -> b -> b) -> AB a -> b
recAB cNil cBin arbol = case arbol of
    Nil -> cNil
    Bin i r d -> cBin i r d (rec i) (rec d)
    where rec = recAB cNil cBin

esNil :: AB a -> Bool
esNil arbol = case arbol of
    Nil -> True
    _   -> False

altura :: AB a -> Int
altura = foldAB 0 (\ri _ rd -> 1+ max ri rd)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\ri _ rd -> 1+ ri + rd)

mejorSegún :: (a -> a -> Bool) -> AB a -> a
mejorSegún _ Nil = undefined
mejorSegún f (Bin i r d) = foldAB r (\ri _ rd -> mejor (mejor r ri) rd) (Bin i r d)
    where mejor x y = if f x y then x else y

esABB :: Ord a => AB a -> Bool
esABB = recAB True (\i r d ri rd -> ri && rd && menorARaiz2 r i && mayorARaiz2 r d)

menorARaiz2 :: Ord a => a ->  AB a -> Bool
menorARaiz2 r = foldAB False (\ri n rd -> r > n && ri && rd)

mayorARaiz2 :: Ord a => a ->  AB a -> Bool
mayorARaiz2 r = foldAB False (\ri n rd -> r < n && ri && rd)

-- En el ii. utilizo foldAB sabiendo que voy a utilizar recursion estructural con la raiz y la recursion sobre el subarbol izq y der, no hace falta hacer otra cosa mas.
-- En el iii. tambien.
-- En el iv. utilizo recAB ya que necesito asegurarme que los arboles i y d son ABB utilizando ri y rd, pero tambien tengo que comprobar en el arbol original sea ABB utilizando los arboles originales i y d, 
-- por lo que utilizo recursion primitiva para trabajar con la raiz y los arboles i y d.

{-
Ejercicio 13
Dado el tipo AB a del ejercicio 12:
i. Definir las funciones ramas (caminos desde la raíz hasta las hojas), cantHojas y espejo.
ii. Definir la función mismaEstructura :: AB a -> AB b -> Bool que, dados dos árboles, indica si éstos
tienen la misma forma, independientemente del contenido de sus nodos. Pista: usar evaluación parcial y
recordar el ejercicio 8.
-}

ramas :: AB a -> [[a]]
ramas = foldAB [] (\ri r rd -> if null ri && null rd then [[r]] else map (r:) (ri ++ rd))

cantHojas :: AB a -> Int
cantHojas = foldAB 0 (\ri _ rd -> if ri == 0 && rd == 0 then 1 else 0 + ri + rd)

espejo :: AB a -> AB a
espejo = foldAB Nil (\ri r rd -> Bin rd r ri)

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura Nil Nil = True
mismaEstructura Nil _ =  False
mismaEstructura _ Nil = False
mismaEstructura (Bin i _ d) (Bin l _ r) = mismaEstructura i l && mismaEstructura d r

mismaEstructura2 :: AB a -> AB b -> Bool
mismaEstructura2 = recAB esNil (\ i _ d ri rd arb -> case arb of
    Nil -> False
    (Bin y1 _ y2) -> ri y1 && rd y2 && esNil i == esNil y1 && esNil d == esNil y2)

{-
Ejercicio 14
Se desea modelar en Haskell los árboles con información en las hojas (y sólo en ellas). Para esto introduciremos
el siguiente tipo:
data AIH a = Hoja a | Bin (AIH a) (AIH a)
a) Definir el esquema de recursión estructural foldAIH y dar su tipo. Por tratarse del primer esquema de
recursión que tenemos para este tipo, se permite usar recursión explícita.
b) Escribir las funciones altura :: AIH a -> Integer y tamaño :: AIH a -> Integer.
Considerar que la altura de una hoja es 1 y el tamaño de un AIH es su cantidad de hojas
-}

data AIH a = Hoja a | Bin2 (AIH a) (AIH a)
foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH cHoja cBin2 arbol = case arbol of
    Hoja a -> cHoja a
    (Bin2 i d) -> cBin2 (rec i) (rec d)
    where rec = foldAIH cHoja cBin2

altura2 :: AIH a -> Integer
altura2 = foldAIH (const 1) (\ri rd -> 1 + max ri rd)

tamaño :: AIH a -> Integer
tamaño = foldAIH (const 1) (\ri rd -> ri + rd)

{-
Ejercicio 15 ⋆
i. Definir el tipo RoseTree de árboles no vacíos, con una cantidad indeterminada de hijos para cada nodo.
ii. Escribir el esquema de recursión estructural para RoseTree. Importante escribir primero su tipo.
iii. Usando el esquema definido, escribir las siguientes funciones:
a) hojas, que dado un RoseTree, devuelva una lista con sus hojas ordenadas de izquierda a derecha,
según su aparición en el RoseTree.
b) distancias, que dado un RoseTree, devuelva las distancias de su raíz a cada una de sus hojas.
c) altura, que devuelve la altura de un RoseTree (la cantidad de nodos de la rama más larga). Si el
RoseTree es una hoja, se considera que su altura es 1.
-}

data RoseTree a = Rose a [RoseTree a]
foldRT :: (a -> [b] -> b) -> RoseTree a -> b
foldRT f (Rose x hijos) = f x (map (foldRT f) hijos)

hojas :: RoseTree a -> [a]
hojas = foldRT (\x recs -> if null recs then [x] else concat recs)

altura3 :: RoseTree a -> Int
altura3 = foldRT (\_ recs -> if null recs then 1 else 1 + maximum recs)

tamaño2 :: RoseTree a -> Int
tamaño2 (Rose _ hijos) = 1 + sum (map tamaño2 hijos)

tamañoConfold :: RoseTree a -> Int
tamañoConfold = foldRT (\_ recs -> 1+ sum recs)

-- Ejercicios opcionales
data HashSet a = Hash (a -> Integer) (Integer -> [a])
vacío :: (a -> Integer) -> HashSet a
vacío f = Hash f (const [])

pertenece :: Eq a => a -> HashSet a -> Bool
pertenece e (Hash f tabla) = e `elem` tabla (f e)
-- f e = indice de la tabla de Hash

agregar :: Eq a => a -> HashSet a -> HashSet a
agregar e (Hash f tabla) = Hash f nuevaTabla
    where indice = f e -- Calcula el indice del elemento
          nuevaTabla i | i == indice = if pertenece e (Hash f tabla) then tabla i else e : tabla i
          -- Si el indice i == indice del elemento i, si ya pertenece lo deja como esta, sino se lo agrego al principio
                       | otherwise = tabla i
                       -- Si el indice i != indice del elemento i, devuelvo la tabla como estaba antes

intersección :: Eq a => HashSet a -> HashSet a -> HashSet a
intersección (Hash f tabla) (Hash _ tabla2) = Hash f nuevaTabla
  where nuevaTabla i = filter (\x -> x `elem` tabla i) (tabla2 i)

foldr1' :: (a -> b -> b) -> [a] -> b
foldr1' f = foldr (\x rec -> f x rec) undefined
-- Si pongo error, me tira que no me matchean los tipos

-- Generacion infinita
paresDeNat :: [(Int,Int)]
paresDeNat = [(x, y) | s <- [0..], x <- [0..s], y <- [0..s], x + y == s]

-- Esa funcion no es util ya que intenta buscar todas las combinaciones posibles de a, b y c tq cumplen con la prop de Pitagoras, lo cual lo hace muy ineficiente.
-- Ademas por las 3 leyes de generacion infinita, deberia haber solo un generador infinito.
-- La lista trata de generar triplas infinitas dentro de otras triplas infinitas, lo cual es computacionalmente imposible y no converge en resultados útiles
pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(a, b, c) | c <- [1..], a <- [1..c-1], b <- [1..c-1], a * a + b * b == c * c]

listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listasQueSuman n | n > 0 = [x:xs | x <- [1..], xs <- listasQueSuman (n-x)]
                 | otherwise = []

