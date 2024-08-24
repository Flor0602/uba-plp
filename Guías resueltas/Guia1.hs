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
evaluarEnCero = \f -> f 0

dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f

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

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (a,b) = f a b

-- iii. ¿Se podría definir una función curryN, que tome una función de un número arbitrario de argumentos y devuelva su versión currificada?
-- Sugerencia: pensar cuál sería el tipo de la función.
-- Si quisiéramos una función curryN que funcione para cualquier número de argumentos, nos enfrentaríamos a la limitación de que Haskell requiere que el número de argumentos de una función esté determinado en tiempo de compilación. 
-- Es decir, no se puede definir una función curryN con un tipo genérico como: curryN :: ((a, b, c, ...) -> z) -> a -> b -> c -> ... -> z

-- Esquemas de recursion
-- Ejercicio 3
-- i. Redefinir usando foldr las funciones sum, elem, (++), filter y map.
sum :: Num a => [a] -> a
sum = foldr (+) 0 

elem :: Eq a => a -> [a] -> Bool
elem n = foldr (\x acc -> (n == x) || acc) False

(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) ys xs

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x ac -> if p x then x : ac else ac) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x ac -> f x : ac) []

-- ii. Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento de la lista según una función de comparación, utilizando foldr1. 
-- Por ejemplo, maximum = mejorSegún (>).


-- iii. Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original desde la cabeza hasta la posición actual. 
-- Por ejemplo, sumasParciales [1,4,-1,0,5] -> [1,5,4,4,9].
sumasParciales :: Num a => [a] -> [a]
sumasParciales = sumAux 0 
    where sumAux _ []       = []
          sumAux acc (x:xs) = (x + acc) : sumAux (x + acc) xs

-- iv. Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.


-- v. Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo, etc.). Pensar qué esquema de recursión conviene usar en este caso.


-- Ejercicio 4
-- i. Definir la función permutaciones :: [a] -> [[a]], que dada una lista devuelve todas sus permutaciones. 
-- Se recomienda utilizar concatMap :: (a -> [b]) -> [a] -> [b], y también take y drop.
