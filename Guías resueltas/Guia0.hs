-- ============================ PRE-PRÁCTICA ===========================
--             🌟🌟🌟 REPASO DE PROGRAMACIÓN FUNCIONAL 🌟🌟🌟
-- ====================================================================

-- Ejercicio 1 : Dar el tipo y describir el comportamiento de 
-- las funciones del módulo Prelude de Haskell.

null :: Foldable t => t a -> Bool
null :: [a] -> Bool
-- Se pregunta si [a] es vacía o no.

head :: [a] -> a
-- Es el primer elemento de la lista.

tail :: [a] -> [a]
-- Es la lista original sin su primer elemento.

init :: [a] -> [a]
-- Devuelve la lista original eliminando su último elemento.

last :: [a] -> a
-- Es el último elemento de la lista.

take :: Int -> [a] -> [a]
-- Devuelve la lista de los primeros n elementos de la lista original.

drop :: Int -> [a] -> [a]
-- Borra los primeros n elementos de la lista original.

(++) :: [a] -> [a] -> [a]
-- Concatena/junta dos listas en una.

concat :: Foldable t => t [a] -> [a]
concat :: [[a]] -> [a]
-- Junta sublistas en una sola.

(!!) :: [a] -> Int -> a
-- Es el n-ésimo elemento de la lista.

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem :: a -> [a] -> Bool
-- Se pregunta si el elemento a pertenece a la lista dada.

-- Ejercicio 2 : Definir las funciones.
-- a. 
valorAbsoluto :: Float -> Float
valorAbsoluto n | n < 0     = abs n
                | otherwise = n

-- b.
bisiesto :: Int -> Bool
bisiesto x = (x mod 4 == 0) && (x mod 100 != 0) || (x mod 400 == 0)

-- c.
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

-- d.
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos x = contarDivPrimos (x 1 0)
    where
        contarDivPrimos x div conteo | div > x                       = conteo
                                     | x mod div == 0 && esPrimo div = contarDivPrimos (x div+1 conteo+1)
                                     | otherwise                     = contarDivPrimos (x div+1 conteo)
esPrimo :: Int -> Bool
esPrimo x | x <= 1    = False
          | otherwise = not (esDivisible n 2)
    where esDivisible x y | y >= x         = False
                          | x `mod` y == 0 = True
                          | otherwise      = esDivisible x (y+1)

-- Ejercicio 3 : Contamos con los tipos Maybe y Either definidos así:
data Maybe a = Nothing | Just a
data Either a b = Left a | Right a

-- a. 
inverso :: Float -> Maybe Float
inverso x | x != 0    = Just (1/x)
          | otherwise = Nothing

-- b.
aEntero :: Either Int Bool -> Int
aEntero Left x  = x
aEntero Right y = if y then 1 else 0

-- Ejercicio 4 : Definir las funciones sobre listas.
-- a. 
limpiar :: String -> String -> String
limpiar [] n      = n
limpiar (x:xs) ys = limpiar xs (eliminar x ys)

eliminar :: Char -> String -> String
eliminar _ [] = []
eliminar x (y:ys) = if x == y then eliminar x ys else y : eliminar x ys

-- b. 
difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio xs = head xs - promedio xs : difPromedio tail xs 

promedio :: [Float] -> Float
promedio [] = 0
promedio xs = sum xs / fromIntegral (length xs)


-- c. 
todosIguales :: [Int] -> Bool
todosIguales []       = True
todosIguales [x]      = True
todosIguales (x:y:xs) = x == y && todosIguales (y:xs)

-- Ejercicio 5 : Dado el modelo para árboles binarios definir las funciones.
data AB a = Nil | Bin (AB a) a (AB a)
--          Nulo| Bin izq   raíz der

-- a.
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _   = False

-- b.
negacionAB :: AB Bool -> AB Bool
negacionAB Nil       = Nil
negacionAB Bin i r d = Bin (negacionAB i) (not r) (negacionAB d)

-- c. 
productoAB :: AB Int -> Int
productoAB Nil       = 1
productoAB Bin i r d = productoAB i * r * productoAB d

