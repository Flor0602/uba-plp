# Práctica 2 - Razonamiento ecuacional e inducción estructural

## Extensionalidad

- [Ejercicio 1](#ejercicio-1)
- [Ejercicio 2](#ejercicio-2)
## Inducción sobre listas

- [Ejercicio 3](#ejercicio-3)
- [Ejercicio 4 falta hacer](#ejercicio-4)
- [Ejercicio 5](#ejercicio-5)
- [Ejercicio 6 iv terminar](#ejercicio-6)
- [Ejercicio 7 falta hacer](#ejercicio-7)
## Otras estructuras de datos

- [Ejercicio 8 falta hacer](#ejercicio-8)
- [Ejercicio 9](#ejercicio-9)
- [Ejercicio 10 falta hacer](#ejercicio-10)
- [Ejercicio 11 falta hacer](#ejercicio-11)
- [Ejercicio 12](#ejercicio-12)



### Ejercicio 1
Sean las siguientes definiciones de funciones:
```haskell
intercambiar (x,y) = (y,x)

espejar (Left x)  = Right x
espejar (Right x) = Left x

asociarI (x,(y,z)) = ((x,y),z)
asociarD ((x,y),z)) = (x,(y,z))

flip f x y = f y x

curry f x y = f (x,y)
uncurry f (x,y) = f x y
```
Demostrar las siguientes igualdades usando los principios de extensionalidad cuando sea necesario.
#### I. ∀ p : : (a, b) . intercambiar (intercambiar p) = p
```
Por el principio de extensionalidad de pares, basta probar que:  
∀p :: (a, b). ∃x :: a. ∃y :: b. p = (x, y), intercambiar (intercambiar (x, y)) = (x, y)  

intercambiar (intercambiar (x,y))
= intercambiar (y,x)                (def intercambiar)
= (x,y)                             (def intercambiar)
```


#### II. ∀ p : : (a, (b, c)) . asociarD (asociarI p) = p
```
Por el principio de extensionalidad de pares, basta probar que:
∀p :: (a,(b,c)). ∃x :: a. ∃w :: (b,c). p = (x,w).
asociarD (asociarI (x,w)) = (x,w)

Por el principio de extensionalidad de pares, basta probar que:
∀p :: (a,(b,c)).
∃x :: a. ∃w :: (b,c). p = (x,w).
∃y :: b. ∃z :: c. w = (y,z). p = (x,(y,z)).
asociarD (asociarI (x,(y,z))) = (x,(y,z))

asociarD (asociarI (x,(y,z)))
= asociarD ((x,y),z)            (def asociarI)
= (x,(y,z))                     (def asociarD)
```

#### III. ∀ p : : Either a b . espejar (espejar p) = p
```
Por principio de extensionalidad para sumas basta ver que:
∀p :: Either a b
o bien ∃x :: a. p = Left x
o bien ∃y :: b. p = Right y
espejar (espejar p) = p

Caso p = Left x:
espejar (espejar (Left x))
= espejar (Right x)             (def espejar)
= Left x                        (def espejar)

Caso p = Right x:
espejar (espejar (Right x))
= espejar (Left x)              (def espejar)
= Right x                       (def espejar)
```

#### IV. ∀ f : : a -> b -> c . ∀ x : : a . ∀ y : : b . flip (flip f) x y = f x y
```
flip (flip f) x y
= (flip f) y x      (def flip)
= flip f y x        (la aplicación asocia a izquierda)
= f x y             (def flip)
```
#### V. ∀ f : : a -> b -> c . ∀ x : : a . ∀ y : : b . curry (uncurry f) x y = f x y
```
curry (uncurry f) x y
= uncurry f (x,y)       {def curry}
= f x y                 {def uncurry}
```

[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 2
Demostrar las siguientes igualdades utilizando el principio de extensionalidad funcional.
##### Principio de Extensionalidad Funcional

Dos funciones \( f \) y \( g \) son iguales si, y solo si, producen el mismo resultado para todas las entradas:

$$
f = g \Leftrightarrow \forall x \ (f(x) = g(x))
$$

Esto significa que, si para cualquier valor de \( x \) las dos funciones producen el mismo resultado, entonces las funciones son iguales.

#### I. flip . flip = id
```
Por el principio basta ver que:
∀f :: a -> b -> c. ∀x :: a. ∀y :: b.
(flip . flip) f x y = id f x y

(flip . flip) f x y
= flip (flip f x y)     {def .}
= flip f y x            {def flip}
= f x y                 {def flip}
= id f x y              {def id}
```
#### II. ∀ f : : (a, b) -> c . uncurry (curry f) = f
```
Por el principio basta ver que:
∀ f::(a,b)->c, ∀p :: (a,b). uncurry (curry f) p = f p

Por el principio de extensionalidad de pares tengo que probar:
∀ f::(a,b)->c, ∀p :: (a,b), ∃x :: a. ∃y :: b. p = (x, y). uncurry (curry f) (x, y) = f (x, y)

uncurry (curry f) (x,y)
= curry f x y               {def uncurry}
= f (x,y)                   {def curry}
```

#### III. flip const = const id
```
Por el principio basta ver que:
∀x :: a. ∀y :: b. flip const x y = const id x y

flip const x y
= const y x         {def flip}
= x                 {def const}
= id x              {def id}
= const id x y      {def const}
```

#### IV. ∀ f : : a -> b . ∀ g : : b -> c . ∀ h : : c -> d . ((h . g) . f) = (h . (g . f)) con la definición usual de la composición: (.) f g x = f (g x).
```
Por principio de extensionalidad funcional basta ver que:
∀f :: a -> b. ∀g :: b -> c. ∀h :: c -> d. ∀x :: a.  ((h . g) . f) x = (h . (g . f)) x

((h . g) . f) x
= (h . g) (f x)         (def .)
= h (g (f x))           (def .)
= h ((g . f) x)         (def .)
= (h . (g . f)) x       (def .)
```

[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 3
Considerar las siguientes funciones y demostrar las propiedades.
```haskell
     length :: [a] -> Int
{L0} length [] = 0
{L1} length (x:xs) = 1 + length xs

     duplicar :: [a] -> [a]
{D0} duplicar [] = []
{D1} duplicar (x:xs) = x : x : duplicar xs

     append :: [a] -> [a] -> [a]
{A0} append [] ys = ys
{A1} append (x:xs) ys = x : append xs ys

     (++) :: [a] -> [a] -> [a]
{++} xs ++ ys = foldr (:) ys xs

     ponerAlFinal :: a -> [a] -> [a]
{P0} ponerAlFinal x = foldr (:) (x:[])

     reverse :: [a] -> [a]
{R0} reverse = foldl (flip (:)) []
```
**Nota**: en adelante, siempre que se necesite usar reverse, se podrá utilizar cualquiera de las dos definiciones, según se considere conveniente.

#### I. ∀ xs::[a] . length (duplicar xs) = 2 * length xs
```
Por inducción de listas sobre xs necesito probar que:
P(xs): length (duplicar xs) = 2 * length xs

Caso base: P([])

length (duplicar [])
= length []                         (D0)
= 0                                 (L0)
= 2 * 0
= 2 * length []                     (L0)

Caso recursivo: ∀x :: a. ∀xs :: [a]. P(xs) ⇒ P(x:xs)
H.I.: length (duplicar xs) = 2 * length xs
Qvq length (duplicar (x : xs)) = 2 * length (x:xs)  

length (duplicar (x : xs)) 
= length (x : x : duplicar xs)      (D1)
= 1 + length (x : duplicar xs)      (L1)
= 1 + 1 + length (duplicar xs)      (L1)
= 2 + 2 * length xs                 (HI)
= 2 * (1 + length xs)
= 2 * length (x:xs)                 (L1)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

#### II. ∀ xs::[a] . ∀ ys::[a] . length (append xs ys) = length xs + length ys
```
Por inducción de listas sobre xs necesito probar que:
P(xs): ∀ ys::[a]. length (append xs ys) = length xs + length ys

Caso base: P([])

length (append [] ys)
= length ys                         (A0)
= 0 + length ys
= length [] + length ys             (LO)

Caso recursivo: ∀x :: a. ∀xs :: [a]. P(xs) ⇒ P(x:xs)
H.I.: ∀ ys::[a]. length (append xs ys) = length xs + length ys
Qvq ∀ ys::[a]. length (append (x:xs) ys) = length (x:xs) + length ys 

length (append (x:xs) ys)
= length (x : append xs ys)          (A1)
= 1 + length (append xs ys)
= 1 + length xs + length ys          (HI)
= length (x:xs) + length ys          (L1)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```


#### III. ∀ xs : : [a] . ∀ x::a . [x] ++ xs = x:xs

```
Por inducción de listas sobre xs necesito probar que:
P(xs): ∀ x::a. [x] ++ xs = x:xs

Caso base: P([])

[x] ++ []
= foldr (:) [] [x]                    (++)
= [x]                                 (def foldr)
= x : []

Caso recursivo: ∀y :: a. ∀xs :: [a]. P(xs) ⇒ P(y:xs)
H.I.: [x] ++ xs = x:xs
Qvq ∀y :: a. [x] ++ (y:xs) = x:(y:xs)

[x] ++ (y:xs)
= foldr (:) (y:xs) [x]               (++)
= (:) x (foldr (:) (y:xs) [])        (def foldr)
= (:) x (y:xs)                       (def foldr)
= x : (y:xs) 

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```


#### IV. ∀ xs::[a] . ∀ f::(a->b) . length (map f xs) = length xs
```
Por inducción de listas sobre xs necesito probar que:
P(xs): ∀ f::(a -> b) . length (map f xs) = length xs

Caso base: P([])

length (map f [])
= length []                          (def map)
= 0                                  (LO)
= length []                          (LO)

Caso recursivo: ∀x :: a. ∀xs :: [a]. P(xs) ⇒ P(x:xs)
H.I.: ∀ f::(a -> b) . length (map f xs) = length xs
Qvq ∀ f::(a -> b) . length (map f (x:xs)) = length (x:xs)

length (map f (x:xs))
= length (f x : map f xs)            (def map)
= 1 + length (map f xs)              (L1)
= 1 + length xs                      (HI)
= length (x:xs)                      (L1)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```


#### V. ∀ xs::[a] . ∀ p::a->Bool . ∀ e::a . ((elem e (filter p xs)) ⇒ (elem e xs)) (asumiendo Eq a)
```
Por inducción de listas sobre xs necesito probar que:
P(xs): ∀ p::a->Bool . ∀ e::a . ((elem e (filter p xs)) ⇒ (elem e xs)) (asumiendo Eq a)

Caso base: P([])

(elem e (filter p []))
= elem e []                          (def filter)
= False
= False => (elem e xs)               (def =>)

Caso recursivo: ∀x :: a. ∀xs :: [a]. P(xs) ⇒ P(x:xs)
H.I.: ∀ p::a->Bool . ∀ e::a . ((elem e (filter p xs)) ⇒ (elem e xs)) (asumiendo Eq a)
Qvq ∀ p::a->Bool . ∀ e::a . ((elem e (filter p (x:xs))) ⇒ (elem e (x:xs))) (asumiendo Eq a)

elem e (filter p (x:xs))
= elem e (if p x then x:filter p xs else filter p xs)     (def filter)

Por extensionalidad de booleanos, analizo los casos por separado:
1. Si p x = False

elem e (filter p xs)
= (elem e (filter p xs)) ⇒ (elem e xs) (HI)
Si elem e (filter p xs) = False entonces False implica cualquier cosa.
Si elem e (filter p xs) = True significa que e se encuentra en filter p xs ⊆ xs ⊆ x:xs.

2. Si p x = True

elem e (x:filter p xs)
= (e == x) || elem e (filter p xs)

Si e == x = True
True || elem e (filter p xs)
= True                                  (def ||)
Qvq True ⇒ (elem e x:xs) para que valga P(x:xs).
Basta ver: elem e x:xs = True sabiendo que e == x

elem e x:xs = True
= (e == x || elem e xs) = True                                      (def elem)
= (True || elem e xs) = True                                        (e == x)
= True = True                                                       (def ||)
= True

Si e == x = False
False || elem e (filter p xs)
= elem e (filter p xs)                             (def ||)
= (elem e (filter p xs)) ⇒ (elem e xs)            (HI)
= (elem e (filter p xs) = True) ⇒ (elem e x:xs)   (por caso 1)


Como ya lo probé para todo los casos posibles. Por lo tanto, la propiedad vale para ∀ xs :: [a].
```


#### VI. ∀ xs::[a] . ∀ x::a . ponerAlFinal x xs = xs ++ (x:[])
```
Por inducción de listas sobre xs necesito probar que:
P(xs): ∀ x::a . ponerAlFinal x xs = xs ++ (x:[])

Caso base: P([])

ponerAlFinal x []
= foldr (:) (x:[]) []                    (P0)
= x:[]                                   (def foldr)
= foldr (:) (x:[]) []                    (++)
= [] ++ (x:[])                           (def foldr)

Caso recursivo: ∀y :: a. ∀xs :: [a]. P(xs) ⇒ P(y:xs)
H.I.: ∀ x::a . ponerAlFinal x xs = xs ++ (x:[])
Qvq ∀ x::a . ponerAlFinal x (y:xs) = (y:xs) ++ (x:[])

ponerAlFinal x (y:xs)
= foldr (:) (x:[]) (y:xs)               (P0)
= (:) y (foldr (:) (x:[]) xs)           (def foldr)
= (:) y (ponerAlFinal x xs)             (P0)
= (:) y (xs ++ (x:[]))                  (HI)
= y : (xs ++ (x:[]))                    
= (y:xs) ++ (x:[])                      (def ++)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```


#### VII. reverse = foldr (\x rec -> rec ++ (x:[])) []
```
Por inducción de listas sobre xs necesito probar que:
P(xs): reverse = foldr (\x rec -> rec ++ (x:[])) []

Caso base: P([])

reverse []
= foldl (flip (:)) [] []                (R0)
= []                                    (def foldl)
= foldr (\x rec -> rec ++ (x:[])) [] []

Caso recursivo: ∀y :: a. ∀xs :: [a]. P(xs) ⇒ P(y:xs)
H.I.: reverse xs = foldr (\x rec -> rec ++ (x:[])) [] xs
Qvq reverse (y:xs) = foldr (\x rec -> rec ++ (x:[])) [] (y:xs)

reverse (y:xs)
= foldl (flip (:)) [] (y:xs)                      (R0)
= foldl (flip (:)) ((flip (:)) [] y) xs           (def foldl)
= foldl (flip (:)) ((:) y []) xs                  (def flip y (:))
= reverse xs ++ (y:[])                            (Lema)
= (\x rec -> rec ++ (x:[])) y (reverse xs)        
= (\x rec -> rec ++ (x:[])) y (foldr (\x rec -> rec ++ (x:[])) [] xs)     (HI)
= foldr (\x rec -> rec ++ (x:[])) [] (y:xs)                               (def foldr)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```
```
Lema:
Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ ac:[a]. foldl (flip (:)) ac xs = reverse xs ++ ac

Caso base: P([])

foldl (flip (:)) ac []
= ac                                   (def foldl)
= [] ++ ac                             (++)
= foldl (flip (:)) [] [] ++ ac
= reverse [] ++ ac                     (R0)

Caso inductivo: ∀ xs::[a]. ∀ x::a. P(xs) => P(x:xs)
H.I.: ∀ ac:[a]. foldl (flip (:)) ac xs = reverse xs ++ ac
Qvq ∀ ac:[a]. foldl (flip (:)) ac (x:xs) = reverse (x:xs) ++ ac

foldl (flip (:)) ac (x:xs)
= foldl (flip (:)) ((flip (:)) ac x) xs          (def foldl)
= foldl (flip (:)) ((:) x ac) xs                 (def flip)
= foldl (flip (:)) (x:ac) xs
= foldl (flip (:)) [] xs ++ (x:ac)               (HI)
= foldl (flip (:)) [] xs ++ (x:[]) ++ ac         (por ejercicio 3.III) 
= foldl (flip (:)) ((:) x []) xs ++ ac           (HI)
= foldl (flip (:)) ((flip (:)) [] x) xs ++ ac    
= foldl (flip (:)) [] (x:xs) ++ ac               (def foldl)
= reverse (x:xs) ++ ac                           (R0)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```


#### VIII. ∀ xs::[a] . ∀ x::a . head (reverse (ponerAlFinal x xs)) = x
```
Por inducción de listas sobre xs necesito probar que:
P(xs): ∀ x::a . head (reverse (ponerAlFinal x xs)) = x

Caso base: P([])

head (reverse (ponerAlFinal x []))
= head (reverse (foldr (:) (x:[]) [])               (P0)
= head (reverse (x:[]))                             (def foldr)
= head (foldr (\x rec -> rec ++ (x:[])) [] (x:[])   (por ejercicio anterior)
= head ((\x rec -> rec ++ (x:[])) x (foldr (\x rec -> rec ++ (x:[])) [] []))   (def foldr)
= head ((\x rec -> rec ++ (x:[])) x [])
= head ([] ++ (x:[]))                               (regla beta)
= head (foldr (:) (x:[]) [])                        (++)
= head (x:[])                                       (def foldr)
= x                                                 (def head)

Caso recursivo: ∀y :: a. ∀xs :: [a]. P(xs) ⇒ P(y:xs)
H.I.: ∀ x::a . head (reverse (ponerAlFinal x xs)) = x
Qvq ∀ x::a . head (reverse (ponerAlFinal x (y:xs))) = x

head (reverse (ponerAlFinal x (y:xs)))
= head (reverse (foldr (:) (x:[]) (y:xs)))          (P0)
= head (reverse ((:) y (foldr (:) (x:[]) xs)))      (def foldr)
= head (reverse ((:) y (ponerAlFinal x xs)))        (P0)
= head (reverse ((:) y (xs ++ (x:[]))))             (por ejercicio 3.VI)
= head (reverse (y : (xs ++ [x])
= head (reverse ((y:xs) ++ [x]))
= head (reverse [x] ++ reverse (y:xs))              (Lema 1)
= head ([x] ++ reverse (y:xs))                      (def reverse)
= x                                                 (Lema 2)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```
```
Lema 1
Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ ys::[a]. reverse (xs ++ ys) = reverse ys ++ reverse xs

Caso base: P([])

reverse ([] ++ ys)
= reverse (foldr (:) ys [])                         (++)
= reverse ys                                        (def foldr)
= reverse ys ++ []
= reverse ys ++ reverse []

Caso recursivo: ∀y :: a. ∀xs :: [a]. P(xs) ⇒ P(y:xs)
H.I.: ∀ ys::[a]. reverse (xs ++ ys) = reverse ys ++ reverse xs
Qvq ∀ ys::[a]. reverse ((y:xs) ++ ys) = reverse ys ++ reverse (y:xs)

reverse ((y:xs) ++ ys)
= reverse (foldr (:) ys (y:xs))                      (++)
= reverse ((:) y (foldr (:) ys xs))                  (def foldr)
= reverse ((:) y (xs ++ ys))                         (++)
= reverse (y:(xs ++ ys))
= foldr (\x rec -> rec ++ (x:[])) [] (y:(xs ++ ys))  (R0 version foldr)
= (\x rec -> rec ++ (x:[])) y (foldr (\x rec -> rec ++ (x:[])) [] (xs ++ ys))     (def foldr)
= (reverse (xs ++ ys)) ++ (y:[])                    (regla beta y R0 version foldr)
= reverse ys ++ reverse xs ++ (y:[])                (HI)
= reverse ys ++ (foldl (flip (:)) (y:[]) xs)        (por Lema del ejercicio 3.VII)
= reverse ys ++ (foldl (flip (:)) ((flip (:)) [] y) xs)  (def foldl)
= reverse ys ++ (foldl (flip (:)) [] (y:xs))             (def foldl)
= reverse ys ++ reverse (y:xs)                           (R0)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```
```
Lema 2
Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ ys::[a]. head (xs ++ ys) = head xs, asumo que xs tiene 1 o más elementos

Caso base: P([x])

head ([x] ++ ys)
= head (foldr (:) ys [x])                    (++)
= head ((:) x (foldr (:) ys [])              (def foldr)
= head ((:) x ys)
= head (x:ys)
= x                                          (def head)
= head([x])

Caso recursivo: ∀y :: a. ∀xs :: [a]. P(xs) ⇒ P(y:xs)
H.I.: ∀ ys::[a]. head (xs ++ ys) = head xs
Qvq ∀ ys::[a]. head ((y:xs) ++ ys) = head (y:xs)

head ((y:xs) ++ ys)
= head (foldr (:) ys (y:xs))                      (++)
= head ((:) y (foldr (:) ys xs))                  (def foldr)
= head ((:) y (xs ++ ys))                         (++)
= head (y:(xs ++ ys))
= y                                               (def head)
= head (y:xs)          

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 4

[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 5
Dadas las siguientes funciones, demostrar que zip = zip' utilizando inducción estructural y el principio de extensionalidad.
```haskell
     zip :: [a] -> [b] -> [(a,b)]
{Z0} zip = foldr (\x rec ys -> if null ys then [] else (x, head ys) : rec (tail ys)) (const [])

      zip' :: [a] -> [b] -> [(a,b)]
{Z'0} zip' [] ys = []
{Z'1} zip' (x:xs) ys = if null ys then [] else (x, head ys) : zip' xs (tail ys)
```
```
Por el [principio de extensionalidad funcional](#principio-de-extensionalidad-funcional) basta ver que:
∀ xs :: [a]. ∀ ys :: [b]. zip xs ys = zip' xs ys

Por inducción de listas sobre xs necesito probar que:
∀ xs :: [a]. P(xs): ∀ ys :: [b]. zip xs ys = zip' xs ys

Caso base: P([])

zip [] ys
= foldr (\x rec ys -> if null ys then [] else (x, head ys) : rec (tail ys)) (const []) [] ys     (Z0)
= const [] ys                         (def foldr)
= []                                  (def const)
= zip' [] ys                          (Z'0)

Caso recursivo: ∀x :: a. ∀xs :: [a]. P(xs) ⇒ P(x:xs)
H.I.: ∀ ys :: [b]. zip xs ys = zip' xs ys
Qvq ∀ ys :: [b]. zip (x:xs) ys = zip' (x:xs) ys

zip (x:xs) ys
= foldr f (const []) (x:xs) ys         (Z0)
= f y (foldr f (const []) xs) ys       (def foldr)
= if null ys then [] else (x, head ys) : (foldr f (const []) xs (tail ys))     (def f)
= if null ys then [] else (x, head ys) : (zip xs (tail ys))                    (Z0)
= if null ys then [] else (x, head ys) : (zip' xs (tail ys))                   (HI)
= zip' (x:xs) ys                                                               (Z'1)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

### Ejercicio 6
Dadas las siguientes funciones, Indicar si las siguientes propiedades son verdaderas o falsas. Si son verdaderas, realizar una demostración. Si son falsas, presentar un contraejemplo.
```haskell
     nub :: Eq a => [a] -> [a]
{N0} nub [] = []
{N1} nub (x:xs) = x : filter (\y -> x /= y) (nub xs)

     union :: Eq a => [a] -> [a] -> [a]
{U0} union xs ys = nub (xs++ys)

     intersect :: Eq a => [a] -> [a] -> [a]
{I0} intersect xs ys = filter (\e -> elem e ys) xs
```

#### I. Eq a => ∀ xs::[a] . ∀ e::a . ∀ p::a -> Bool . elem e xs && p e = elem e (filter p xs)

```
Si Eq a = False, False implica cualquier cosa.
Asumo que vale Eq a.

Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀e :: a. ∀p :: a -> Bool. (elem e xs && p e) = elem e (filter p xs)

Caso base: P([])

(elem e [] && p e)
= (False && p e)                              (def elem)
= False                                       (def &&)
= elem e []                                   (def elem)
= elem e (filter p [])                        (def filter)

Caso recursivo: ∀x :: a. ∀xs :: [a]. P(xs) ⇒ P(x:xs)
H.I.: ∀e :: a. ∀p :: a -> Bool. (elem e xs && p e) = elem e (filter p xs)
Qvq ∀e :: a. ∀p :: a -> Bool. (elem e (x:xs) && p e) = elem e (filter p (x:xs))

Lado derecho:
elem e (filter p (x:xs))
= elem e (if p x then x : filter p xs else filter p xs)  (def filter)

Por extensionalidad de booleanos, analizo los casos por separado:

Caso 1: Si p x = True
elem e (if True then x : filter p xs else filter p xs)
= elem e (x : filter p xs)                                (def if)
= (e == x) || elem e (filter p xs)                        (def elem)
= (e == x) || (elem e xs && p e)                          (HI)
= (e == x || elem e xs) && (e == x || p e)                (distributiva del ||)
= elem e (x:xs) && (e == x || p e)                        (def elem)

Lado izquierdo:
elem e (x:xs) && p e
= (e == x || elem e xs) && p e                            (def elem)

Por extensionalidad de booleanos, analizo los casos por separado:

Caso 1.1: Si e == x = False
(e == x || elem e xs) && p e
= (False || elem e xs) && p e
= elem e xs && p e
= elem e (filter p xs)                                    (HI)

Caso 1.2: Si e == x = True
(e == x || elem e xs) && p e
= (True || elem e xs) && p e
= True && p e                                             (def ||)
Como e == x y estoy en el caso 1 donde p x = True queda:
= True

Caso 2: Si p x = False
elem e (filter p (x:xs))
= elem e (filter p xs)

Lado izquierdo:
elem e (x:xs) && p e
= (e == x || elem e xs) && p e

Lo separo en dos subcasos:

Subcaso 2.1: Si e == x
Si e == x, entonces p x = False, lo que implica que p e = False.
Entonces, ambos lados son False.

Subcaso 2.2: Si e == x = False
(False || elem e xs) && p e = elem e xs && p e
elem e xs && p e = elem e (filter p xs)
Entonces ambos lados coinciden.

Como ya demostré que la proposición se cumple para todos los casos.
Por lo tanto, la propiedad vale para ∀ xs :: [a]. 
```

#### II. Eq a => ∀ xs::[a] . ∀ e::a . elem e xs = elem e (nub xs)
```
Si Eq a = False, False implica cualquier cosa.
Asumo que vale Eq a.

Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ e::a . elem e xs = elem e (nub xs)

Caso base: P([])

elem e []
= False                                   (def elem)
= elem e []                               (N0)
= elem e (nub [])

Caso recursivo: ∀x :: a. ∀xs :: [a]. P(xs) ⇒ P(x:xs)
H.I.: ∀ e::a . elem e xs = elem e (nub xs)
Qvq ∀ e::a . elem e (x:xs) = elem e (nub (x:xs))

Lado izquierdo:
elem e (x:xs)
= e == x || elem e xs

Lado derecho:
elem e (nub (x:xs))
= elem e (x : filter (\y -> x /= y) (nub xs))          (N1)
= e == x || elem e (filter (\y -> x /= y) (nub xs))    (def elem)

Por extensionalidad de booleanos, analizo los casos por separado:

1. Si e == x = True
Lado izquierdo:
= e == x || elem e xs
= True || elem e xs

Lado derecho:
= e == x || elem e (filter (\y -> x /= y) (nub xs))    (def elem)
= True || ...
= True

2. Si e == x = False
Lado izquierdo:
= e == x || elem e xs
= False || elem e xs
= elem e xs

Lado derecho:
= e == x || elem e (filter (\y -> x /= y) (nub xs))    (def elem)
= False || elem e (filter (\y -> x /= y) (nub xs))
= elem e (filter (\y -> x /= y) (nub xs))

Qvq elem e xs = elem e (filter (\y -> x /= y) (nub xs))
elem e xs = elem e (filter (\y -> x /= y) (nub xs)
elem e xs = elem e (nub xs) && (\y -> x /= y) e      (uso el 6.I)
Como estoy en el donde e == x = False, la condición "(\y -> x /= y) e" se cumple trivialmente. Y por la conjunción de booleanos, True && cualquier cosa = cualquier cosa.
elem e xs = elem e (nub xs)
elem e xs = elem e xs                         (HI)                          
Entonces ambos lados coinciden.

Como ya demostré que la proposición se cumple para todos los casos.
Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

#### III. Eq a => ∀ xs::[a] . ∀ ys::[a] . ∀ e::a . elem e (union xs ys) = (elem e xs) || (elem e ys)
```
Si Eq a = False, False implica cualquier cosa.
Asumo que vale Eq a.

Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ e::a . elem e (union xs ys) = (elem e xs) || (elem e ys)

Caso base: P([])

elem e (union [] ys)
= elem e (nub ([] ++ ys))                 (U0)
= elem e ys
= False || elem e ys                                  
= elem e [] || elem e ys  

Caso recursivo: ∀x :: a. ∀xs :: [a]. P(xs) ⇒ P(x:xs)
H.I.: ∀ e::a . elem e (union xs ys) = (elem e xs) || (elem e ys)
Qvq ∀ e::a . elem e (union (x:xs) ys) = (elem e (x:xs)) || (elem e ys)

Lado izquierdo:
elem e (union (x:xs) ys)
= elem e (nub ((x:xs) ++ ys))                                  (U0)
= elem e (nub (x : (xs ++ ys)))                                (def ++) 
= elem e (x : filter (\y -> x /= y) (nub (xs++ys)))            (N1)
= e == x || elem e (filter (\y -> x /= y) (nub (xs ++ ys)))    (def elem)

Lado derecho:
(elem e (x:xs)) || (elem e ys)
= e == x || elem e xs || elem e ys                             (def elem)

Por extensionalidad de booleanos, analizo los casos por separado:

1. Si e == x = True
Lado izquierdo:
= e == x || elem e (filter (\y -> x /= y) (nub (xs ++ ys)))
= True || ...
= True                       

Lado derecho:
= e == x || elem e xs || elem e ys
= True || ... || ...
= True

2. Si e == x = False
Lado izquierdo:
= e == x || elem e (filter (\y -> x /= y) (nub (xs ++ ys)))
= False || elem e (filter (\y -> x /= y) (nub (xs ++ ys)))
= elem e (filter (\y -> x /= y) (nub (xs ++ ys)))
= elem e (nub (xs ++ ys)) && (\y -> x /= y) e                    (uso el 6.I)
Como estoy en el donde e == x = False, la condición "(\y -> x /= y) e" se cumple trivialmente. Y por la conjunción de booleanos, True && cualquier cosa = cualquier cosa.
= elem e (nub (xs ++ ys))
= elem e (union xs ys)                                   (U0)
= (elem e xs) || (elem e ys)                             (HI)

Lado derecho:
= e == x || elem e xs || elem e ys
= False || elem e xs || elem e ys
= elem e xs || elem e ys
Entonces ambos lados coinciden.

Como ya demostré que la proposición se cumple para todos los casos.
Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

#### IV. Pendiente... Eq a => ∀ xs::[a] . ∀ ys::[a] . ∀ e::a . elem e (intersect xs ys) = (elem e xs) && (elem e ys)
```
(Es parecido al anterior pero con algunas cosas mas)
Si Eq a = False, False implica cualquier cosa.
Asumo que vale Eq a.

Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ ys::[a] . ∀ e::a . elem e (intersect xs ys) = (elem e xs) && (elem e ys)

Caso base: P([])

elem e (intersect [] ys)
= elem e (filter (\e -> elem e ys) [])               (I0)
= elem e []                                          (def filter)
= False
= False && (elem e ys)
= (elem e []) && (elem e ys)

Caso recursivo: ∀x :: a. ∀xs :: [a]. P(xs) ⇒ P(x:xs)
H.I.: ∀ ys::[a] . ∀ e::a . elem e (intersect xs ys) = (elem e xs) && (elem e ys)
Qvq ∀ ys::[a] . ∀ e::a . elem e (intersect (x:xs) ys) = (elem e (x:xs)) && (elem e ys)

Lado izquierdo:
elem e (intersect (x:xs) ys)
= elem e (filter (\e -> elem e ys) (x:xs))                (I0)
= elem e (if p x then x : filter p xs else filter p xs)   (def filter)

Lado derecho:
(elem e (x:xs)) && (elem e ys)
= e == x || elem e xs || elem e ys                        (def elem)

Por extensionalidad de booleanos, analizo los casos por separado:

1. Si p x = True
Lado izquierdo:
= elem e (if p x then x : filter p xs else filter p xs)   (def filter)
= elem e (x : filter p xs)
= (e == x) || elem e (filter p xs)                        (def elem)                   

Analizo los subcasos:
1.1 Si e == x = True
= (e == x) || elem e (filter p xs)                        (def elem)  


2. Si p x = False
Lado izquierdo:
= elem e (if p x then x : filter p xs else filter p xs)   (def filter)
= elem e (filter p xs)
= 

Lado derecho:


2. Si e == x = False
Lado izquierdo:


Lado derecho:

Entonces ambos lados coinciden.

Como ya demostré que la proposición se cumple para todos los casos.
Por lo tanto, la propiedad vale para ∀ xs :: [a].
```
#### V. Eq a => ∀ xs::[a] . ∀ ys::[a] . length (union xs ys) = length xs + length ys
```
Contraejemplo:
Sea xs = [1,2] e ys = [3,4]
Entonces union xs ys = [1,2,3] y length (union xs ys) = 3
Pero
length xs = 2
length ys = 2

Por lo tanto,
length (union xs ys) = length xs + length ys
3 = 2 + 2
¡¡¡Absurdo!!! 
```

#### VI. Eq a => ∀ xs::[a] . ∀ ys::[a] . length (union xs ys) ≤ length xs + length ys
```
Si Eq a = False, False implica cualquier cosa.
Asumo que vale Eq a.

Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ ys::[a] . length (union xs ys) ≤ length xs + length ys

Caso base: P([])

length (union [] ys) ≤ length [] + length ys
= length (nub ([] ++ ys)) ≤ 0 + length ys                 (U0 y def length)
= length (nub ys) ≤ length ys 

Caso recursivo: ∀x :: a. ∀xs :: [a]. P(xs) ⇒ P(x:xs)
H.I.: ∀ ys::[a] . length (union xs ys) ≤ length xs + length ys
Qvq ∀ ys::[a] . length (union (x:xs) ys) ≤ length (x:xs) + length ys

length (union (x:xs) ys) ≤ length (x:xs) + length ys
= length (nub ((x:xs)++ys)) ≤ 1 + length xs + length ys
= length (nub (x:(xs++ys))) ≤ 1 + length xs + length ys
= length (x : filter (\y -> x /= y) (nub (xs++ys))) ≤ 1 + length xs + length ys
= 1 + length (filter (\y -> x /= y) (nub (xs++ys))) ≤ 1 + length xs + length ys
= length (filter (\y -> x /= y) (nub (xs++ys))) ≤ length xs + length ys
= length (union xs ys) ≤ length xs + length ys
Esto vale por la hipotesis inductiva.

Como ya demostré que la proposición se cumple para todos los casos.
Por lo tanto, la propiedad vale para ∀ xs :: [a].
```
[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 7

[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 8

[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 9

Dadas las funciones altura y cantNodos definidas en la práctica 1 para árboles binarios, demostrar la siguiente propiedad: ∀ t :: AB a . altura t ≤ cantNodos t

```haskell
data AB a = Nil | Bin (AB a) a (AB a) deriving (Show, Eq)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin arbol = case arbol of
    Nil -> cNil
    Bin i r d -> cBin (rec i) r (rec d)
    where rec = foldAB cNil cBin

altura :: AB a -> Int
altura = foldAB 0 (\ri _ rd -> 1+ max ri rd)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\ri _ rd -> 1+ ri + rd)
```
```
Utilizo induccion estructural en arboles binarios.
∀ t :: AB a. P(t): altura t ≤ cantNodos t

Caso base: P(Nil)

altura Nil
= foldAB 0 f Nil
= 0
≤ 0
= foldAB 0 g Nil
= cantNodos Nil

Caso recursivo: ∀i :: AB a. ∀r :: a. ∀d :: AB a. P(i) ∧ P(d) ⇒ P(Bin i r d)
H.I.:
∀ i :: AB a. P(i): altura i ≤ cantNodos i.
∀ d :: AB a. P(d): altura d ≤ cantNodos d.
Qvq ∀ t :: AB a. P(Bin i r d): altura (Bin i r d) ≤ cantNodos (Bin i r d).

altura (Bin i r d)
= foldAB 0 (\ri _ rd -> 1+ max ri rd) (Bin i r d)               (def altura)
= f (foldAB 0 f i) r (foldAB 0 f d)                             (def foldAB)
= 1 + max (foldAB 0 f i) (foldAB 0 f d)                         (regla beta)
= 1 + max (altura i) (altura d)
≤ 1 + max (cantNodos i) (cantNodos d)                           (HI)
≤ 1 + (cantNodos i) + (cantNodos d)                             (regla beta)
= g (cantNodos i) r (cantNodos d)                               (def foldAB)
= foldAB 0 g (Bin i r d)                                        (def cantNodos)
= cantNodos (Bin i r d)

Por lo tanto, la propiedad vale para ∀ t :: AB a.
```
[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 10

[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 11

[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 12
Dados el tipo Polinomio definido en la práctica 1 y las siguientes funciones, demostrar las siguientes propiedades. 

```haskell
data Polinomio a = X | Cte a | Suma (Polinomio a) (Polinomio a) | Prod (Polinomio a) (Polinomio a)

derivado :: Num a => Polinomio a -> Polinomio a
derivado poli = case poli of
                    X -> Cte 1
                    Cte _ -> Cte 0
                    Suma p q -> Suma (derivado p) (derivado q)
                    Prod p q -> Suma (Prod (derivado p) q) (Prod (derivado q) p)

sinConstantesNegativas :: Num a => Polinomio a -> Polinomio a
sinConstantesNegativas = foldPoli True (>=0) (&&) (&&)

esRaiz :: Num a => a -> Polinomio a -> Bool
esRaiz n p = evaluar n p == 0
```

#### I. Num a => ∀ p::Polinomio a . ∀ q::Polinomio a . ∀ r::a . (esRaiz r p ⇒ esRaiz r (Prod p q))

```
Si Num a = False, False implica cualquier cosa.
Asumo que vale Num a.

Utilizo inducción estructural sobre polinomios, necesito ver que:
∀ p::Polinomio a. P(p): ∀ q::Polinomio a . ∀ r::a . (esRaiz r p ⇒ esRaiz r (Prod p q))

Casos base:
- P(X)

esRaiz r X ⇒ esRaiz r (Prod X q)
= evaluar r X == 0 ⇒ evaluar r (Prod X q) == 0
= r == 0 ⇒ (evaluar r X * evaluar r q) == 0
= r == 0 ⇒ (evaluar r X * evaluar r q) == 0
= r == 0 ⇒ (r * evaluar r q) == 0

Si r == 0 = True, entonces
0 == 0 ⇒ (0 * evaluar 0 q) == 0
True ⇒ 0 == 0
True ⇒ True = True

Si r == 0 = False, entonces
False ⇒ cualquier cosa

- P(Cte a)

esRaiz r (Cte a) ⇒ esRaiz r (Prod (Cte a) q)
= evaluar r (Cte a) == 0 ⇒ (evaluar r (Cte a) * evaluar r q) == 0
= a == 0 ⇒ (a * evaluar r q) == 0

Si a == 0 = True, entonces
0 == 0 ⇒ (0 * evaluar r q) == 0
True ⇒ 0 == 0
True ⇒ True = True

Si a == 0 = False, entonces
False ⇒ cualquier cosa

Caso recursivo:
- ∀ p, s::Polinomio a. ∀ r::a . (P(p) y P(s)) ⇒ P(Suma p s)
H.I.:
P(p): ∀ q::Polinomio a . ∀ r::a . (esRaiz r p ⇒ esRaiz r (Prod p q))
P(s): ∀ q::Polinomio a . ∀ r::a . (esRaiz r s ⇒ esRaiz r (Prod s q))

esRaiz r (Suma p s) ⇒ esRaiz r (Prod (Suma p s) q)
= evaluar r (Suma p s) == 0 ⇒ (evaluar r (Suma p s) * evaluar r q) == 0
= (evaluar r p + evaluar r s) == 0  ⇒ ((evaluar r p + evaluar r s) * evaluar r q) == 0

No sé si está bien...
Si evaluar r p == 0, i.e. esRaiz r p ⇒ esRaiz r (Prod p q)
= (0 + evaluar r s) == 0  ⇒ ((0 + evaluar r s) * evaluar r q) == 0
y si evaluar r s == 0, i.e. esRaiz r s ⇒ esRaiz r (Prod s q)
= (0 + 0) == 0  ⇒ ((0 + 0) * evaluar r q) == 0
Queda:
= True ⇒ 0 == 0
= True

Lo mismo para P(Prod p q)

Como ya demostré que la proposición se cumple para todos los casos.
Por lo tanto, la propiedad vale para ∀ p::Polinomio a.
```

#### II. Num a => ∀ p::Polinomio a . ∀ k::a . ∀ e::a . evaluar e (derivado (Prod (Cte k) p)) = evaluar e (Prod (Cte k) (derivado p))
```
Si Num a = False, False implica cualquier cosa.
Asumo que vale Num a.

Utilizo inducción estructural sobre polinomios, necesito ver que:

```
#### III. Num a => ∀ p::Polinomio a. (sinConstantesNegativas p⇒sinConstantesNegativas (derivado p))

#### IV. La recursión utilizada en la definición de la función derivado ¿es estructural, primitiva o ninguna de las dos?



[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

