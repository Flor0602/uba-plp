# Práctica 2 - Razonamiento ecuacional e inducción estructural

## Extensionalidad

- [Ejercicio 1](#ejercicio-1)
- [Ejercicio 2](#ejercicio-2)
## Inducción sobre listas

- [Ejercicio 3](#ejercicio-3)
- [Ejercicio 4](#ejercicio-4)
- [Ejercicio 5](#ejercicio-5)
- [Ejercicio 6](#ejercicio-6)
- [Ejercicio 7](#ejercicio-7)
## Otras estructuras de datos

- [Ejercicio 8](#ejercicio-8)
- [Ejercicio 9](#ejercicio-9)
- [Ejercicio 10](#ejercicio-10)
- [Ejercicio 11](#ejercicio-11)
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

Lo separo en casos:
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


### Ejercicio 4

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

Caso recursivo: ∀y :: a. ∀xs :: [a]. P(xs) ⇒ P(y:xs)
H.I.: ∀ ys :: [b]. zip xs ys = zip' xs ys
Qvq ∀ ys :: [b]. zip (y:xs) ys = zip' (y:xs) ys

zip (y:xs) ys
= foldr f (const []) (y:xs) ys
= f y (foldr f (const []) xs) ys
= 
= 
```
[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

