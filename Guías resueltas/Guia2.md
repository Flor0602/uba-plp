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
#### I. ∀ xs::[a] . length (duplicar xs) = 2 * length xs
```
Por inducción de listas sobre xs necesito probar que:
P(xs): length (duplicar xs) = 2 * length xs

Caso base P([]):

length (duplicar [])
= length []                         {D0}
= 0                                 {L0}
= 2 * 0
= 2 * length []                     {L0}

Caso recursivo ∀x :: a. ∀xs :: [a]. P(xs) ⇒ P(x:xs):
H.I.: length (duplicar xs) = 2 * length xs

length (duplicar (x : xs)) 
= length (x : x : duplicar xs)      (D1)
= 1 + length (x : duplicar xs)      (L1)
= 1 + 1 + length (duplicar xs)      (L1)
= 2 + 2 * length xs                 (HI)
= 2 * (1 + length xs)
= 2 * length (x:xs)                 {L1}

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

#### II. ∀ xs::[a] . ∀ ys::[a] . length (append xs ys) = length xs + length ys




#### III. ∀ xs::[a] . ∀ x::a . [x]++xs = x:xs
Descripción del Ejercicio III.



#### IV. ∀ xs::[a] . ∀ f::(a->b) . length (map f xs) = length xs
Descripción del Ejercicio IV.



#### V. ∀ xs::[a] . ∀ p::a->Bool . ∀ e::a . ((elem e (filter p xs)) ⇒ (elem e xs)) (asumiendo Eq a)
Descripción del Ejercicio V.



#### VI. ∀ xs::[a] . ∀ x::a . ponerAlFinal x xs = xs ++ (x:[])
Descripción del Ejercicio VI.



#### VII. reverse = foldr (\x rec -> rec ++ (x:[])) []
Descripción del Ejercicio VII.


#### VIII. ∀ xs::[a] . ∀ x::a . head (reverse (ponerAlFinal x xs)) = x
Descripción del Ejercicio VIII.

[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

