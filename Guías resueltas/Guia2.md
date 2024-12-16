# Práctica 2 - Razonamiento ecuacional e inducción estructural

## Extensionalidad

- [Ejercicio 1](#ejercicio-1) ✔️
- [Ejercicio 2](#ejercicio-2) ✔️
## Inducción sobre listas

- [Ejercicio 3](#ejercicio-3) ✔️
- [Ejercicio 4](#ejercicio-4) ✔️
- [Ejercicio 5](#ejercicio-5) ✔️
- [Ejercicio 6](#ejercicio-6) ✔️
- [Ejercicio 7 falta hacer](#ejercicio-7)
## Otras estructuras de datos

- [Ejercicio 8 falta hacer](#ejercicio-8)
- [Ejercicio 9](#ejercicio-9)
- [Ejercicio 10 falta hacer](#ejercicio-10)
- [Ejercicio 11 falta hacer](#ejercicio-11)
- [Ejercicio 12 incompleto](#ejercicio-12)



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
∀ f :: a -> b -> c. ∀ x :: a. ∀y :: b.
(flip . flip) f x y = id f x y

(flip . flip) f x y
= flip (flip f x y)     (def .)
= flip f y x            (def flip)
= f x y                 (def flip)
= id f x y              (def id)
```

#### II. ∀ f : : (a, b) -> c . uncurry (curry f) = f
```
Por el principio basta ver que:
∀ f::(a,b)->c, ∀ p :: (a,b). uncurry (curry f) p = f p

Por el principio de extensionalidad de pares tengo que probar:
∀ f::(a,b)->c, ∀ p :: (a,b), ∃x :: a. ∃y :: b. p = (x, y). uncurry (curry f) (x, y) = f (x, y)

uncurry (curry f) (x,y)
= curry f x y               (def uncurry)
= f (x,y)                   (def curry)
```

#### III. flip const = const id
```
Por el principio basta ver que:
∀x :: a. ∀y :: b. flip const x y = const id x y

flip const x y
= const y x         (def flip)
= x                 (def const)
= id x              (def id)
= const id x y      (def const)
```

#### IV. ∀ f : : a -> b . ∀ g : : b -> c . ∀ h : : c -> d . ((h . g) . f) = (h . (g . f)) con la definición usual de la composición: (.) f g x = f (g x).
```
Por principio de extensionalidad funcional (ver ejercicio 2) basta ver que:
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
∀ xs::[a]. P(xs): length (duplicar xs) = 2 * length xs

Caso base: P([])

length (duplicar [])
= length []                         (D0)
= 0                                 (L0)
= 2 * 0
= 2 * length []                     (L0)

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys)
Supongamos que P(ys) se cumple, es decir, length (duplicar ys) = 2 * length ys.
Ahora, debemos demostrar que length (duplicar (y:ys)) = 2 * length (y:ys).

length (duplicar (y:ys)) 
= length (y : y : duplicar ys)      (D1)
= 1 + length (y : duplicar ys)      (L1)
= 1 + 1 + length (duplicar ys)      (L1)
= 2 + 2 * length ys                 (HI)
= 2 * (1 + length ys)
= 2 * length (y:ys)                 (L1)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

#### II. ∀ xs::[a] . ∀ ys::[a] . length (append xs ys) = length xs + length ys
```
Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ ys::[a]. length (append xs ys) = length xs + length ys

Caso base: P([])

length (append [] ys)
= length ys                         (A0)
= 0 + length ys
= length [] + length ys             (LO)

Caso recursivo: xs = z:zs

∀z :: a. ∀zs :: [a]. P(zs) ⇒ P(z:zs)
Supongamos que P(zs) se cumple, es decir, ∀ ys::[a]. length (append zs ys) = length zs + length ys.
Ahora, debemos demostrar que ∀ ys::[a]. length (append (z:zs) ys) = length (z:zs) + length ys.

length (append (z:zs) ys)
= length (z : append zs ys)          (A1)
= 1 + length (append zs ys)
= 1 + length zs + length ys          (HI)
= length (z:zs) + length ys          (L1)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```


#### III. ∀ xs : : [a] . ∀ x::a . [x] ++ xs = x:xs

```
Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ x::a. [x] ++ xs = x:xs

Caso base: P([])

[x] ++ []
= foldr (:) [] [x]                    (++)
= [x]                                 (def foldr)
= x : []

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys)
Supongamos que P(ys) se cumple, es decir, ∀ x::a. [x] ++ ys = x:ys.
Ahora, debemos demostrar que ∀x :: a. [x] ++ (y:ys) = x:(y:ys). 

[x] ++ (y:ys)
= foldr (:) (y:ys) [x]               (++)
= (:) x (foldr (:) (y:ys) [])        (def foldr)
= (:) x (y:ys)                       (def foldr)
= x : (y:ys) 

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```


#### IV. ∀ xs::[a] . ∀ f::(a->b) . length (map f xs) = length xs
```
Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ f :: a -> b. length (map f xs) = length xs

Caso base: P([])

length (map f [])
= length []                          (def map)
= 0                                  (LO)
= length []                          (LO)

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys)
Supongamos que P(ys) se cumple, es decir, ∀ f :: a -> b. length (map f ys) = length ys.
Ahora, debemos demostrar que ∀ f :: a -> b. length (map f (y:ys)) = length (y:ys). 

length (map f (y:ys))
= length (f y : map f ys)            (def map)
= 1 + length (map f ys)              (L1)
= 1 + length ys                      (HI)
= length (y:ys)                      (L1)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```


#### V. ∀ xs::[a] . ∀ p::a->Bool . ∀ e::a . ((elem e (filter p xs)) ⇒ (elem e xs)) (asumiendo Eq a)
```
Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ p::a->Bool . ∀ e::a . ((elem e (filter p xs)) ⇒ (elem e xs)) (asumiendo Eq a)

Caso base: P([])

(elem e (filter p []))
= elem e []                          (def filter)
= False
= False => (elem e xs)               (def =>)

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys)
Supongamos que P(ys) se cumple, es decir, ∀ p::a->Bool . ∀ e::a . ((elem e (filter p ys)) ⇒ (elem e ys)) (asumiendo Eq a).
Ahora, debemos demostrar que ∀ p::a->Bool . ∀ e::a . ((elem e (filter p (y:ys))) ⇒ (elem e (y:ys))) (asumiendo Eq a). 

elem e (filter p (y:ys))
= elem e (if p y then y:filter p ys else filter p ys)     (def filter)

Por extensionalidad de booleanos, analizo los casos por separado:
1. Si p y = False

= elem e (filter p ys)
= (elem e (filter p ys)) ⇒ (elem e ys) (HI)
Si elem e (filter p ys) = False entonces False implica cualquier cosa.
Si elem e (filter p ys) = True significa que e se encuentra en filter p ys ⊆ ys ⊆ y:ys.

2. Si p y = True

elem e (y:filter p ys)
= (e == y) || elem e (filter p ys)

Si e == y = True
True || elem e (filter p ys)
= True                                  (def ||)

Qvq True ⇒ (elem e (y:ys)) para que valga el caso recursivo.
Para eso veo: elem e (y:ys) = True sabiendo que e == y

elem e (y:ys) = True
= (e == y || elem e ys) = True                                      (def elem)
= (True || elem e ys) = True                                        (e == y)
= True = True                                                       (def ||)
= True

Si e == y = False
False || elem e (filter p ys)
= elem e (filter p ys)                             (def ||)
= (elem e (filter p ys)) ⇒ (elem e ys)            (HI)
= (elem e (filter p ys) = True) ⇒ (elem e (y:ys))   (por caso 1)


Como ya lo probé para todo los casos posibles. Por lo tanto, la propiedad vale para ∀ xs :: [a].
```


#### VI. ∀ xs::[a] . ∀ x::a . ponerAlFinal x xs = xs ++ (x:[])
```
Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ x::a . ponerAlFinal x xs = xs ++ (x:[])

Caso base: P([])

ponerAlFinal x []
= foldr (:) (x:[]) []                    (P0)
= x:[]                                   (def foldr)
= foldr (:) (x:[]) []                    (++)
= [] ++ (x:[])                           (def foldr)

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys)
Supongamos que P(ys) se cumple, es decir, ∀ x::a. ponerAlFinal x ys = ys ++ (x:[])
Ahora, debemos demostrar que ∀ x::a . ponerAlFinal x (y:ys) = (y:ys) ++ (x:[]). 

ponerAlFinal x (y:ys)
= foldr (:) (x:[]) (y:ys)               (P0)
= (:) y (foldr (:) (x:[]) ys)           (def foldr)
= (:) y (ponerAlFinal x ys)             (P0)
= (:) y (ys ++ (x:[]))                  (HI)
= y : (ys ++ (x:[]))                    
= (y:ys) ++ (x:[])                      (def ++)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```


#### VII. reverse = foldr (\x rec -> rec ++ (x:[])) []
```
Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): reverse = foldr (\x rec -> rec ++ (x:[])) []

Caso base: P([])

reverse []
= foldl (flip (:)) [] []                (R0)
= []                                    (def foldl)
= foldr (\x rec -> rec ++ (x:[])) [] []

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys)
Supongamos que P(ys) se cumple, es decir, reverse ys = foldr (\x rec -> rec ++ (x:[])) [] ys
Ahora, debemos demostrar que reverse (y:ys) = foldr (\x rec -> rec ++ (x:[])) [] (y:ys).

reverse (y:ys)
= foldl (flip (:)) [] (y:ys)                      (R0)
= foldl (flip (:)) ((flip (:)) [] y) ys           (def foldl)
= foldl (flip (:)) ((:) y []) ys                  (def flip y (:))
= reverse ys ++ (y:[])                            (Lema)
= (\x rec -> rec ++ (x:[])) y (reverse ys)        
= (\x rec -> rec ++ (x:[])) y (foldr (\x rec -> rec ++ (x:[])) [] ys)     (HI)
= foldr (\x rec -> rec ++ (x:[])) [] (y:ys)                               (def foldr)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```
```
Lema:
Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ ac::[a]. foldl (flip (:)) ac xs = reverse xs ++ ac

Caso base: P([])

foldl (flip (:)) ac []
= ac                                   (def foldl)
= [] ++ ac                             (++)
= foldl (flip (:)) [] [] ++ ac
= reverse [] ++ ac                     (R0)

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys)
Supongamos que P(ys) se cumple, es decir, ∀ ac::[a]. foldl (flip (:)) ac ys = reverse ys ++ ac.
Ahora, debemos demostrar que ∀ ac::[a]. foldl (flip (:)) ac (y:ys) = reverse (y:ys) ++ ac.

foldl (flip (:)) ac (y:ys)
= foldl (flip (:)) ((flip (:)) ac y) ys          (def foldl)
= foldl (flip (:)) ((:) y ac) ys                 (def flip)
= foldl (flip (:)) (y:ac) ys
= foldl (flip (:)) [] ys ++ (y:ac)               (HI)
= foldl (flip (:)) [] ys ++ (y:[]) ++ ac         (por ejercicio 3.III) 
= foldl (flip (:)) ((:) y []) ys ++ ac           (HI)
= foldl (flip (:)) ((flip (:)) [] y) ys ++ ac    
= foldl (flip (:)) [] (y:ys) ++ ac               (def foldl)
= reverse (y:ys) ++ ac                           (R0)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```


#### VIII. ∀ xs::[a] . ∀ x::a . head (reverse (ponerAlFinal x xs)) = x
```
Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ x::a . head (reverse (ponerAlFinal x xs)) = x

Caso base: P([])

head (reverse (ponerAlFinal x []))
= head (reverse (foldr (:) (x:[]) []))              (P0)
= head (reverse (x:[]))                             (def foldr)
= head (foldr (\x rec -> rec ++ (x:[])) [] (x:[]))   (por ejercicio anterior)
= head ((\x rec -> rec ++ (x:[])) x (foldr (\x rec -> rec ++ (x:[])) [] []))   (def foldr)
= head ((\x rec -> rec ++ (x:[])) x [])
= head ([] ++ (x:[]))                               (regla beta)
= head (foldr (:) (x:[]) [])                        (++)
= head (x:[])                                       (def foldr)
= x                                                 (def head)

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys)
Supongamos que P(ys) se cumple, es decir, ∀ x::a. head (reverse (ponerAlFinal x ys)) = x.
Ahora, debemos demostrar que ∀ x::a. head (reverse (ponerAlFinal x (y:ys))) = x.

head (reverse (ponerAlFinal x (y:ys)))
= head (reverse (foldr (:) (x:[]) (y:ys)))          (P0)
= head (reverse ((:) y (foldr (:) (x:[]) ys)))      (def foldr)
= head (reverse ((:) y (ponerAlFinal x ys)))        (P0)
= head (reverse ((:) y (ys ++ (x:[]))))             (por ejercicio 3.VI)
= head (reverse (y : (ys ++ [x])))
= head (reverse ((y:ys) ++ [x]))
= head (reverse [x] ++ reverse (y:ys))              (Lema 1)
= head ([x] ++ reverse (y:ys))                      (def reverse)
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

Caso recursivo: xs = z:zs

∀z :: a. ∀zs :: [a]. P(zs) ⇒ P(z:zs)
Supongamos que P(zs) se cumple, es decir, ∀ ys::[a]. reverse (zs ++ ys) = reverse ys ++ reverse zs.
Ahora, debemos demostrar que ∀ ys::[a]. reverse ((z:zs) ++ ys) = reverse ys ++ reverse (z:zs).

reverse ((z:zs) ++ ys)
= reverse (foldr (:) ys (z:zs))                      (++)
= reverse ((:) z (foldr (:) ys zs))                  (def foldr)
= reverse ((:) z (zs ++ ys))                         (++)
= reverse (z:(zs ++ ys))
= foldr (\x rec -> rec ++ (x:[])) [] (z:(zs ++ ys))  (R0 version foldr)
= (\x rec -> rec ++ (x:[])) z (foldr (\x rec -> rec ++ (x:[])) [] (zs ++ ys))     (def foldr)
= (reverse (zs ++ ys)) ++ (z:[])                    (regla beta y R0 version foldr)
= reverse ys ++ reverse zs ++ (z:[])                (HI)
= reverse ys ++ (foldl (flip (:)) (z:[]) zs)        (por Lema del ejercicio 3.VII)
= reverse ys ++ (foldl (flip (:)) ((flip (:)) [] z) zs)  (def foldl)
= reverse ys ++ (foldl (flip (:)) [] (z:zs))             (def foldl)
= reverse ys ++ reverse (z:zs)                           (R0)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```
```
Lema 2
Por inducción de listas sobre xs necesito probar que:
∀ xs::[a]. P(xs): ∀ ys::[a]. head (xs ++ ys) = head xs, asumo que xs tiene 1 o más elementos

Caso base: P([x])

head ([x] ++ ys)
= head (foldr (:) ys [x])                    (++)
= head ((:) x (foldr (:) ys []))              (def foldr)
= head ((:) x ys)
= head (x:ys)
= x                                          (def head)
= head([x])

Caso recursivo: xs = z:zs

∀z :: a. ∀zs :: [a]. P(zs) ⇒ P(z:zs)
Supongamos que P(zs) se cumple, es decir, ∀ ys::[a]. head (zs ++ ys) = head zs.
Ahora, debemos demostrar que ∀ ys::[a]. head ((z:zs) ++ ys) = head (z:zs).

head ((z:zs) ++ ys)
= head (foldr (:) ys (z:zs))                      (++)
= head ((:) z (foldr (:) ys zs))                  (def foldr)
= head ((:) z (zs ++ ys))                         (++)
= head (z:(zs ++ ys))
= z                                               (def head)
= head (z:zs)          

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 4
Demostrar las siguientes propiedades utilizando inducción estructural sobre listas y el principio de extensionalidad.
#### I. reverse . reverse = id
```
Por principio de extensionalidad funcional (ver ejercicio 2) basta ver que:
∀ xs :: [a]. (reverse . reverse) xs = id xs

Por inducción de listas sobre xs, necesitamos probar que:
∀ xs :: [a]. P(xs): reverse (reverse xs) = xs

Caso base: xs = []
reverse (reverse []) = reverse [] = [] = id []

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys).
Supongamos que P(ys) se cumple, es decir, reverse (reverse ys) = ys.
Ahora, debemos demostrar que reverse (reverse (y : ys)) = y : ys

reverse (reverse (y : ys)) 
= reverse (y : reverse ys)                  (def. reverse)
= reverse (reverse ys ++ [y])               (prop. reverse sobre listas)
= reverse [y] ++ reverse (reverse ys)       (prop. reverse)
= [y] ++ ys                                 (HI)
= y : ys  

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

#### II. append = (++)
```
Por principio de extensionalidad funcional (ver ejercicio 2) basta ver que:
∀ xs :: [a]. ∀ ys :: [a]. append xs ys = xs ++ ys

Por inducción de listas sobre xs, necesitamos probar que:
∀ xs :: [a]. P(xs): ∀ ys :: [a]. append xs ys = (++) xs ys = xs ++ ys

Caso base: xs = []
append [] ys 
= ys                     (append)
= foldr (:) ys []        (foldr)
= [] ++ ys               (++)
= (++) [] ys

Caso recursivo: xs = z : zs

∀z :: a. ∀zs :: [a]. P(zs) ⇒ P(z:zs).
Supongamos que P(zs) se cumple, es decir, ∀ ys :: [a]. append zs ys = (++) zs ys.
Ahora, debemos demostrar que ∀ ys :: [a]. append (z:zs) ys = (++) (z:zs) ys.

append (z:zs) ys
= z : append zs ys          (append)
= z : (zs ++ ys)            (HI)
= z : (foldr (:) ys zs)     (++)
= (:) z (foldr (:) ys zs)
= foldr (:) ys (z:zs)       (foldr)
= (z:zs) ++ ys              (++)
= (++) (z:zs) ys

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

#### III. map id = id
```
Por principio de extensionalidad funcional (ver ejercicio 2) basta ver que:
∀xs :: [a]. map id xs = id xs.

Por inducción de listas sobre xs, necesitamos probar que:
∀ xs :: [a]. P(xs): map id xs = id xs.

Caso base: xs = []
map id [] = [] = id []        

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys).
Supongamos que P(ys) se cumple, es decir, map id ys = id ys.
Ahora, debemos demostrar que map id (y:ys) = id (y:ys).

map id (y:ys)
= id y : map id ys            (map)
= id y : id ys                (HI)
= y:ys                        (id x2)
= id (y:ys)                   (id)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

#### IV. ∀ f::a->b . ∀ g::b->c . map (g . f) = map g . map f
```
Por principio de extensionalidad funcional (ver ejercicio 2) basta ver que:
∀f :: a -> b. ∀g :: b -> c. ∀xs :: [a]. map (g . f) xs = (map g . map f) xs.

Por inducción de listas sobre xs, necesitamos probar que:
∀ xs :: [a]. P(xs): ∀f :: a -> b. ∀g :: b -> c. map (g . f) xs = (map g . map f) xs.

Caso base: xs = []
map (g . f) []
= []                          (map)
= map f []                    (map)
= map g (map f [])            (map)
= (map g . map f) []          (def .)

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys).
Supongamos que P(ys) se cumple, es decir, ∀f :: a -> b. ∀g :: b -> c. map (g . f) ys = (map g . map f) ys.
Ahora, debemos demostrar que ∀f :: a -> b. ∀g :: b -> c. map (g . f) (y:ys) = (map g . map f) (y:ys).

map (g . f) (y:ys)
= (g . f) y : map (g . f) ys        (map)
= (g . f) y : (map g . map f) ys    (HI)
= (g (f y)) : (map g (map f ys))    (def . x2)
= map g (f y : (map f ys))          (map sobre g)
= map g (map f (y:ys))              (map sobre f)
= (map g . map f) (y:ys)            (def .)

Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

#### V. ∀ f::a->b . ∀ p::b->Bool . map f . filter (p . f) = filter p . map f
```
Por principio de extensionalidad funcional (ver ejercicio 2) basta ver que:
∀f :: a -> b. ∀p :: b -> Bool. ∀xs :: [a]. (map f . filter (p . f)) xs = (filter p . map f) xs.

Por inducción de listas sobre xs, necesitamos probar que:
∀ xs :: [a]. P(xs): ∀f :: a -> b. ∀p :: b -> Bool. (map f . filter (p . f)) xs = (filter p . map f) xs.

Caso base: xs = []
(map f . filter (p . f)) []
= map f (filter (p . f) [])             (def .)
= map f []                              (filter)
= filter p (map f [])                   (filter)
= (filter p . map f) []                 (def .)

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys).
Supongamos que P(ys) se cumple, es decir, ∀f :: a -> b. ∀p :: b -> Bool. (map f . filter (p . f)) ys = (filter p . map f) ys.
Ahora, debemos demostrar que ∀f :: a -> b. ∀p :: b -> Bool. (map f . filter (p . f)) (y:ys) = (filter p . map f) (y:ys).

(map f . filter (p . f)) (y:ys)
= map f (filter (p . f) (y:ys))         (def .)
= map f (if (p . f) y then y : filter (p . f) ys else filter (p . f) ys)    (filter)

Por extensionalidad de booleanos, analizo los casos por separado:
1. Si (p . f) y = False

= map f (filter (p . f) ys)
= (map f . filter (p . f)) ys           (def .)
= (filter p . map f) ys                 (HI)

2. Si (p . f) y = True

= map f (y : filter (p . f) ys)
= f y : (map f (filter (p . f) ys))     (map)
= f y : ((map f . filter (p . f)) ys)   (def .)
= f y : ((filter p . map f) ys)         (HI)
= f y : (filter p (map f ys))           (def .)
= filter p (f y : (map f ys))           (filter y caso 2)
= filter p (map f (y:ys))               (map)
= (filter p . map f) (y:ys)             (def .)


Como ya lo probé para todo los casos posibles. Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

#### VI. ∀ f::a->b . ∀ e::a . ∀ xs::[a] . ((elem e xs) ⇒ (elem (f e) (map f xs))) (Asumiendo Eq a y Eq b)
```
Por inducción de listas sobre xs, necesitamos probar que:
∀ xs :: [a]. P(xs): ∀f :: a -> b. ∀e :: a. (elem e xs) ⇒ (elem (f e) (map f xs)).

Caso base: xs = []
elem e []
= False                               (elem)
= False
= False ⇒ (elem (f e) (map f []))                
Como false implica cualquier cosa, vale el caso base.

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys).
Supongamos que P(ys) se cumple, es decir, ∀f :: a -> b. ∀e :: a. (elem e ys) ⇒ (elem (f e) (map f ys))..
Ahora, debemos demostrar que ∀f :: a -> b. ∀e :: a. (elem e (y:ys)) ⇒ (elem (f e) (map f (y:ys)))..

(elem e (y:ys))
= (e == y || elem e ys)

Por extensionalidad de booleanos, analizo los casos por separado:
1. Si (e == y) = False

= (False || elem e ys)
= elem e ys                               (def ||)
= elem e ys ⇒ (elem (f e) (map f ys))     (HI)

2. Si (e == y) = True

= (True || elem e ys)
= True                                    (def ||)

Qvq True ⇒ (elem (f e) (map f (y:ys))) para que valga el caso recursivo.
Para eso veo: elem (f e) (map f (y:ys)) = True sabiendo que e == y

elem (f e) (map f (y:ys)) = True
= elem (f e) (f y : (map f ys)) = True                  (map)
= (f e == f y || elem (f e) (map f ys)) = True          (elem)
= (f y == f y || elem (f e) (map f ys)) = True          (hipótesis)
= (True || elem (f e) (map f ys)) = True
= True = True                                           (def ||)
= True

Como ya lo probé para todo los casos posibles. Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

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
Por el principio de extensionalidad funcional (ver ejercicio 2) basta ver que:
∀ xs :: [a]. ∀ ys :: [b]. zip xs ys = zip' xs ys

Por inducción de listas sobre xs necesito probar que:
∀ xs :: [a]. P(xs): ∀ ys :: [b]. zip xs ys = zip' xs ys

Caso base: P([])

zip [] ys
= foldr (\x rec ys -> if null ys then [] else (x, head ys) : rec (tail ys)) (const []) [] ys     (Z0)
= const [] ys                         (def foldr)
= []                                  (def const)
= zip' [] ys                          (Z'0)

Caso recursivo: xs = z:zs

∀z :: a. ∀zs :: [a]. P(zs) ⇒ P(z:zs).
Supongamos que P(zs) se cumple, es decir, ∀ ys :: [b]. zip zs ys = zip' zs ys.
Ahora, debemos demostrar que ∀ ys :: [b]. zip (z:zs) ys = zip' (z:zs) ys.

zip (z:zs) ys
= foldr f (const []) (z:zs) ys         (Z0)
= f z (foldr f (const []) zs) ys       (def foldr)
= if null ys then [] else (z, head ys) : (foldr f (const []) zs (tail ys))     (def f)
= if null ys then [] else (z, head ys) : (zip zs (tail ys))                    (Z0)
= if null ys then [] else (z, head ys) : (zip' zs (tail ys))                   (HI)
= zip' (z:zs) ys                                                               (Z'1)

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

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys).
Supongamos que P(ys) se cumple, es decir, ∀e :: a. ∀p :: a -> Bool. (elem e ys && p e) = elem e (filter p ys).
Ahora, debemos demostrar que ∀e :: a. ∀p :: a -> Bool. (elem e (y:ys) && p e) = elem e (filter p (y:ys)). 

Lado derecho:
elem e (filter p (y:ys))
= elem e (if p y then y : filter p ys else filter p ys)  (def filter)

Por extensionalidad de booleanos, analizo los casos por separado:

Caso 1: Si p y = True

elem e (if True then y : filter p ys else filter p ys)
= elem e (y : filter p ys)                                (def if)
= (e == y) || elem e (filter p xs)                        (def elem)
= (e == y) || (elem e ys && p e)                          (HI)
= (e == y || elem e ys) && (e == y || p e)                (distributiva del ||)
= elem e (y:ys) && (e == y || p e)                        (def elem)

Lado izquierdo:
elem e (y:ys) && p e
= (e == y || elem e ys) && p e                            (def elem)

Por extensionalidad de booleanos, analizo los casos por separado:

Caso 1.1: Si e == y = False

(e == y || elem e ys) && p e
= (False || elem e ys) && p e
= elem ys && p e
= elem e (filter p ys)                                    (HI)

Caso 1.2: Si e == y = True

(e == y || elem e ys) && p e
= (True || elem e ys) && p e
= True && p e                                             (def ||)
Como e == y y estoy en el caso 1 donde p y = True queda:
= True

Caso 2: Si p y = False

elem e (filter p (y:ys))
= elem e (filter p ys)

Lado izquierdo:
elem e (y:ys) && p e
= (e == y || elem e ys) && p e

Lo separo en dos subcasos:

Subcaso 2.1: Si e == y
Si e == y, entonces p y = False, lo que implica que p e = False.
Entonces, ambos lados son False.

Subcaso 2.2: Si e == y = False
(False || elem e ys) && p e = elem e ys && p e
elem e ys && p e = elem e (filter p ys)
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

Caso recursivo: xs = y:ys

∀y :: a. ∀ys :: [a]. P(ys) ⇒ P(y:ys).
Supongamos que P(ys) se cumple, es decir, ∀ e::a . elem e ys = elem e (nub ys).
Ahora, debemos demostrar que ∀ e::a . elem e (y:ys) = elem e (nub (y:ys)).

Lado izquierdo:
elem e (y:ys)
= e == y || elem e ys

Lado derecho:
elem e (nub (y:ys))
= elem e (y : filter (\y -> x /= y) (nub ys))          (N1)
= e == y || elem e (filter (\y -> x /= y) (nub ys))    (def elem)

Por extensionalidad de booleanos, analizo los casos por separado:

1. Si e == y = True
Lado izquierdo:
= e == y || elem e ys
= True || elem e ys

Lado derecho:
= e == y || elem e (filter (\y -> x /= y) (nub ys))    (def elem)
= True || ...
= True

2. Si e == y = False
Lado izquierdo:
= e == y || elem e ys
= False || elem e ys
= elem e ys

Lado derecho:
= e == y || elem e (filter (\y -> x /= y) (nub ys))    (def elem)
= False || elem e (filter (\y -> x /= y) (nub ys))
= elem e (filter (\y -> x /= y) (nub ys))

Qvq elem e ys = elem e (filter (\y -> x /= y) (nub ys))
    elem e ys = elem e (filter (\y -> x /= y) (nub ys))
    elem e ys = elem e (nub ys) && (\y -> x /= y) e      (uso el 6.I)

Como estoy en el donde e == y = False, la condición "(\y -> x /= y) e" se cumple trivialmente. Y por la conjunción de booleanos, True && cualquier cosa = cualquier cosa.

    elem e ys = elem e (nub ys)
    elem e ys = elem e ys                         (HI)                          

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

Caso recursivo: xs = z:zs

∀z :: a. ∀zs :: [a]. P(zs) ⇒ P(z:zs).
Supongamos que P(zs) se cumple, es decir, ∀ e::a . elem e (union zs ys) = (elem e zs) || (elem e ys).
Ahora, debemos demostrar que ∀ e::a . elem e (union (z:zs) ys) = (elem e (z:zs)) || (elem e ys).

Lado izquierdo:
elem e (union (z:zs) ys)
= elem e (nub ((z:zs) ++ ys))                                  (U0)
= elem e (nub (z : (zs ++ ys)))                                (def ++) 
= elem e (z : filter (\y -> x /= y) (nub (zs++ys)))            (N1)
= e == z || elem e (filter (\y -> x /= y) (nub (zs ++ ys)))    (def elem)

Lado derecho:
(elem e (z:zs)) || (elem e ys)
= e == z || elem e zs || elem e ys                             (def elem)

Por extensionalidad de booleanos, analizo los casos por separado:

1. Si e == z = True

Lado izquierdo:
= e == z || elem e (filter (\y -> x /= y) (nub (zs ++ ys)))
= True || ...
= True                       

Lado derecho:
= e == z || elem e zs || elem e ys
= True || ... || ...
= True

2. Si e == z = False

Lado izquierdo:
= e == z || elem e (filter (\y -> x /= y) (nub (zs ++ ys)))
= False || elem e (filter (\y -> x /= y) (nub (zs ++ ys)))
= elem e (filter (\y -> x /= y) (nub (zs ++ ys)))
= elem e (nub (zs ++ ys)) && (\y -> x /= y) e                    (uso el 6.I)

Como estoy en el donde e == x = False, la condición "(\y -> x /= y) e" se cumple trivialmente. Y por la conjunción de booleanos, True && cualquier cosa = cualquier cosa.

= elem e (nub (zs ++ ys))
= elem e (union zs ys)                                   (U0)
= (elem e zs) || (elem e ys)                             (HI)

Lado derecho:
= e == z || elem e zs || elem e ys
= False || elem e zs || elem e ys
= elem e zs || elem e ys


Como ya demostré que la proposición se cumple para todos los casos.
Por lo tanto, la propiedad vale para ∀ xs :: [a].
```

#### IV. Eq a => ∀ xs::[a] . ∀ ys::[a] . ∀ e::a . elem e (intersect xs ys) = (elem e xs) && (elem e ys)
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

Caso recursivo: xs = z:zs

∀z :: a. ∀zs :: [a]. P(zs) ⇒ P(z:zs).
Supongamos que P(zs) se cumple, es decir, ∀ ys::[a] . ∀ e::a . elem e (intersect zs ys) = (elem e zs) && (elem e ys).
Ahora, debemos demostrar que ∀ ys::[a] . ∀ e::a . elem e (intersect (z:zs) ys) = (elem e (z:zs)) && (elem e ys).

Lado izquierdo:
elem e (intersect (z:zs) ys)
= e == z || (elem e (intersect zs ys))              (elem)
= e == z || ((elem e zs) && (elem e ys))            (HI)

Por extensionalidad de booleanos, analizo los casos por separado:

1. Si (e == z) = True

= True || ((elem e zs) && (elem e ys))
= True

2. Si (e == z) = False

= False || ((elem e zs) && (elem e ys))
= (elem e zs) && (elem e ys)

Lado derecho:
(elem e (z:zs)) && (elem e ys)
= ((e == z) || elem e zs) && (elem e ys)

Comparación de casos

1. con lado derecho:
True = ((e == z) || elem e zs) && (elem e ys)
True = (True || ...) && (elem e ys)                       (hipótesis del 1.)
True = True && (elem e ys)

En el caso e == z, sabemos que e está en la intersección (intersect xs ys) porque, según la definición de intersect, este filtra los elementos de xs que también están en ys. Como e == z y z pertenece a xs, entonces e sólo puede aparecer en la intersección si también pertenece a ys. Por lo tanto, elem e ys = True.

2. con lado derecho:
(elem e zs) && (elem e ys) = ((e == z) || elem e zs) && (elem e ys)
(elem e zs) && (elem e ys) = (False || elem e zs) && elem e ys     (hipótesis del 2.)
(elem e zs) && (elem e ys) = (elem e zs) && (elem e ys)


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

Caso recursivo: xs = z:zs

∀z :: a. ∀zs :: [a]. P(zs) ⇒ P(z:zs).
Supongamos que P(zs) se cumple, es decir, ∀ ys::[a] . length (union zs ys) ≤ length zs + length ys.
Ahora, debemos demostrar que ∀ ys::[a] . length (union (z:zs) ys) ≤ length (z:zs) + length ys.

length (union (z:zs) ys) ≤ length (z:zs) + length ys
= length (nub ((z:zs)++ys)) ≤ 1 + length zs + length ys                            (U0 y length)
= length (nub (z:(zs++ys))) ≤ 1 + length zs + length ys                            (++)
= length (z : filter (\y -> x /= y) (nub (zs++ys))) ≤ 1 + length zs + length ys    (nub)
= 1 + length (filter (\y -> x /= y) (nub (zs++ys))) ≤ 1 + length zs + length ys    (length)
= length (filter (\y -> x /= y) (nub (zs++ys))) ≤ length zs + length ys            (prop de int)
= length (union zs ys) ≤ length zs + length ys                                     (nub)
Y esto vale por la hipótesis inductiva.


Como ya demostré que la proposición se cumple para todos los casos.
Por lo tanto, la propiedad vale para ∀ xs :: [a].
```
[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 7
Dadas las definiciones usuales de foldr y foldl, demostrar las siguientes propiedades:

#### I. ∀ f::a->b->b . ∀ z::b . ∀ xs, ys::[a] . foldr f z (xs ++ ys) = foldr f (foldr f z ys) xs

#### II. ∀ f::b->a->b . ∀ z::b . ∀ xs, ys::[a] . foldl f z (xs ++ ys) = foldl f (foldl f z xs) ys


[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 8
Demostrar que la función potencia definida en la práctica 1 usando foldNat funciona correctamente mediante inducción en el exponente.


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
Dada la siguiente función:
```haskell
     truncar :: AB a -> Int -> AB a
{T0} truncar Nil \_ = Nil
{T1} truncar (Bin i r d) n = if n == 0 then Nil else Bin (truncar i (n-1)) r (truncar d (n-1))
```
Y los siguientes lemas:

1. ∀ x::Int . ∀ y::Int . ∀ z::Int . max (min x y) (min x z) = min x (max y z)
2. ∀ x::Int . ∀ y::Int . ∀ z::Int . z + min x y = min (z+x) (z+y)

Demostrar las siguientes propiedades:

#### I. ∀ t :: AB a . altura t ≥ 0

#### II. ∀ t::AB a . ∀ n::Int . (n ≥ 0 ⇒ (altura (truncar t n) = min n (altura t)))

[Volver al indice](#práctica-2---razonamiento-ecuacional-e-inducción-estructural)

### Ejercicio 11

Considerar las siguientes funciones:

```haskell
     inorder :: AB a -> [a]
{I0} inorder = foldAB [] (\ri x rd -> ri ++ (x:rd))
     elemAB :: Eq a => a -> AB a -> Bool
{A0} elemAB e = foldAB False (\ri x rd -> (e == x) || ri || rd)
     elem :: Eq a => [a] -> Bool
{E0} elem e = foldr (\x rec -> (e == x) || rec) False
```

Demostrar la siguiente propiedad:
#### Eq a => ∀ e::a . elemAB e = elem e . inorder


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

