module Solucion where

-- Nombre de Grupo: PuntoDeEncuentro
-- Integrante 1: Astor Martinez Belizzi, astormbelizzi@gmail.com, 781/21
-- Integrante 2: Gonzalo Nicolas Moreira Valdez, gn.moreiravaldez@gmail.com, 712/23
-- Integrante 3: Iván Alejandro Miguel Viola, ivanmiguelviola@gmail.com, 711/23
-- Integrante 4: Andreas Pierre André Giacomini, andreasgiacomini2@gmail.com, 772/23

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas
usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicio 1
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios red)

proyectarNombres :: [Usuario] -> [String]
proyectarNombres us | sinRepetidos listaNombres = listaNombres
                    | otherwise = sacarRepetidos listaNombres
                    where listaNombres = proyectarNombresAux us
-- proyectarNombres revisa si hay repetidos
proyectarNombresAux :: [Usuario] -> [String]
proyectarNombresAux [] = []
proyectarNombresAux (x:xs) = nombreDeUsuario x : proyectarNombres xs
-- proyectarNombresAux crea una lista con los nombres de usuario

-- Ejercicio 2
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red us = amigosDeAux (relaciones red) us

amigosDeAux :: [Relacion] -> Usuario -> [Usuario]
amigosDeAux [] _ = []
amigosDeAux ((r1,r2):xs) us | r1 == us = r2 : amigosDeAux xs us
                            | r2 == us = r1 : amigosDeAux xs us
                            | otherwise = amigosDeAux xs us

-- Ejercicio 3 

cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos rc us = longitud (amigosDe rc us)

-- Ejercicio 4 
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosAux red (usuarios red)

comparacionAmigos :: RedSocial -> Usuario -> Usuario -> Usuario
comparacionAmigos red x y | (cantidadDeAmigos red x) > (cantidadDeAmigos red y) = x
                          | otherwise = y

usuarioConMasAmigosAux :: RedSocial -> [Usuario] -> Usuario     
usuarioConMasAmigosAux _ [x] = x
usuarioConMasAmigosAux red (x:y:xs) = usuarioConMasAmigosAux red (masAmigosxy : xs)                       
                                    where masAmigosxy = comparacionAmigos red x y

{- usuarioConMasAmigosAux compara quien tiene mas amigos entre los primeros 2 elementos y hace una recursion 
   mantniendo el mas grande en la cabecera. Al final de la recursion solo queda el usuario con mas amigos-}

-- Ejercicio 5 

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = estaRobertoCarlosAux red (usuarios red)

estaRobertoCarlosAux :: RedSocial -> [Usuario] -> Bool
estaRobertoCarlosAux _ [] = False
estaRobertoCarlosAux red (x:xs) | (cantidadDeAmigos red x) > 10 = True
                            | otherwise = estaRobertoCarlosAux red xs
                            
-- Ejercicio 6
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red us = publicacionesDeAux (publicaciones red) us


publicacionesDeAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAux [] _ = []
publicacionesDeAux (x:xs) us | usuarioDePublicacion x == us = x : publicacionesDeAux xs us
                             | otherwise = publicacionesDeAux xs us
--La funcion auxiliar crea la lista de publicaciones del usuario

-- Ejercicio 7
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red us = sacarRepetidos (publicacionesQueLeGustanAAux (publicaciones red) us)

publicacionesQueLeGustanAAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAAux [] _ = []
publicacionesQueLeGustanAAux (x:xs) us | pertenece us (likesDePublicacion x) = x : publicacionesQueLeGustanAAux xs us
                                       | otherwise = publicacionesQueLeGustanAAux xs us

-- Ejercicio 8
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = (mismoselementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2))

-- Ejercicio 9
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red us | publicacionesDe red us /= [] = tieneUnSeguidorFielAux red (usuarios red) us

tieneUnSeguidorFielAux :: RedSocial -> [Usuario] -> Usuario -> Bool
tieneUnSeguidorFielAux red [] us = False
tieneUnSeguidorFielAux red (seguidor:xs) us | seguidor == us = tieneUnSeguidorFielAux red xs us
                                            | esFiel red us seguidor = True
                                            | otherwise = tieneUnSeguidorFielAux red xs us
-- Revisa si us tiene un seguidor fiel con una recursion y si lo encuentra devuelve True. 
-- Si termina la recursion sin encontrar uno da False

esFiel :: RedSocial -> Usuario -> Usuario -> Bool
esFiel red us seguidor = incluido (publicacionesDe red us) (publicacionesQueLeGustanA red seguidor)
-- seguidor es fiel si le dio like a todas las publicaciones del us

-- Ejercicio 10

-- Verifica si existe una cadena de amigos como la planteada en cadenaDeAmigos entre usuarios que le demos
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red us1 us2 = existeSecuenciaDeAmigosAux red (usuarios red) us1 us2

-- Verifica que para la lista creada en armarCadena, haya una cadena de amigos y que esta tambien empiece con el usuario 1 y termine con el usuario 2. 
existeSecuenciaDeAmigosAux :: RedSocial -> [Usuario] -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigosAux red us u1 u2 = cadenaDeAmigos z red && empiezaCon u1 z && terminaCon u2 z
                                                where z = (armarCadena us u1 u2)

-- Crea una lista de usuarios con los usuarios entre us1 y us2 inclusive
-- Si no se cumplen las condiciones, remueve elementos de la lista recursivamente hasta que se cumplan o que se genere una lista de longitud menor o igual a 2
armarCadena :: [Usuario] -> Usuario -> Usuario -> [Usuario]
armarCadena (x:xs) u1 u2 | longitud (x:xs) <= 2 = (x:xs)
                         | x == u1 && terminaCon u2 (x:xs) = (x:xs)
                         | x /= u1 = armarCadena xs u1 u2
                         | otherwise = armarCadena (quitarUltimo (x:xs)) u1 u2

quitarUltimo :: (Eq t) => [t] ->[t]
quitarUltimo ls = quitar (ultimo ls) ls


-- Funciones auxiliares

-- Determina la longitud de una lista.
longitud :: (Eq t) => [t] -> Integer
longitud [] = 0
longitud (x:xs) = longitud xs + 1

-- Indica si un valor de entrada es un elemento en una lista determinada.
pertenece :: Eq t => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys)
        | x == y = True 
        | otherwise = pertenece x ys

sonAmigos :: Relacion -> RedSocial -> Bool
sonAmigos (u1, u2) red = pertenece (u1,u2) (relaciones red) || pertenece (u2,u1) (relaciones red)

-- Indica si una lista contiene los mismos elementos sin importar el orden ni las repeticiones              
mismoselementos :: Eq t => [t] -> [t] -> Bool
mismoselementos l1 l2 = incluido l1 l2 && incluido l2 l1

-- Verifica si entre un x cualquiera y una lista, ese x es el elemento final de la lista
terminaCon :: Eq t => t -> [t] -> Bool
terminaCon y xs = y == ultimo xs

-- Verifica que tiene id y que tiene nombre 
usuarioValido :: Usuario -> Bool
usuarioValido u = idDeUsuario u > 0 && (length (nombreDeUsuario u)) > 0

incluido :: Eq t => [t] -> [t] -> Bool
incluido [] _ = True
incluido (x:l1) l2 | pertenece x l2 = incluido l1 l2
                   | otherwise = False

sinRepetidos :: Eq t => [t] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) | pertenece x xs = False
                    | otherwise = sinRepetidos xs
                    
sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red us = incluido us (usuarios red)

sacarRepetidos :: Eq a => [a] -> [a]
sacarRepetidos [] = []
sacarRepetidos (x:xs) | pertenece x xs = sacarRepetidos xs
                      | otherwise = x : sacarRepetidos xs

-- Verifica si en la lista de usuarios hay relaciones en cadena, es decir us1 con us2, us2 con us3, us3 con us4 y asi sucesivamente
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [] _ = False
cadenaDeAmigos [x] _ = False
cadenaDeAmigos [x,y] red = sonAmigos (x,y) red
cadenaDeAmigos (x:y:xs) red | sonAmigos (x,y) red = cadenaDeAmigos (y:xs) red
                            | otherwise = False

quitar :: Eq t => t -> [t] -> [t]
quitar _ [] = []
quitar y (x:xs)
        | y == x = xs
        | y /= x = (x : (quitar y xs))

quitartodos :: Eq t => t -> [t] -> [t]
quitartodos y (x:xs) | pertenece y (x:xs) == True = quitartodos y (quitar y xs)
                     | otherwise = (x:xs)

empiezaCon :: Eq t => t -> [t] -> Bool
empiezaCon _ [] = False
empiezaCon y (x:xs) | (x == y) = True
                    | otherwise = False

ultimo :: [t] -> t
ultimo [] = undefined
ultimo [x] = x
ultimo (x:xs) = ultimo xs


