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

-- Ejercicios
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

-- Predicados

-- Indica si un valor de entrada es un elemento en una lista determinada.
pertenece :: Eq t => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys)
        | x == y = True 
        | otherwise = pertenece x ys

-- Indica si una lista contiene los mismos elementos sin importar el orden ni las repeticiones              
mismoselementos :: Eq t => [t] -> [t] -> Bool
mismoselementos l1 l2 = incluido l1 l2 && incluido l2 l1
-- Provee el elemento final de una lista             
terminacon :: [t] -> t
terminacon xs = head (reverse xs)
-- Verifica que tiene id y que tiene nombre 
usuarioValido :: Usuario -> Bool
usuarioValido u = idDeUsuario u > 0 && (length (nombreDeUsuario u)) > 0
incluido :: Eq t => [t] -> [t] -> Bool
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

-- Publicaciones Validas
publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us pubs = incluido (listaDeUsuariosPubs pubs, us) && likeDePublicacionSonUsuariosDeRed us pubs && noHayPublicacionesRepetidas pubs

likeDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
likeDePublicacionSonUsuariosDeRed us ((u,pub, likes) : pubs) = incluido likes us && likeDePublicacionSonUsuariosDeRed us pubs

listadeIds :: [Usuario] -> [Integer]
listadeIds [] = []
listadeIds (x:xs) = idDeUsuario x : listadeIds xs

noHayIdsRepetidosaux :: [Integer] -> Bool
noHayIdsRepetidosaux (x:xs) = sinRepetidos (x:xs)

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos (x:xs) = noHayIdsRepetidosaux (listadeIds (x:xs))

noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas pubs = idsListasRepetidos (listaDeLikes pubs) && noHayIdsRepetidos  (listaDeUsuariosPubs pubs)

idsListasRepetidos :: [[Usuario]] -> Bool
idsListasRepetidos (x:xs) = noHayIdsRepetidos x && idsListasRepetidos xs

listaDeLikes :: [Publicacion] -> [[Usuario]]
listaDeLikes [] = []
listaDeLikes (x:xs) = likesDePublicacion x : listaDeLikes xs

listaDeUsuariosPubs :: [Publicacion] -> [Usuario]
listaDeUsuariosPubs [] = []
listaDeUsuariosPubs (x:xs) = usuarioDePublicacion x : listaDeUsuariosPubs xs

--usuariosValidos
usuariosValidosAux :: [Usuario] -> Bool
usuariosValidosAux [] = True
usuariosValidosAux (x:xs) | usuarioValido (x) == False = False
                          | usuarioValido (x) = usuariosValidosAux (xs)

usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = False
usuariosValidos us = usuariosValidosAux (us) && noHayIdsRepetidos (us)

empiezaCon :: [t] -> t
empiezaCon xs = head xs
