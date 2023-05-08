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

-- Predicados 

-- Indica si un valor de entrada es un elemento en una lista determinada.
pertenece :: Eq t => t -> [t] -> Bool
pertenece x ys 
        | ys == [] = False
        | x == head ys = True 
        | otherwise = (pertenece x (tail ys)) 

-- Indica si una lista contiene los mismos elementos sin importar el orden ni las repeticiones 
mismoselementos :: Eq t => [t] -> [t] -> Bool
mismoselementos l1 l2 = mismoselementosAux l1 l2 && mismoselementosAux l2 l1

mismoselementosAux :: Eq t => [t] -> [t] -> Bool
mismoselementosAux [x] ys = pertenece x ys
mismoselementosAux (x:xs) ys
                | pertenece x ys == False = False
                | otherwise = mismoselementosAux xs ys && pertenece x ys

-- Provee el elemento final de una lista             
terminacon :: [t] -> t
terminacon xs = head (reverse xs)

-- Verifica que tiene id y que tiene nombre 
usuarioValido :: Usuario -> Bool
usuarioValido u = idDeUsuario u > 0 && (length (nombreDeUsuario u)) > 0


incluido :: Eq t => [t] -> [t] -> Bool
incluido [] _ = True
incluido l1 l2 | pertenece (head l1) l2 = incluido (tail l1) l2
               | otherwise = False

sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red us = incluido us (usuarios red)
