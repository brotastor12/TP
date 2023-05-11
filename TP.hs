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


publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us pubs = incluido (listaDeUsuariosPubs pubs) us && likeDePublicacionSonUsuariosDeRed us pubs && noHayPublicacionesRepetidas pubs


likeDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
likeDePublicacionSonUsuariosDeRed us ((u,pub, likes) : pubs) = incluido likes us && likeDePublicacionSonUsuariosDeRed us pubs
