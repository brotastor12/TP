module Tests where
import Test.HUnit
import Solucion

run = runTestTT tests

tests = test (testNombresDeUsuarios ++ testAmigosDe ++ testCantidadDeAmigos ++ testUsuarioConMasAmigos ++ testEstaRobertoCarlos ++ testPublicacionesDe ++ testPublicacionesQueLeGustanA ++ testLesGustanLasMismasPublicaciones ++ testTieneUnSeguidorFiel ++ testExisteSecuenciaDeAmigos)

--Ejercicio 1
testNombresDeUsuarios = [
    " devuelve los nombres de usuarios de la red" ~: nombresDeUsuarios red1 ~?= ["Pablo", "Juan", "Juana", "Valentina"],
    " una red sin ususarios devuelve []" ~: nombresDeUsuarios redVacia ~?= []]

-- Ejercicio 2
testAmigosDe = [
    " devuelve todos los amigos del usuario" ~: amigosDe red1 usuario1 ~?= [usuario3, usuario4, usuario2],
    " un usuario sin amigos devuelve []" ~: amigosDe red2 usuario4 ~?= []]

-- Ejercicio 3
testCantidadDeAmigos = [
    " devuelve la cantidad de amigos del usuario" ~: cantidadDeAmigos red1 usuario1 ~?= 3]

--Ejercicio 4
testUsuarioConMasAmigos = [
    " devuelve el usuario con mas amigos" ~: usuarioConMasAmigos red1 ~?= usuario1 ,
    " devuelve el ultimo usuario con la mayor cantidad de amigos si hay mas de uno" ~: usuarioConMasAmigos red2 ~?= usuario3 ,
    " devuelve undefined si la red no tiene usuarios" ~: usuarioConMasAmigos redVacia ~?= undefined]

-- Ejercicio 5
testEstaRobertoCarlos = [
    " Si tiene mas de 10 amigos da True" ~: estaRobertoCarlos redRC ~?= True ,
    " Si tiene menos de 10 amigos da False" ~: estaRobertoCarlos red1 ~?= False ,
    " Si tiene 10 amigos da false" ~: estaRobertoCarlos redRC2 ~?= False]

-- Ejercicio 6
testPublicacionesDe = [
    " devuelve todas las publicaciones del usuario" ~: publicacionesDe red1 usuario2 ~?= [publicacion3_us2, publicacion2_us2, publicacion1_us2],
    " si el usuario no tiene publicaciones devuelve vacio" ~: publicacionesDe red1 usuario3 ~?= []]

-- Ejercicio 7
testPublicacionesQueLeGustanA = [
    " devuelve todas las publicaciones que le gustan al usuario" ~: publicacionesQueLeGustanA red1 usuario4 ~?= [publicacion1_us2, publicacion1_us1],
    " devuelve vacio si no le gusta ninguna publicacion" ~: publicacionesQueLeGustanA red1 usuario2 ~?= []]

--Ejercicio 8
testLesGustanLasMismasPublicaciones = [
    " si les gustan las mismas publicaciones devuelve True" ~: lesGustanLasMismasPublicaciones red1 usuario1 usuario4 ~?= True ,
    " si no les gustan las mismas publicaciones devuelve False" ~: lesGustanLasMismasPublicaciones red1 usuario1 usuario2 ~?= False]

--Ejercicio 9
testTieneUnSeguidorFiel = [
    " si existe un alguien que le gusten todas las publicaciones devuelve True" ~: tieneUnSeguidorFiel red1 usuario2 ~?= True ,
    " si no se cumple la condicion devuelve False" ~: tieneUnSeguidorFiel red1 usuario1 ~?= False]

-- Ejercicio 10
testExisteSecuenciaDeAmigos = [
    " devuelve True si existe una cadena de amigos entre los usuarios dados" ~: existeSecuenciaDeAmigos red1 usuario1 usuario3 ~?= True ,
    " devuelve False si no cumple la condicion" ~: existeSecuenciaDeAmigos red1 usuario1 usuario4 ~?= False ,
    " devuelve False si los usuarios dados no estan en orden" ~: existeSecuenciaDeAmigos red1 usuario3 usuario1 ~?= False]

usuario1 :: Usuario
usuario1 = (1, "Pablo")
usuario2 :: Usuario
usuario2 = (2, "Juan")
usuario3 :: Usuario
usuario3 = (3, "Juana")
usuario4 :: Usuario
usuario4 = (4, "Valentina")


usuarios1 :: [Usuario]
usuarios1 = [usuario1, usuario2, usuario3, usuario4]

relacion1 :: Relacion
relacion1 = (usuario1, usuario3)
relacion2 :: Relacion
relacion2 = (usuario2, usuario3)
relacion3 :: Relacion
relacion3 = (usuario1, usuario4)
relacion4 :: Relacion
relacion4 = (usuario2, usuario1)


relaciones1 :: [Relacion]
relaciones1 = [relacion1, relacion2, relacion3, relacion4]
relaciones2 :: [Relacion]
relaciones2 = [relacion1, relacion2, relacion4]



publicacion1_us1 :: Publicacion
publicacion1_us1 = (usuario1, "primer post", [usuario1, usuario4])
publicacion2_us1 :: Publicacion
publicacion2_us1 = (usuario1, "segundo post", [usuario3])

publicacion1_us2 :: Publicacion
publicacion1_us2 = (usuario2, "primer post", [usuario1, usuario4, usuario3])
publicacion2_us2 :: Publicacion
publicacion2_us2 = (usuario2, "segundo post", [usuario3])
publicacion3_us2 :: Publicacion
publicacion3_us2 = (usuario2, "tercer post", [usuario3])


publicaciones1 :: [Publicacion]
publicaciones1 = [publicacion3_us2, publicacion2_us2, publicacion1_us2, publicacion2_us1, publicacion1_us1]

red1 :: RedSocial
red1 = (usuarios1, relaciones1, publicaciones1)
red2 :: RedSocial
red2 = (usuarios1, relaciones2, publicaciones1)
redVacia :: RedSocial
redVacia = ([], [], [])

-- Roberto Carlos
-- usuarios para Roberto Carlos
usuario5 = (5, "Martin")
usuario6 = (6, "Nicolas")
usuario7 = (7, "Martina")
usuario8 = (8, "Felipe")
usuario9 = (9, "Julia")
usuario10 = (10, "Ariel")
usuario11 = (11, "Katia")
usuario12 = (12, "Franco")
robertoCarlos = (13, "Roberto Carlos")

usuariosRC :: [Usuario]
usuariosRC = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12, robertoCarlos]

-- Relaciones para roberto Carlos
relacionRC1 = (robertoCarlos, usuario2)
relacionRC2 = (robertoCarlos, usuario3)
relacionRC3 = (robertoCarlos, usuario4)
relacionRC4 = (robertoCarlos, usuario5)
relacionRC5 = (robertoCarlos, usuario6)
relacionRC6 = (robertoCarlos, usuario7)
relacionRC7 = (robertoCarlos, usuario8)
relacionRC8 = (robertoCarlos, usuario9)
relacionRC9 = (robertoCarlos, usuario10)
relacionRC10 = (robertoCarlos, usuario11)
relacionRC11 = (robertoCarlos, usuario12)

-- Relaciones para roberto Carlos
relacionesRC :: [Relacion]
relacionesRC = [relacionRC1, relacionRC2, relacionRC3, relacionRC4, relacionRC5, relacionRC6, relacionRC7, relacionRC8, relacionRC9, relacionRC10, relacionRC11]
relacionesRC2 :: [Relacion]
relacionesRC2 = [relacionRC1, relacionRC2, relacionRC3, relacionRC4, relacionRC5, relacionRC6, relacionRC7, relacionRC8, relacionRC9, relacionRC10]

-- Red para Roberto Carlos
redRC = (usuariosRC, relacionesRC, [])
redRC2 = (usuariosRC, relacionesRC2, [])
