module Test where
import Test.HUnit
import TP 

run = runTestTT tests

tests = test (testNombresDeUsuarios ++ testAmigosDe ++ testCantidadDeAmigos ++ testUsuarioConMasAmigos ++ testPublicacionesDe)

testNombresDeUsuarios = [
    " devuelve los nombres de usuarios de la red" ~: nombresDeUsuarios red1 ~?= ["Pablo", "Juan", "Juana", "Valentina"],
    " no devuelve repetidos" ~: nombresDeUsuarios red2 ~?= ["Pablo", "Juan", "Juana", "Valentina"],
    " una red sin ususarios devuelve []" ~: nombresDeUsuarios redVacia ~?= []]

testAmigosDe = [
    " devuelve todos los amigos del usuario" ~: amigosDe red1 usuario1 ~?= [usuario3, usuario4, usuario2],
    " un usuario sin amigos devuelve []" ~: amigosDe red2 usuario4 ~?= []]

testCantidadDeAmigos = [
    " devuelve la cantidad de amigos del usuario" ~: cantidadDeAmigos red1 usuario1 ~?= 3]
testUsuarioConMasAmigos = [
    "devuelve el usuario con mas amigos" ~: usuarioConMasAmigos red1 ~?= usuario1 ,
    "devuelve el ultimo usuario con la mayor cantidad de amigos" ~: usuarioConMasAmigos red2 ~?= usuario3]
testEstaRobertoCarlos = []
testPublicacionesDe = [
    " devuelve todas las publicaciones del usuario" ~: publicacionesDe red1 usuario2 ~?= [publicacion3_us2, publicacion2_us2, publicacion1_us2],
    " si el usuario no tiene publicaciones devuelve vacio" ~: publicacionesDe red1 usuario3 ~?= []]
testPublicacionesQueLeGustanA = 
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
usuarios2 :: [Usuario]
usuarios2 = [usuario1, usuario2, usuario1, usuario2, usuario3, usuario4]

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
publicacion1_us1 = (usuario1, "primer post", [])
publicacion2_us1 :: Publicacion
publicacion2_us1 = (usuario1, "segundo post", [usuario2, usuario3, usuario4, usuario2])

publicacion1_us2 :: Publicacion
publicacion1_us2 = (usuario2, "primer post", [usuario1, usuario4, usuario3, usuario3])
publicacion2_us2 :: Publicacion
publicacion2_us2 = (usuario2, "segundo post", [usuario3, usuario4])
publicacion3_us2 :: Publicacion
publicacion3_us2 = (usuario2, "tercer post", [usuario3, usuario1])


publicaciones1 :: [Publicacion]
publicaciones1 = [publicacion3_us2, publicacion2_us2, publicacion1_us2, publicacion2_us1, publicacion1_us1]

red1 :: RedSocial
red1 = (usuarios1, relaciones1, publicaciones1)
red2 :: RedSocial
red2 = (usuarios2, relaciones2, publicaciones1)
redVacia :: RedSocial
redVacia = ([], [], [])
