import Text.Show.Functions

type Accion = Participante -> Participante

data Participante = UnParticipante{
    nombre :: String,
    cantidadDeDinero :: Int,
    tacticaDeJuego :: String,
    propiedadesCompradas :: [(String,Int)],
    accionesDeParticipante :: [Accion]
} deriving (Show)

type Propiedad = (String,Int)

edificio :: Propiedad
edificio = ("Edificio Blanco",100)

carolina :: Participante
carolina = UnParticipante "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]
manuel :: Participante
manuel = UnParticipante "Manuel" 800 "Oferente Singular" [] [pasarPorElBanco, enojarse]
pedro :: Participante
pedro = UnParticipante "Pedro" 80 "Apostador" [edificio] [pasarPorElBanco]
juan :: Participante
juan = UnParticipante "j" 80 "Apostador" [] [pasarPorElBanco]

-----FUNCIONES AUXILIARES-----
modificarDinero :: (Int->Int->Int)->Participante->Int->Participante
modificarDinero unaFuncion unParticipante cantidad= unParticipante {cantidadDeDinero = unaFuncion (cantidadDeDinero unParticipante) cantidad}

modificarAcciones :: Participante->Accion->Participante
modificarAcciones unParticipante unaAccion = unParticipante {accionesDeParticipante = unaAccion : (accionesDeParticipante unParticipante)}

verificarTacticaDeJuego :: Participante->String->Bool
verificarTacticaDeJuego unParticipante tactica = (==tactica) (tacticaDeJuego unParticipante)

verificarAptoSubasta :: Participante->Bool
verificarAptoSubasta unParticipante = (verificarTacticaDeJuego unParticipante "Accionista") || (verificarTacticaDeJuego unParticipante "Oferente Singular") 

verificarAptoCompra :: Participante->Propiedad->Bool
verificarAptoCompra unParticipante unaPropiedad = cantidadDeDinero unParticipante >= snd unaPropiedad    

propiedadesBaratasDeParticipante :: Participante->Int
propiedadesBaratasDeParticipante unParticipante = length (filter (<150) (map snd (propiedadesCompradas unParticipante)))

propiedadesCarasDeParticipante :: Participante->Int
propiedadesCarasDeParticipante unParticipante = length (filter (>=150) (map snd (propiedadesCompradas unParticipante)))

comprar :: Propiedad->Accion
comprar unaPropiedad unParticipante = (modificarDinero (-) unParticipante (snd unaPropiedad)) {propiedadesCompradas = unaPropiedad : (propiedadesCompradas unParticipante)}
-----FIN FUNCIONES AUXILIARES-----

pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = (modificarDinero (+) unParticipante 40) {tacticaDeJuego = "Comprador Compulsivo"}

enojarse :: Accion
enojarse unParticipante = modificarAcciones (modificarDinero (+) unParticipante 50) gritar

gritar :: Accion
gritar unParticipante = unParticipante {nombre = "AHHHH " ++ nombre unParticipante}

subastar :: Propiedad->Accion
subastar unaPropiedad unParticipante | (verificarAptoSubasta unParticipante && verificarAptoCompra unParticipante unaPropiedad) = comprar unaPropiedad unParticipante
                                     | otherwise = unParticipante 

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = modificarDinero (+) unParticipante (10 * (propiedadesBaratasDeParticipante unParticipante) + 20 * (propiedadesCarasDeParticipante unParticipante)) 

pagarAAccionistas :: Accion
pagarAAccionistas unParticipante | verificarTacticaDeJuego unParticipante "Accionista" = modificarDinero (+) unParticipante 200
                                 | otherwise = modificarDinero (-) unParticipante 100                      

hacerBerrinchePor :: Propiedad->Accion
hacerBerrinchePor unaPropiedad unParticipante | verificarAptoCompra unParticipante unaPropiedad = comprar unaPropiedad unParticipante
                                              | otherwise = hacerBerrinchePor unaPropiedad (modificarAcciones (modificarDinero (+) unParticipante 10) gritar)

ultimaRonda :: Participante->Accion
ultimaRonda unParticipante = foldl1 (.) (accionesDeParticipante unParticipante) 

juegoFinal :: Participante->Participante->String
juegoFinal participante1 participante2 | (cantidadDeDinero ((ultimaRonda participante1) participante1) > cantidadDeDinero ((ultimaRonda participante2) participante2)) = "El ganador es el participante 1"
                                       | (cantidadDeDinero ((ultimaRonda participante1) participante1) < cantidadDeDinero ((ultimaRonda participante2) participante2)) = "El ganador es el participante 2"
                                       | otherwise = "Hay un empate"