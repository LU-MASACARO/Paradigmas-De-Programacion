import Text.Show.Functions

type Accion = Participante -> Participante

type Propiedad = (String,Int)

data Participante = UnParticipante{
    nombre :: String,
    cantidadDeDinero :: Int,
    tacticaDeJuego :: String,
    propiedadesCompradas :: [Propiedad],
    accionesDeParticipante :: [Accion]
} deriving (Show)

edificio :: Propiedad
edificio = ("Edificio Blanco",100)
casa :: Propiedad
casa = ("Casa Blanca",200)

carolina :: Participante
carolina = UnParticipante "Carolina" 500 "Accionista" [edificio] [pasarPorElBanco, pagarAAccionistas]
manuel :: Participante
manuel = UnParticipante "Manuel" 800 "Oferente Singular" [] [pasarPorElBanco, enojarse]
pedro :: Participante
pedro = UnParticipante "Pedro" 80 "Apostador" [casa,edificio] [pasarPorElBanco]
juan :: Participante
juan = UnParticipante "juan" 80 "Apostador" [] [pasarPorElBanco]

-----FUNCIONES AUXILIARES-----
modificarDinero :: (Int->Int)->Participante->Participante
modificarDinero unaFuncion unParticipante = unParticipante {cantidadDeDinero = unaFuncion (cantidadDeDinero unParticipante)}

modificarPropiedades :: Propiedad->Participante->Participante
modificarPropiedades unaPropiedad unParticipante = unParticipante {propiedadesCompradas = unaPropiedad : propiedadesCompradas unParticipante}

modificarTactica :: String->Participante->Participante
modificarTactica tactica unParticipante = unParticipante {tacticaDeJuego = tactica}

agregarAccionAJugador :: Accion->Participante->Participante
agregarAccionAJugador unaAccion unParticipante= unParticipante {accionesDeParticipante = unaAccion : accionesDeParticipante unParticipante}

verificarTacticaDeJuego :: Participante->String->Bool
verificarTacticaDeJuego unParticipante tactica = (==tactica) . tacticaDeJuego $ unParticipante

esAccionista :: Participante->Bool
esAccionista unParticipante = verificarTacticaDeJuego unParticipante "Accionista"

verificarAptoSubasta :: Participante->Bool
verificarAptoSubasta unParticipante = esAccionista unParticipante || verificarTacticaDeJuego unParticipante "Oferente Singular"

verificarAptoCompra :: Participante->Propiedad->Bool
verificarAptoCompra unParticipante unaPropiedad = cantidadDeDinero unParticipante >= snd unaPropiedad    

preciosDePropiedadesCompradas :: Participante->[Int]
preciosDePropiedadesCompradas unParticipante = map snd (propiedadesCompradas unParticipante)

esPropiedadBarata :: Propiedad->Int
esPropiedadBarata unaPropiedad | snd unaPropiedad < 150 = 10
                               | otherwise = 20

comprar :: Propiedad->Accion
comprar unaPropiedad unParticipante = modificarPropiedades unaPropiedad . modificarDinero (subtract (snd unaPropiedad)) $ unParticipante

valorAlquiler :: Participante->Int
valorAlquiler unParticipante = sum (map esPropiedadBarata (propiedadesCompradas unParticipante))

cantidadDeDineroEnUltimaRonda :: Participante->Int
cantidadDeDineroEnUltimaRonda unParticipante = cantidadDeDinero . ultimaRonda unParticipante $ unParticipante

tieneMayorPlataEnUltimaRondaQue :: Participante->Participante->Bool
tieneMayorPlataEnUltimaRondaQue participante1 participante2 = cantidadDeDineroEnUltimaRonda participante1 >= cantidadDeDineroEnUltimaRonda participante2
-----FIN FUNCIONES AUXILIARES-----

pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = modificarTactica "Comprador Compulsivo" . modificarDinero (+40) $ unParticipante

enojarse :: Accion
enojarse unParticipante = agregarAccionAJugador gritar . modificarDinero (+50) $ unParticipante

gritar :: Accion
gritar unParticipante = unParticipante {nombre = "AHHHH " ++ nombre unParticipante}

subastar :: Propiedad->Accion
subastar unaPropiedad unParticipante | verificarAptoSubasta unParticipante && verificarAptoCompra unParticipante unaPropiedad = comprar unaPropiedad unParticipante
                                     | otherwise = unParticipante 

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = modificarDinero (+ valorAlquiler unParticipante) unParticipante  

pagarAAccionistas :: Accion
pagarAAccionistas unParticipante | esAccionista unParticipante = modificarDinero (+200) unParticipante 
                                 | otherwise = modificarDinero (subtract 100) unParticipante                       

hacerBerrinchePor :: Propiedad->Accion
hacerBerrinchePor unaPropiedad unParticipante | verificarAptoCompra unParticipante unaPropiedad = comprar unaPropiedad unParticipante
                                              | otherwise = gritar . hacerBerrinchePor unaPropiedad . agregarAccionAJugador gritar . modificarDinero (+10) $ unParticipante

ultimaRonda :: Participante->Accion
ultimaRonda unParticipante = foldl1 (.) (accionesDeParticipante unParticipante) 

juegoFinal :: Participante->Participante->Participante
juegoFinal participante1 participante2 | tieneMayorPlataEnUltimaRondaQue participante1 participante2 = participante1
                                       | otherwise = participante2
