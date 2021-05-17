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
agregarAccionAJugador unaAccion unParticipante= unParticipante {accionesDeParticipante = unaAccion : (accionesDeParticipante unParticipante)}

verificarTacticaDeJuego :: Participante->String->Bool
verificarTacticaDeJuego unParticipante tactica = (==tactica) (tacticaDeJuego unParticipante)

verificarAptoSubasta :: Participante->Bool
verificarAptoSubasta unParticipante = (verificarTacticaDeJuego unParticipante "Accionista") || (verificarTacticaDeJuego unParticipante "Oferente Singular") 

verificarAptoCompra :: Participante->Propiedad->Bool
verificarAptoCompra unParticipante unaPropiedad = cantidadDeDinero unParticipante >= snd unaPropiedad    

cantidadPropiedadesDePrecio :: (Int->Bool)->Participante->Int
cantidadPropiedadesDePrecio comparacion unParticipante = length (filter comparacion (map snd (propiedadesCompradas unParticipante)))

comprar :: Propiedad->Accion
comprar unaPropiedad unParticipante = modificarPropiedades unaPropiedad . modificarDinero (subtract (snd unaPropiedad)) $ unParticipante

calculoDeAlquileres :: Participante->Int
calculoDeAlquileres unParticipante = 10 * (cantidadPropiedadesDePrecio (<150) unParticipante) + 20 * (cantidadPropiedadesDePrecio (>=150) unParticipante)

calculoFinal :: Participante->Participante->Bool
calculoFinal participante1 participante2 = (cantidadDeDinero ((ultimaRonda participante1) participante1) >= cantidadDeDinero ((ultimaRonda participante2) participante2))
-----FIN FUNCIONES AUXILIARES-----

pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = modificarTactica "Comprador Compulsivo" . modificarDinero (+40) $ unParticipante

enojarse :: Accion
enojarse unParticipante = agregarAccionAJugador gritar . modificarDinero (+50) $ unParticipante

gritar :: Accion
gritar unParticipante = unParticipante {nombre = "AHHHH " ++ nombre unParticipante}

subastar :: Propiedad->Accion
subastar unaPropiedad unParticipante | (verificarAptoSubasta unParticipante && verificarAptoCompra unParticipante unaPropiedad) = comprar unaPropiedad unParticipante
                                     | otherwise = unParticipante 

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = modificarDinero (+calculoDeAlquileres unParticipante) unParticipante  

pagarAAccionistas :: Accion
pagarAAccionistas unParticipante | verificarTacticaDeJuego unParticipante "Accionista" = modificarDinero (+200) unParticipante 
                                 | otherwise = modificarDinero (subtract 100) unParticipante                       

hacerBerrinchePor :: Propiedad->Accion
hacerBerrinchePor unaPropiedad unParticipante | verificarAptoCompra unParticipante unaPropiedad = comprar unaPropiedad unParticipante
                                              | otherwise = hacerBerrinchePor unaPropiedad (agregarAccionAJugador gritar (modificarDinero (+10) unParticipante ))

ultimaRonda :: Participante->Accion
ultimaRonda unParticipante = foldl1 (.) (accionesDeParticipante unParticipante) 

juegoFinal :: Participante->Participante->Participante
juegoFinal participante1 participante2 | calculoFinal participante1 participante2 = participante1
                                       | otherwise = participante2
