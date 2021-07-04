module Lib where

--data & types
type Accion = Jugador -> Jugador

data Tacticas = Accionista | OferenteSingular | CompradorCompulsivo deriving (Enum, Eq)

instance Show Tacticas where
    show Accionista = "Accionista"
    show OferenteSingular = "Oferente Singular"
    show CompradorCompulsivo = "Comprador Compulsivo"

instance Read Tacticas where
    readsPrec _ "Accionista" = [(Accionista, "")]
    readsPrec _ "Oferente Singular" = [(OferenteSingular, "")]
    readsPrec _ "Comprador Compulsivo" = [(CompradorCompulsivo, "")]


data Propiedad = Propiedad {
    nombrePropiedad :: String,
    precio :: Int --porque en monopoly no hay "centavos"
}

data Jugador = Jugador {
    nombre :: String,
    dinero :: Int, --porque en monopoly no hay "centavos"
    tacticaDeJuego :: Tacticas,
    propiedades :: [Propiedad],
    acciones :: [Accion]
}


--inicializacion
jugadorInicial :: Jugador -> Jugador
jugadorInicial unJugador = (sumarAccion pasarPorElBanco unJugador) {dinero = 500, propiedades = []}

carolinaJugador = jugadorInicial (Jugador { nombre = "Carolina", tacticaDeJuego = Accionista, acciones = [pagarAAccionistas]})

manuelJugador = jugadorInicial (Jugador { nombre = "Manuel", tacticaDeJuego = OferenteSingular, acciones = [enojarse]})


--helpers
mapDinero :: (Int -> Int) -> Accion
mapDinero modifierDinero unJugador = unJugador { dinero = modifierDinero.dinero $ unJugador }

mapAcciones :: ([Accion] -> [Accion]) -> Accion
mapAcciones modifierAccion unJugador = unJugador { acciones = modifierAccion.acciones $ unJugador }

mapPropiedades :: ([Propiedad] -> [Propiedad]) -> Accion
mapPropiedades modifierPropiedad unJugador = unJugador { propiedades = modifierPropiedad.propiedades $ unJugador }

asignarTactica :: Tacticas -> Accion
asignarTactica tactica unJugador = unJugador { tacticaDeJuego = tactica }


perderDinero :: Int -> Accion
perderDinero dinero = mapDinero (subtract dinero)

ganarDinero :: Int -> Accion
ganarDinero dinero = mapDinero (+ dinero)

sumarAccion :: Accion -> Accion
sumarAccion unaAccion = mapAcciones (++[unaAccion])

comprarPropiedad :: Propiedad -> Accion
comprarPropiedad unaPropiedad = mapPropiedades (++[unaPropiedad])


esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = precio unaPropiedad < 15

esAccionista :: Jugador -> Bool
esAccionista Jugador {tacticaDeJuego = Accionista} = True

esOferente :: Jugador -> Bool
esOferente Jugador {tacticaDeJuego = OferenteSingular} = True


--funciones
pasarPorElBanco :: Accion
pasarPorElBanco = ganarDinero 40 . asignarTactica CompradorCompulsivo

enojarse :: Accion
enojarse = ganarDinero 50 . sumarAccion gritar

gritar :: Accion
gritar unJugador = unJugador { nombre = (++ nombre unJugador) "AHHHH" }

pagarAAccionistas :: Accion
pagarAAccionistas unJugador
    | esAccionista unJugador = ganarDinero 200 unJugador
    | otherwise = perderDinero 100 unJugador

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unJugador
    | puedePagarPropiedad unaPropiedad unJugador = unJugador
    | otherwise = hacerBerrinchePor unaPropiedad . gritar . ganarDinero 10 $ unJugador


--subastar
subastar :: Propiedad -> Accion
subastar unaPropiedad unJugador
    | puedeGanarSubasta unaPropiedad unJugador = pagarPropiedad unaPropiedad unJugador
    | otherwise = unJugador

puedeGanarSubasta :: Propiedad -> Jugador -> Bool
puedeGanarSubasta unaPropiedad unJugador = esTacticaDeSubasta unJugador && puedePagarPropiedad unaPropiedad unJugador

esTacticaDeSubasta :: Jugador -> Bool
esTacticaDeSubasta unJugador = esAccionista unJugador || esOferente unJugador

puedePagarPropiedad :: Propiedad -> Jugador -> Bool
puedePagarPropiedad unaPropiedad unJugador  = dinero unJugador >= precio unaPropiedad

pagarPropiedad :: Propiedad -> Accion
pagarPropiedad unaPropiedad = perderDinero (precio unaPropiedad) . comprarPropiedad unaPropiedad


--cobrarAlquileres
cobrarAlquileres :: Accion
cobrarAlquileres unJugador = ganarDinero (cuantoCobrarDeAlquileres unJugador) unJugador

cuantoCobrarDeAlquileres :: Jugador -> Int
cuantoCobrarDeAlquileres unJugador = sum.map alquilerSegunPropiedad $ propiedades unJugador

alquilerSegunPropiedad :: Propiedad -> Int
alquilerSegunPropiedad unaPropiedad
    | esPropiedadBarata unaPropiedad = 10
    | otherwise = 200


--juegoFinal
ultimaRonda :: Jugador -> Accion
ultimaRonda unJugador = foldr1 (.) (acciones unJugador)

pasarPorUltimaRonda :: Accion
pasarPorUltimaRonda unJugador = ultimaRonda unJugador unJugador

dineroUltimaRonda :: Jugador -> Int
dineroUltimaRonda = dinero . pasarPorUltimaRonda

juegoFinal :: Jugador -> Jugador -> Jugador
juegoFinal unJugador otroJugador
    | dineroUltimaRonda unJugador >= dineroUltimaRonda otroJugador = pasarPorUltimaRonda unJugador
    | otherwise = pasarPorUltimaRonda otroJugador
