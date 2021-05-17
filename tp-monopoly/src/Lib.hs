module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


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
cambiarDinero :: (Int -> Int) -> Accion
cambiarDinero funcionDinero unJugador = unJugador { dinero = funcionDinero.dinero $ unJugador }

cambiarAccion :: ([Accion] -> [Accion]) -> Accion
cambiarAccion funcionAccion unJugador = unJugador { acciones = funcionAccion.acciones $ unJugador }

cambiarPropiedad :: ([Propiedad] -> [Propiedad]) -> Accion
cambiarPropiedad funcionPropiedad unJugador = unJugador { propiedades = funcionPropiedad.propiedades $ unJugador }

asignarTactica :: Tacticas -> Accion
asignarTactica tactica unJugador = unJugador { tacticaDeJuego = tactica }


perderDinero :: Int -> Accion
perderDinero dinero = cambiarDinero (subtract dinero)

ganarDinero :: Int -> Accion
ganarDinero dinero = cambiarDinero (+ dinero)

sumarAccion :: Accion -> Accion
sumarAccion unaAccion = cambiarAccion (++[unaAccion])

comprarPropiedad :: Propiedad -> Accion
comprarPropiedad unaPropiedad = cambiarPropiedad (++[unaPropiedad])


esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = precio unaPropiedad < 15

esAccionista :: Jugador -> Bool
esAccionista unJugador = tacticaDeJuego unJugador == Accionista

esOferente :: Jugador -> Bool
esOferente unJugador = tacticaDeJuego unJugador == OferenteSingular



--funciones
pasarPorElBanco :: Accion
pasarPorElBanco = (ganarDinero 40) . (asignarTactica CompradorCompulsivo)

enojarse :: Accion
enojarse = (ganarDinero 50) . (sumarAccion gritar)

gritar :: Accion
gritar unJugador = unJugador { nombre = (++ nombre unJugador) "AHHHH" }

pagarAAccionistas :: Accion
pagarAAccionistas unJugador 
    | esAccionista unJugador = ganarDinero 200 unJugador
    | otherwise = perderDinero 100 unJugador

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unJugador
    | puedePagarPropiedad unaPropiedad unJugador = unJugador
    | otherwise = hacerBerrinchePor unaPropiedad (gritar . (ganarDinero 10) $ unJugador)


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
pagarPropiedad unaPropiedad = (perderDinero (precio unaPropiedad)) . (comprarPropiedad unaPropiedad)



--cobrarAlquileres
cobrarAlquileres :: Accion
cobrarAlquileres unJugador = ganarDinero (cuantoCobrar.propiedades $ unJugador) unJugador

cuantoCobrar :: [Propiedad] -> Int
cuantoCobrar = sum.map alquilerSegunPropiedad

alquilerSegunPropiedad :: Propiedad -> Int
alquilerSegunPropiedad unaPropiedad
    | esPropiedadBarata unaPropiedad = 10
    | otherwise = 200


--juegoFinal
ultimaRonda :: Jugador -> Accion
ultimaRonda unJugador = foldr1 (.) (acciones unJugador)

pasarPorUltimaRonda :: Accion
pasarPorUltimaRonda unJugador = (ultimaRonda unJugador) unJugador

dineroUltimaRonda :: Jugador -> Int
dineroUltimaRonda = dinero . pasarPorUltimaRonda

juegoFinal :: Jugador -> Jugador -> Jugador
juegoFinal unJugador otroJugador 
    | dineroUltimaRonda unJugador >= dineroUltimaRonda otroJugador = unJugador
    | otherwise = otroJugador
