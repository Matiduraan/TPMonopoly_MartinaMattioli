import Text.Show.Functions()

main :: IO ()
main = return ()

type Propiedad = (String, Int)
type Accion = Participante -> Participante
type Tactica = String

data Participante = UnParticipante { nombreDelParticipante :: String
                                    , dineroEnCuenta :: Int
                                    , tacticaDelJuego :: String
                                    , propiedadesDeParticipante :: [Propiedad]
                                    , unaAccion :: [Accion]
                                   } deriving (Show)  

participanteCarolina :: Participante
participanteCarolina = UnParticipante  "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]

participanteManuel :: Participante
participanteManuel = UnParticipante "Manuel" 500 "Oferente Singular" [] [pasarPorElBanco, enojarse]

pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = unParticipante {dineroEnCuenta = dineroEnCuenta unParticipante + 40, tacticaDelJuego = "Comprador Compulsivo"}

enojarse:: Accion
enojarse unParticipante = unParticipante {dineroEnCuenta = dineroEnCuenta unParticipante + 50, unaAccion = unaAccion unParticipante ++ [gritar]}

gritar:: Accion
gritar unParticipante = unParticipante {nombreDelParticipante = "AHHH" ++ nombreDelParticipante unParticipante}

subastar :: Propiedad -> Accion
subastar propiedadASubastar unParticipante 
    | cumpleCondicionesDeSubastar unParticipante == True = ganarLaPropiedad propiedadASubastar unParticipante
    | otherwise = unParticipante

ganarLaPropiedad :: Propiedad -> Accion
ganarLaPropiedad  propiedadAGanar unParticipante  = unParticipante {dineroEnCuenta = dineroEnCuenta unParticipante - obtenerPrecioDePropiedad propiedadAGanar, propiedadesDeParticipante = propiedadesDeParticipante unParticipante ++ [propiedadAGanar]}

cumpleCondicionesDeSubastar :: Participante -> Bool
cumpleCondicionesDeSubastar unParticipante = tieneTacticaDe "Oferente Singular" unParticipante || tieneTacticaDe "Accionista" unParticipante 

tieneTacticaDe :: Tactica -> Participante -> Bool
tieneTacticaDe unaTactica unParticipante = ((== unaTactica).tacticaDelJuego) unParticipante 

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = unParticipante {dineroEnCuenta = dineroEnCuenta unParticipante + (sum (map esPropiedadBarataOCara propiedadesDeParticipante)) unParticipante}

esPropiedadBarataOCara :: Propiedad -> Int
esPropiedadBarataOCara propiedadDeParticipante 
    | obtenerPrecioDePropiedad propiedadDeParticipante < 150 = 10
    | otherwise = 20

obtenerPrecioDePropiedad :: Propiedad -> Int
obtenerPrecioDePropiedad (_,precioDePropiedad) = precioDePropiedad

pagarAAccionistas :: Accion
pagarAAccionistas unParticipante 
    | tieneTacticaDe "Accionista" unParticipante = unParticipante {dineroEnCuenta = dineroEnCuenta unParticipante + 200}
    | otherwise = unParticipante {dineroEnCuenta = dineroEnCuenta unParticipante - 100}
                                
