------------------------------------------- Punto 1 --------------------------------------------

{- Un guantelete está hecho de un material (“hierro”, “uru”, etc.) y sabemos las gemas que posee. -}

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
}

{-  personajes que tienen una edad, una energía, una serie de habilidades (como por ejemplo “usar espada”, “controlar la mente”, etc), 
su nombre y en qué planeta viven. -}

data Personaje = Personaje {
    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
} deriving (Show)

ironMan :: Personaje
ironMan = Personaje { edad = 45, energia = 100, habilidades = [ "volar" , "fuerza" ], nombre = "IronMan", planeta = "Tierra" } 

type Universo = [Personaje]

type Gema = Personaje -> Personaje

{- cuando un guantelete está completo -es decir, tiene las 6 gemas posibles- y su material es “uru”, se tiene la posibilidad de chasquear 
un universo que contiene a todos sus habitantes y reducir a la mitad la cantidad de dichos personajes. -}

chasquear :: Guantelete -> Universo -> Universo
chasquear unGuantelete unUniverso 
    | puedeChasquear unGuantelete = mitadDelUniverso unUniverso
    | otherwise = id unUniverso

puedeChasquear :: Guantelete -> Bool
puedeChasquear unGuantelete = ((== 6) . length . gemas) unGuantelete && ((== "uru") . material) unGuantelete

mitadDelUniverso :: Universo -> Universo
mitadDelUniverso unUniverso = take (div (length unUniverso) 2) unUniverso

------------------------------------------- Punto 2 --------------------------------------------

{- Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años -}

aptoParaPendex :: Universo -> Bool
aptoParaPendex unUniverso = (any (< 45) . map edad) unUniverso

{- Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad. -}

energiaTotal :: Universo -> Int
energiaTotal = sumarEnergias . (filter ((>= 2) . length . habilidades))

sumarEnergias :: Universo -> Int
sumarEnergias unUniverso = (sum . map energia) unUniverso

------------------------------------------- Punto 3 --------------------------------------------

{- La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado. -}

mente :: Int -> Gema
mente n unPersonaje = quitarEnergia n unPersonaje

quitarEnergia :: Int -> Personaje -> Personaje
quitarEnergia n unPersonaje = unPersonaje { energia = energia unPersonaje - n}

{- El alma puede controlar el alma de nuestro oponente permitiéndole eliminar una habilidad en particular si es que la posee.
 Además le quita 10 puntos de energía. -}

alma :: String -> Gema
alma habilidad unPersonaje = quitarEnergia 10 unPersonaje { habilidades = filter (/= habilidad) (habilidades unPersonaje) }

{- El espacio que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía. -}

espacio :: String -> Gema
espacio nuevoPlaneta unPersonaje = quitarEnergia 20 unPersonaje { planeta = nuevoPlaneta }

{- El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad). -}

poder :: Gema
poder unPersonaje = (quitarHabilidades . quitarEnergia (energia unPersonaje)) unPersonaje

quitarHabilidades :: Personaje -> Personaje
quitarHabilidades unPersonaje 
    | puedeQuitar unPersonaje = unPersonaje {habilidades = []}
    | otherwise = id unPersonaje

puedeQuitar :: Personaje -> Bool
puedeQuitar unPersonaje = ((<= 2) . length . habilidades) unPersonaje

{- El tiempo que reduce a la mitad la edad de su oponente pero como no está permitido pelear con menores, no puede dejar la edad del oponente 
con menos de 18 años. Considerar la mitad entera, por ej: si el oponente tiene 50 años, le quedarán 25. Si tiene 45, le quedarán 22 
(por división entera). Si tiene 30 años, le deben quedar 18 en lugar de 15. También resta 50 puntos de energía. -}

tiempo :: Gema
tiempo unPersonaje = (quitarEnergia 50 . reducirEdad) unPersonaje

reducirEdad :: Personaje -> Personaje
reducirEdad unPersonaje = unPersonaje { edad = ((max 18) . (flip div 2) . edad) unPersonaje}

{- La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival. -}

gemaLoca :: Gema -> Gema
gemaLoca unaGema unPersonaje = (unaGema . unaGema) unPersonaje

------------------------------------------- Punto 4 --------------------------------------------

{- Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” y la gema loca que manipula 
el poder del alma tratando de eliminar la “programación en Haskell”. -}

guanteleteGoma :: Guantelete
guanteleteGoma = Guantelete "goma" [alma "usar Mjolnir", tiempo, gemaLoca (alma "programacion Haskell")]

------------------------------------------- Punto 5 --------------------------------------------

{- Generar la función utilizar  que dado una lista de gemas y un enemigo ejecuta el poder de cada una de las gemas que lo componen contra el 
personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima. -}

usarGema :: [Gema] -> Personaje -> Personaje
usarGema gemas unPersonaje = (foldl (flip ($)) unPersonaje) gemas

------------------------------------------- Punto 6 --------------------------------------------

{- Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del infinito que produce la 
pérdida más grande de energía sobre la víctima. -}

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa unGuantelete unPersonaje = gemaMasPoderosaDe unPersonaje (gemas unGuantelete)

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe unPersonaje (gema1 : gema2 : gemas) 
    | (energia . gema1) unPersonaje < (energia . gema2) unPersonaje = gemaMasPoderosaDe unPersonaje (gema1 : gemas)
    | otherwise = gemaMasPoderosaDe unPersonaje (gema2 : gemas)









