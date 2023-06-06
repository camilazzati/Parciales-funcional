-------------------------------------------- Punto 1 ----------------------------------

--a
{- Tenemos personas, de las que se conocen sus habilidades (strings) y si son buenas o
no. Dar un sinónimo de tipo y una constante de ejemplo. -}

data Persona = Persona {
    habilidades :: [String],
    esBuena :: Bool
} deriving (Show)

-- b 
{- Tenemos tambien Power Rangers, de los que conocemos su color, sus habilidades y
su nivel de pelea. Nuevamente, dar un sinónimo de tipo y una constante de ejemplo. -}

data PowerRanger = PowerRanger {
    color :: String, 
    habilidadesPower :: [String],
    nivelDePelea :: Int
} deriving (Show)

ailin :: Persona
ailin = Persona ["Minecraft"] True

vicky :: Persona
vicky = Persona ["Traducir"] True

cristina :: Persona 
cristina = Persona ["Robar"] False

ailinRanger :: PowerRanger
ailinRanger = PowerRanger "rosa" ["a"] 10

vickyRanger :: PowerRanger
vickyRanger = PowerRanger "amarillo" ["b"] 8

-------------------------------------------- Punto 2 ----------------------------------

{- convertirEnPowerRanger: dado un color y una persona, convierte a la persona en un
ranger de ese color. 
Al hacer esto, le traspasa todas sus habilidades originales, potenciadas (por ejemplo: si su
habilidad era “bailar”, ahora es ”superBailar” ), y le da tanto nivel de pelea como la cantidad
de letras de todas sus habilidades originales-}

convertirEnPowerRanger :: String -> Persona -> PowerRanger
convertirEnPowerRanger color unaPersona = PowerRanger {color = color, habilidadesPower = potenciarHabilidades (habilidades unaPersona), nivelDePelea = calcularNivel (habilidades unaPersona)}

potenciarHabilidades :: [String] -> [String]
potenciarHabilidades habilidades = map ("Super" ++) habilidades

calcularNivel :: [String] -> Int
calcularNivel habilidades = (sum . map length ) habilidades

-------------------------------------------- Punto 3 ----------------------------------

{- 3. formarEquipoRanger: dada una lista de colores y una lista de personas, genera un
equipo (una lista de Power Rangers) de los colores dados.
El equipo ranger está conformado por las personas buenas, transformadas en ranger, una
para cada color que haya. Por ejemplo, asumiendo que bulk y skull son personas malas, la
siguiente consulta:
formarEquipoRanger ["rojo", "rosa", "azul"] [jason, skull, kimberly, bulk]
Genera a un equipo con jason convertido en ranger rojo, y kimberlyconvertida en ranger
rosa. Nadie se convierte en ranger azul. -}

type Equipo = [PowerRanger]

{-
formarEquipoRanger :: [String] -> [Persona] -> Equipo
formarEquipoRanger _ [] = []
formarEquipoRanger (color : colores) (persona : personas)
    | esBuena persona = (convertirEnPowerRanger color persona : formarEquipoRanger colores personas)
    | otherwise = formarEquipoRanger (color : colores) personas
-}

formarEquipoRanger :: [String] -> [Persona] -> Equipo
formarEquipoRanger colores personas = ( ((zipWith convertirEnPowerRanger colores)) . filter (esBuena)) personas -- fold?


-------------------------------------------- Punto 4 ----------------------------------

-- a
{- findOrElse: Dada una condición, un valor y una lista, devuelve el primer elemento
que cumpla la condición, o el valor dado si ninguno la cumple. -}

findOrElse :: (a -> Bool) -> a -> [a] -> a
findOrElse _ valor [] = valor
findOrElse condicion valor (x : xs) 
    | condicion x = x 
    | otherwise = findOrElse condicion valor xs


-- cumpleCondicion :: (a -> Bool) -> [a] -> Bool
-- cumpleCondicion condicion = 

-- b
{- rangerLider: Dado un equipo de rangers, devuelve aquel que debe liderar el equipo:
este es siempre el ranger rojo, o en su defecto, la cabeza del equipo. -}

rangerLider :: Equipo -> PowerRanger
rangerLider unEquipo = findOrElse (esRangerRojo) (head unEquipo) unEquipo

esRangerRojo :: PowerRanger -> Bool
esRangerRojo powerRanger = ((== "Rojo") . color) powerRanger

-------------------------------------------- Punto 5 ----------------------------------

-- a
{- maximumBy: dada una lista, y una función que tome un elemento y devuelva un valor
ordenable, encuentra el máximo de la misma. -}

maximumBy :: Ord b => [a] -> (a -> b) -> a
maximumBy (x : []) _ = x
maximumBy (x : xs) funcion 
    | max (funcion x) (funcion (head xs)) == (funcion x) =  maximumBy (x : (tail xs)) funcion
    | otherwise = maximumBy xs funcion

-- b 
{- rangerMásPoderoso: devuelve el ranger de un equipo dado que tiene mayor nivel de pelea. -}

rangerMasPoderoso :: Equipo -> PowerRanger
rangerMasPoderoso unEquipo = maximumBy unEquipo (nivelDePelea)

-------------------------------------------- Punto 6 ----------------------------------

{- rangerHabilidoso: nos dice si un ranger tiene más de 5 habilidades -}

rangerHabilidoso :: PowerRanger -> Bool
rangerHabilidoso unRanger = ((>5) . length . habilidadesPower) unRanger

-------------------------------------------- Punto 7 ----------------------------------

{- Alfa 5 es un robot que si bien no sabe pelear, podemos considerarlo como un ranger honorífico. Su color es metálico, su
habilidad de pelea es cero, y tiene dos habilidades: reparar cosas y decir “ay ay ay ay ay ay... (Sí, infinitos “ay” ) -}

alfa5 :: PowerRanger
alfa5 = PowerRanger "metalico" ["reparar", repeat "ay "] 0

-- rangerHabilidoso no 
-- rangerMasPoderoso si




