{- queremos saber cual es el nombre de la misma, quienes actúan en ella (y el orden de importancia), su presupuesto anual,
cantidad de temporadas estimadas, el rating promedio que tiene y si está cancelada o no -}

data Serie = Serie {
    nombreSerie :: String,
    actoresSerie :: [Actor],
    presupuesto :: Int,
    temporadasEstimadas :: Int,
    ratingPromedio :: Int,
    cancelada :: Bool 
} deriving (Eq, Show)

{- También, de cada actor o actriz conocemos el nombre, cuál es su sueldo pretendido (anual)
y qué restricciones tiene a la hora de trabajar -}

data Actor = Actor {
    nombreActor :: String,
    sueldoPretendido :: Int,
    restricciones :: [String]
} deriving (Eq, Show)

---------------------------------------- Punto 1 ---------------------------------------------

{- Saber si la serie está en rojo, esto es si el presupuesto no alcanza a cubrir lo
que quieren cobrar todos los actores -}

estaEnRojo :: Serie -> Bool
estaEnRojo unaSerie = presupuesto unaSerie < sumaDeSueldos (actoresSerie unaSerie) 

sumaDeSueldos :: [Actor] -> Int
sumaDeSueldos actores = (sum . map sueldoPretendido) actores

{- Saber si una serie es problemática, esto ocurre si tienen más de 3 actores o
actrices con más de 1 restricción -}

-- esProblematica :: Serie -> Bool
-- esProblematica unaSerie = ((>3) . length (actoresConMasDeUnaRestriccion) . actoresSerie) unaSerie

-- actoresConMasDeUnaRestriccion :: [Actor] -> [Actor]
-- actoresConMasDeUnaRestriccion actores = filter ((>1) . length . restricciones) actores

masDeUnaRestriccion :: Int -> Serie -> Int
masDeUnaRestriccion num = length . filter (masDeNRestricciones num) . (actoresSerie unaSerie)

masDeNRestricciones :: Int -> [Actor] -> Bool
masDeNRestricciones n = (>n) . length . map restricciones

---------------------------------------- Punto 2 ---------------------------------------------

type Productor = Serie -> Serie

{- con favoritismos: elimina a los dos primeros actores de la serie y los
reemplaza por sus actores favoritos. -}

favoritismos :: Actor -> Actor -> Productor
favoritismos actor1 actor2 unaSerie = eliminarActores 2 unaSerie { actoresSerie = (actor1 : actor2 : actoresSerie unaSerie) }

eliminarActores :: Int -> Serie -> Serie
eliminarActores n unaSerie = unaSerie {actoresSerie = drop n (actoresSerie unaSerie)}

{- tim burton: es un caso particular de un productor con favoritismos, siempre
reemplaza a los primeros dos actores por johnny depp y helena bonham
carter, cuyos sueldos pretendidos anuales son $20.000.000 y $15.000.000
respectivamente, y no tienen ninguna restricción. -}

johnnyDepp :: Actor 
johnnyDepp = Actor "Johhny Depp" 20000000 []

helenaBonham :: Actor
helenaBonham = Actor "Helena Bonham" 15000000 []

timBurton :: Productor
timBurton unaSerie = favoritismos johnnyDepp helenaBonham unaSerie

{- gatopardeitor: no cambia nada de la serie. -}

gatopardeitor :: Productor
gatopardeitor unaSerie = id unaSerie

{- estireitor: duplica la cantidad de temporadas estimada de la serie. -}

estireitor :: Productor
estireitor unaSerie = unaSerie { temporadasEstimadas = temporadasEstimadas unaSerie *2}

{- desespereitor: hace un combo de alguna de las anteriores ideas, mínimo 2 -}

desespereitor :: [Productor] -> Productor
desespereitor productores unaSerie = foldl (flip ($)) unaSerie productores

{- canceleitor: si la serie está en rojo o el rating baja de una cierta cifra, la serie
se cancela -}

canceleitor :: Int -> Productor
canceleitor num unaSerie 
    | estaEnRojo unaSerie || ((<num) . ratingPromedio) unaSerie = unaSerie {cancelada = True}
    | otherwise = id unaSerie


---------------------------------------- Punto 3 ---------------------------------------------

{- Calcular el bienestar de una serie, en base a la sumatoria de estos conceptos:
- Si la serie tiene estimadas más de 4 temporadas, su bienestar es 5, de lo contrario
es (10 - cantidad de temporadas estimadas) * 2
- Si la serie tiene menos de 10 actores, su bienestar es 3, de lo contrario es (10 -
cantidad de actores que tienen restricciones), con un mínimo de 2
Aparte de lo mencionado arriba, si la serie está cancelada, su bienestar es 0 más
allá de cómo diesen el bienestar por longitud y por reparto. -}

bienestarLongitud :: Serie -> Int
bienestarLongitud unaSerie
    | ((>4) . temporadasEstimadas) unaSerie = 5
    | otherwise = (10 - temporadasEstimadas unaSerie) * 2


bienestarPorReparto :: Serie -> Int
bienestarPorReparto unaSerie
    | ((<10) . actoresSerie) unaSerie = 3
    | otherwise = max 2 (10 - masDeUnaRestriccion 0) unaSerie

bienestar :: Serie -> Int
bienestar unaSerie
    | cancelada unaSerie = 0
    | otherwise = bienestarLongitud unaSerie + bienestarPorReparto unaSerie

---------------------------------------- Punto 4 ---------------------------------------------

{- Dada una lista de series y una lista de productores, aplicar para cada serie el
productor que la haga más efectiva: es decir, el que le deja más bienestar. -}

aplicarProductorMasEfectivo :: [Serie] -> [Productor] -> [Serie]
aplicarProductorMasEfectivo series productores = map (flip productorMasEfectivo productores) series

productorMasEfectivo :: Serie -> [Productor] -> Serie
productorMasEfectivo unaSerie (productor1 : []) = productor1 unaSerie
productorMasEfectivo unaSerie (productor1 : productore2 : productores) 
    | bienestar (productor1 unaSerie) > bienestar (productor2 unaSerie) = productor1 unaSerie
    | otherwise = productorMasEfectivo unaSerie (productor2 : productores)

---------------------------------------- Punto 6 ---------------------------------------------

{- Saber si una serie es controvertida, que es cuando no se cumple que cada actor de
la lista cobra más que el siguiente -}

esControvertida :: Serie -> Bool
esControvertida unaSerie = (not . cobraMasQueElSiguiente actoresSerie) unaSerie

cobraMasQueElSiguiente :: [Actor] -> Bool
cobraMasQueElSiguiente (actor1 : []) = True
cobraMasQueElSiguiente (actor1 : actor2 : actores) 
    | sueldoPretendido actor1 > sueldoPretendido actor2 = cobraMasQueElSiguiente (actor2 : actores)
    | otherwise = False




