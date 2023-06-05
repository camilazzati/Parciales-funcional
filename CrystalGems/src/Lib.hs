
data Aspecto = UnAspecto {
  tipoDeAspecto :: String,
  grado :: Float
} deriving (Show, Eq)

{-  tensión, incertidumbre y peligro -}

tension :: Aspecto
tension = UnAspecto "tension" 0.0

incertidumbre :: Aspecto
incertidumbre = UnAspecto "incertidumbre" 0.0

peligro :: Aspecto
peligro = UnAspecto "peligro" 0.0


type Situacion = [Aspecto]

mejorAspecto :: Aspecto -> Aspecto -> Bool
mejorAspecto mejor peor = grado mejor < grado peor

mismoAspecto :: Aspecto -> Aspecto -> Bool
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2

buscarAspecto :: Aspecto -> Situacion -> Aspecto
buscarAspecto aspectoBuscado = head . filter (mismoAspecto aspectoBuscado)

{-
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)
reemplazarAspecto aspectoBuscado situacion = aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)
-}

---------------------------------------- Punto 1 ------------------------------------

-- a 
{- Definir modificarAspecto que dada una función de tipo (Float -> Float) y un aspecto, modifique el aspecto alterando 
su grado en base a la función dada. -}

modificarAspecto :: (Float -> Float) -> Aspecto -> Aspecto
modificarAspecto funcion unAspecto = unAspecto {grado = funcion (grado unAspecto)}

-- b
{- Saber si una situación es mejor que otra: esto ocurre cuando, para la primer situación, cada uno de los aspectos, 
es mejor que ese mismo aspecto en la segunda situación.
Nota: recordar que los aspectos no necesariamente se encuentran en el mismo orden para ambas situaciones. 
Sin embargo, las situaciones a comparar siempre tienen los mismos aspectos. -}

-- mejorAspecto :: Aspecto -> Aspecto -> Bool
-- mejorAspecto mejor peor = grado mejor < grado peor

esMejorSituacion :: Situacion -> Situacion -> Bool
esMejorSituacion unaSituacion1 unaSituacion2 
    | mejorAspecto (head unaSituacion1) (buscarAspecto (head unaSituacion1) unaSituacion2) && mejorAspecto (head (drop 1 unaSituacion1)) (buscarAspecto (head (drop 1 unaSituacion1)) unaSituacion2) && mejorAspecto (head (drop 2 unaSituacion1)) (buscarAspecto (head (drop 2 unaSituacion1)) unaSituacion2) = True
    | otherwise = False
-- tengo que buscar el aspecto de la situacion1 en la situacion2 y compararlos para saber si es mejor (recursividad)

-- c
{- Definir una función modificarSituacion que a partir de una situación permita obtener otra de modo que se modifique de cierta forma
 el grado correspondiente a un tipo de aspecto buscado. La alteración a realizar sobre el grado actual de ese aspecto debe poder ser 
 indicada al usar la función. -}


modificarSituacion :: Situacion -> String -> (Float -> Float) -> Situacion
modificarSituacion unaSituacion aspectoAModificar funcion = (modificarAspecto funcion . filter (== aspectoAModificar) . map tipoDeAspecto) unaSituacion


---------------------------------------- Punto 2 ------------------------------------

-- a
{- Modelar a las Gemas de modo que estén compuestas por su nombre, la fuerza que tienen y la personalidad. La personalidad de una 
Gema debe representar cómo reacciona ante una situación, derivando de ese modo a una situación diferente. -}

data Gema = Gema {
    nombre :: String,
    fuerza :: Int,
    personalidad :: Reaccion
}

type Reaccion = Situacion -> Situacion

-- b
{- vidente: ante una situación disminuye a la mitad la incertidumbre y baja en 10 la tensión. -}

vidente :: Reaccion
vidente unaSituacion = (modificarAspecto (- 10.0) (buscarAspecto tension) . modificarAspecto (flip div 2) (buscarAspecto incertidumbre)) unaSituacion

vidente unaSituacion = (modificarSituacion "tension")

{- relajada: disminuye en 30 la tensión de la situación y, dependiendo de qué tan relajada sea la Gema, 
aumenta el peligro en tantas unidades como nivel de relajamiento tenga. -}

relajada :: Float -> Reaccion
relajada nivelDeRelajamiento unaSituacion = (modificarAspecto (+ nivelDeRelajamiento) (buscarAspecto peligro) . modificarAspecto (- 30) (buscarAspecto tension)) unaSituacion







