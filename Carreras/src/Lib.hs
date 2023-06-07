
-------------------------------------------------- Punto 1 --------------------------------------------------

{- De cada auto conocemos su color (que nos servirá para identificarlo durante el desarrollo de la carrera), la velocidad a la que está yendo 
y la distancia que recorrió, ambos valores de tipo entero -}

data Auto = Auto {
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving (Show,Eq)

type Carrera = [Auto]

-- a
{- Saber si un auto está cerca de otro auto, que se cumple si son autos distintos y la distancia que hay entre ellos (en valor absoluto) es menor a 10. -}

estanCerca :: Auto -> Auto -> Bool
estanCerca auto1 auto2 = (color auto1 /= color auto2) && ((< 10) . abs $ distancia auto1 - distancia auto2) 

-- b 
{- Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca y les va ganando a todos 
(por haber recorrido más distancia que los otros). -}

vaTranquilo :: Carrera -> Auto -> Bool
vaTranquilo unaCarrera unAuto = ((not . any (estanCerca unAuto)) unaCarrera) && (all ((>) (distancia unAuto) . distancia) unaCarrera)

-- c
{- Conocer en qué puesto está un auto en una carrera, que es 1 + la cantidad de autos de la carrera que le van ganando. -}

enQuePuestoEsta :: Auto -> Carrera -> Int
enQuePuestoEsta unAuto unaCarrera = ((+1) . length . filter (( > distancia unAuto) . distancia)) unaCarrera

-------------------------------------------------- Punto 2 --------------------------------------------------

-- a
{- Hacer que un auto corra durante un determinado tiempo. Luego de correr la cantidad de tiempo indicada, la distancia recorrida por 
el auto debería ser equivalente a la distancia que llevaba recorrida + ese tiempo * la velocidad a la que estaba yendo. -}

correr :: Int -> Auto -> Auto
correr tiempo unAuto = unAuto {distancia = distancia unAuto + (tiempo* velocidad unAuto)}

--b
{- A partir de un modificador de tipo Int -> Int, queremos poder alterar la velocidad de un auto de modo que su velocidad final sea 
la resultante de usar dicho modificador con su velocidad actual. -}

alterarVelocidad :: (Int -> Int) -> Auto -> Auto
alterarVelocidad modificador unAuto = unAuto {velocidad = modificador (velocidad unAuto)}

{- Usar la función del punto anterior para bajar la velocidad de un auto en una cantidad indicada de modo que se le reste a la velocidad 
actual la cantidad indicada, y como mínimo quede en 0, ya que no es válido que un auto quede con velocidad negativa. -}

bajarLaVelocidad :: Int -> Auto -> Auto
bajarLaVelocidad cantidad unAuto
    | (velocidad unAuto - cantidad) < 0 = unAuto {velocidad = 0}
    | otherwise = alterarVelocidad ((-) cantidad) unAuto

-- bajarLaVelocidad cantidad unAuto = max 0 (alterarVelocidad ((-) cantidad) unAuto)

-------------------------------------------------- Punto 3 --------------------------------------------------

type PowerUp = Carrera -> Carrera

afectarALosQueCumplen :: (Auto -> Bool) -> (Auto -> Auto) -> PowerUp
afectarALosQueCumplen condicion funcion unaCarrera = (map funcion . filter condicion) unaCarrera ++ filter (not . condicion) unaCarrera 

-- a
{- terremoto: luego de usar este poder, los autos que están cerca del que gatilló el power up bajan su velocidad en 50. -}

terremoto :: Auto -> PowerUp
terremoto unAuto unaCarrera = afectarALosQueCumplen (estanCerca unAuto) (bajarLaVelocidad 50) unaCarrera

-- b
{- miguelitos: este poder debe permitir configurarse con una cantidad que indica en cuánto deberán bajar la velocidad los autos que se vean 
afectados por su uso. Los autos a afectar son aquellos a los cuales el auto que gatilló el power up les vaya ganando. -}

miguelitos :: Int -> Auto -> PowerUp
miguelitos cantidad unAuto unaCarrera = afectarALosQueCumplen ((> distancia unAuto) . distancia) (bajarLaVelocidad cantidad) unaCarrera

-- c
{- jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. El jet pack tiene un impacto que dura 
una cantidad limitada de tiempo, el cual se espera poder configurar.
Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre durante el tiempo indicado y finalmente 
su velocidad vuelve al valor que tenía antes de que se active el poder. -}

jetPack :: Int -> Auto -> PowerUp
jetPack tiempo unAuto unaCarrera = afectarALosQueCumplen ((== color unAuto) . color) (aplicarJet tiempo) unaCarrera

aplicarJet :: Int -> Auto -> Auto
aplicarJet tiempo unAuto = unAuto { distancia = distancia unAuto + (tiempo * velocidad unAuto * 2)}

-------------------------------------------------- Punto 4 --------------------------------------------------

{- A partir de todo lo construido hasta ahora queremos finalmente simular una carrera, para lo cual se provee una lista de eventos, 
que son funciones que permiten ir de un estado de la carrera al siguiente, y el estado inicial de la carrera a partir del cual se producen 
dichos eventos. Con esta información buscamos generar una “tabla de posiciones”, que incluye la información de en qué puesto quedó cada auto
asociado al color del auto en cuestión. -}

type Evento = (Carrera -> Carrera)

type Eventos = [Evento]

type Color = String

-- a
{- simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
que permita obtener la tabla de posiciones a partir del estado final de la carrera, el cual se obtiene produciendo cada 
evento uno detrás del otro, partiendo del estado de la carrera recibido. -}

simularCarrera :: Carrera -> Eventos -> [(Int, Color)]
simularCarrera unaCarrera eventos =  puestos (foldl (flip ($)) unaCarrera eventos)

puestos :: Carrera -> [(Int, Color)]
puestos unaCarrera = [((map enQuePuestoEsta unaCarrera), (map color unaCarrera))]

-- b
{- correnTodos que hace que todos los autos que están participando de la carrera corran durante un tiempo indicado. -}

correnTodos :: Int -> Evento
correnTodos tiempo unaCarrera = map (correr tiempo) unaCarrera

{- usaPowerUp que a partir de un power up y del color del auto que gatilló el poder en cuestión, encuentre el auto correspondiente
 dentro del estado actual de la carrera para usarlo y produzca los efectos esperados para ese power up. -}

usaPowerUp :: PowerUp -> Color -> PowerUp
usaPowerUp unPowerUp unColor unaCarrera = unPowerUp (find (esElColor unColor) unaCarrera)

esElColor :: Color -> Carrera -> Bool
esElColor unColor unaCarrera = ((== unColor) . map color) unaCarrera

-- c
{- Mostrar un ejemplo de uso de la función simularCarrera con autos de colores rojo, blanco, azul y negro que vayan inicialmente a velocidad 120 y 
su distancia recorrida sea 0, de modo que ocurran los siguientes eventos:
- todos los autos corren durante 30 segundos
- el azul usa el power up de jet pack por 3 segundos
- el blanco usa el power up de terremoto
- todos los autos corren durante 40 segundos
- el blanco usa el power up de miguelitos que reducen la velocidad en 20
- el negro usa el power up de jet pack por 6 segundos
- todos los autos corren durante 10 segundos
 -}

ejCarrera :: Carrera
ejCarrera = [{Auto "rojo" 120 0}, {Auto "blanco" 120 0}, {Auto "azul" 120 0}, {Auto "negro" 120 0}]

ejEventos :: Eventos
ejEventos = [correnTodos 30, usaPowerUp (jetPack 3) "azul", usaPowerUp terremoto "blanco", correnTodos 40, usaPowerUp (miguelitos 20) "blanco", usaPowerUp (jetPack 6) "negro", correnTodos 10]




