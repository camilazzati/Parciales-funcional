
-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int


maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


between n m x = elem x [n .. m]

----------------------------------- Punto 1 ---------------------------------------------------

type Palo = Habilidad -> Tiro 

putter :: Palo
putter habilidad = UnTiro {
    velocidad = 10,
    precision = precisionJugador habilidad * 2,
    altura = 0
}

madera :: Palo
madera habilidad = UnTiro {
    velocidad = 100,
    precision = div (precisionJugador habilidad)  2,
    altura = 5
}

hierros :: Int -> Palo
hierros n habilidad = UnTiro {
    velocidad = fuerzaJugador habilidad * n,
    precision = div (precisionJugador habilidad) n,
    altura = max (n - 3) 0
}

palos :: [Palo]
palos = [putter, madera] ++ map hierros [1..10]

----------------------------------- Punto 2 ---------------------------------------------------

golpe :: Jugador -> Palo -> Tiro
golpe unJugador unPalo = unPalo (habilidad unJugador)

----------------------------------- Punto 3 ---------------------------------------------------

-- type Obstaculo = Tiro -> Tiro

data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,
  efectoLuegoDeSuperar :: Tiro -> Tiro
  }

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiro
  | puedeSuperar obstaculo tiro = efectoLuegoDeSuperar obstaculo tiro
  | otherwise = tiroDetenido


tunel :: Obstaculo
tunel = UnObstaculo superaTunel efectoTunel

superaTunel :: Tiro -> Bool
superaTunel tiro = precision tiro > 90 && altura tiro == 0

efectoTunel :: Tiro -> Tiro
efectoTunel tiro = UnTiro { velocidad = velocidad tiro * 2, precision = 100, altura = 0 }


laguna :: Int -> Obstaculo
laguna largo = UnObstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5. altura) tiro

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiro = tiro {altura = div (altura tiro) largo }


hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20. velocidad) tiro && altura tiro == 0 && precision tiro > 95

efectoHoyo :: Tiro -> Tiro
efectoHoyo tiro = tiro { velocidad = 0, precision = 0, altura = 0}

{-
obstaculoSuperableSi :: (Tiro -> Bool) -> (Tiro -> Tiro) ->  Tiro -> Tiro
obstaculoSuperableSi condicion efecto tiro 
    | condicion tiro = efecto tiro
    | otherwise = tiroDetenido
-}

tiroDetenido :: Tiro
tiroDetenido = UnTiro {velocidad = 0, precision = 0, altura = 0}

----------------------------------- Punto 4 a)---------------------------------------------------

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador obstaculo = filter (sirveParaSuperar unJugador obstaculo) palos

sirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
sirveParaSuperar unJugador obstaculo palo = puedeSuperar obstaculo (golpe unJugador palo)

----------------------------------- b) ---------------------------------------------------

cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Int
cuantosObstaculosConsecutivosSupera tiro [] = 0
cuantosObstaculosConsecutivosSupera tiro (obstaculo:obstaculos)
    | puedeSuperar obstaculo tiro = 1 + cuantosObstaculosConsecutivosSupera (efectoLuegoDeSuperar obstaculo tiro) obstaculos
    | otherwise = 0

----------------------------------- c) ---------------------------------------------------

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil unJugador obstaculos = maximoSegun (flip cuantosObstaculosConsecutivosSupera obstaculos . golpe unJugador) palos

----------------------------------- Punto 5 ---------------------------------------------------

jugadorDeTorneo = fst
puntosGanados = snd

padresPerdedores :: [(Jugador, Puntos)] -> [String]
padresPerdedores puntosDelTorneo = (map (padre . jugadorDeTorneo) . filter (not . gano puntosDelTorneo)) puntosDelTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDelTorneo puntosDeUnJugador = (all ((< puntosGanados puntosDeUnJugador) . puntosGanados) . filter (/= puntosDeUnJugador)) puntosDelTorneo

