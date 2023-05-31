-- token: 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

{- De cada investigador conocemos su nombre, su experiencia en investigaciones, su Pokemon compañero, su mochila y 
una serie de Pokemons capturados -}


data Investigador = Investigador {
    nombre :: String,
    experienciaEnInvestigaciones :: Int,
    pokemonCompaniero :: Pokemon,
    mochila :: [Item],
    pokemonsCapturados :: [Pokemon],
    rango :: Rango
} deriving (Show)

{- los Pokemons. De ellos conocemos su mote (apodo), una breve descripción, su nivel y una cantidad de puntos base de investigación 
que otorgan. -}

data Pokemon = Pokemon {
    mote :: String,
    descripcion :: String, 
    nivel :: Int,
    puntosBase :: Int
} deriving (Eq, Show)

data Rango = Cielo | Estrella | Constelacion | Galaxia deriving (Show,Eq)

----------------------------------------- Punto 1 -------------------------------------------------

{- Modelar a Akari, una investigadora de rango Constelación a quien le faltan 501 puntos para pasar a ser de rango Galaxia; 
tiene un Oshawott nivel 5 de compañero y ningún otro Pokemon. Oshawott es un Pokemon que otorga 3 puntos de experiencia en investigación y 
su descripción es “una nutria que pelea con el caparazón de su pecho”. -}


akari :: Investigador
akari = Investigador "Akari" 1499 oshawott [] [] Constelacion 

oshawott :: Pokemon
oshawott = Pokemon "Oshawott" "una nutria que pelea con el caparazón de su pecho" 5 3 

----------------------------------------- Punto 2 -------------------------------------------------

{- Existen 4 rangos que se determinan según su experiencia: Cielo, Estrella, Constelación y Galaxia:
Para pasar de rango Cielo a Estrella se requieren 100 puntos de experiencia
Para pasar de rango Estrella a Constelación se requieren 500 puntos de experiencia
Para pasar de rango Constelación a Galaxia se requieren 2000 puntos de experiencia
-}

queRangoEs :: Investigador -> Rango 
queRangoEs unInvestigador 
    | experienciaEnInvestigaciones unInvestigador < 100 = Cielo
    | experienciaEnInvestigaciones unInvestigador < 500 = Estrella
    | experienciaEnInvestigaciones unInvestigador < 2000 = Constelacion
    | otherwise = Galaxia


----------------------------------------- Punto 3 -------------------------------------------------

type Actividad = Investigador -> Investigador

{- Obtener un ítem: Agrega el ítem a su mochila y afecta a la experiencia según el modificador del ítem.
Bayas: incrementa la experiencia 1 punto y luego la eleva al cuadrado
Apricorns: multiplica la experiencia por 1.5
Guijarros: aumenta la experiencia en 2 
Fragmentos de hierro: divide la experiencia en la cantidad de fragmentos de hierro que tenga el ítem. -}

data Item = Bayas | Apricorns | Guijarros | Fragmentos deriving (Show,Eq)

obtenerUnItem :: Item -> Actividad
obtenerUnItem item unInvestigador = (modificarExperienciaSegunItem item . agregarItemAMochila item) unInvestigador

modificarExperienciaSegunItem :: Item -> Investigador -> Investigador
modificarExperienciaSegunItem Bayas unInvestigador = modificarExperiencia ((^2) . (+1)) unInvestigador
modificarExperienciaSegunItem Apricorns unInvestigador = modificarExperiencia (*1.5) unInvestigador
modificarExperienciaSegunItem Guijarros unInvestigador = modificarExperiencia (+2)unInvestigador
modificarExperienciaSegunItem Fragmentos unInvestigador = modificarExperiencia (flip div 2) unInvestigador

agregarItemAMochila :: Item -> Investigador -> Investigador
agregarItemAMochila item unInvestigador = unInvestigador { mochila = item:mochila unInvestigador}

{- Admirar el paisaje: Es una actividad muy disfrutable, ya que la región de Hisui es bastante pintoresca. 
Debido a la distracción que el bello paisaje provoca, pierde cinco por ciento de experiencia y los primeros 3 ítems de su mochila. -}

admirarPaisaje :: Actividad
admirarPaisaje unInvestigador = (perderItems 3 . modificarExperienciaPorcentual (- 5)) unInvestigador

modificarExperienciaPorcentual :: Int -> Investigador -> Investigador
modificarExperienciaPorcentual porcentual unInvestigador = modificarExperiencia (div (porcentual * experienciaDeInvestigador unInvestigador) 100) unInvestigador

perderItems :: Int -> Investigador -> Investigador
perderItems n unInvestigador = unInvestigador {mochila = drop n (mochila unInvestigador)}

{- Capturar un Pokemon: Lo agrega a su lista de Pokemon y obtiene tanta experiencia como puntos otorga el Pokemon. 
Si el Pokemon otorga más de 20 puntos de experiencia en investigación pasa a ser su nuevo compañero -}

capturarPokemon :: Pokemon -> Actividad
capturarPokemon pokemon unInvestigador = (cambiarCompaniero pokemon . modificarExperiencia (+ puntosBase pokemon) . agregarPokemon pokemon) unInvestigador

agregarPokemon :: Pokemon -> Investigador -> Investigador
agregarPokemon pokemon unInvestigador = unInvestigador {pokemonsCapturados = pokemon:pokemonsCapturados unInvestigador}

cambiarCompaniero :: Pokemon -> Investigador -> Investigador
cambiarCompaniero pokemon unInvestigador
    | puntosBase pokemon > 20 = unInvestigador {pokemonCompaniero = pokemon}
    | otherwise = id unInvestigador

{- Combatir un Pokemon: Si le gana, obtiene tanta experiencia como el 50% de la cantidad de puntos que el Pokemon da. 
Le gana cuando el Pokemon compañero del investigador tiene más nivel. -}

combatirUnPokemon :: Pokemon -> Actividad
combatirUnPokemon pokemon unInvestigador
    | puntosBase pokemon < puntosBase pokemonCompaniero unInvestigador = modificarExperiencia (+50% puntosBase pokemon)
    | otherwise = id unInvestigador

modificarExperiencia :: (a -> b) -> Investigador -> Investigador
modificarExperiencia funcion unInvestigador = unInvestigador {experienciaEnInvestigaciones = funcion (flip ($)) experienciaEnInvestigaciones unInvestigador } 

----------------------------------------- Punto 4 -------------------------------------------------

type Expedicion = [Actividad]

realizarExpedicion :: [Investigador] -> Expedicion -> [Investigador]
realizarExpedicion investigadores expedicion = (map . foldl (flip ($)) expedicion) investigadores 

-- realizarActividad :: Investigador -> Actividad -> Investigador
-- realizarActividad unInvestigador unaActividad = unaActividad unInvestigador

{- El nombre de los investigadores que tras realizar la expedición tengan más de 3 Pokemons alfa. -}
-- primero tengo que hacer que los investigadores hagan la expedicion?

investigadoresConMasDeTresAlfa :: Expedicion -> [Investigador] -> [String]
investigadoresConMasDeTresAlfa expedicion investigadores = ((map nombre . (filter esAlfa)) . (flip realizarExpedicion expedicion)) investigadores

{- Existen también ciertas variantes de los Pokemon, conocidos como Alfa, que duplican la cantidad de puntos que otorgan. 
Un pokemon es alfa si su mote empieza con Alfa o tiene todas las vocales. -}

esAlfa :: Pokemon -> Bool
esAlfa pokemon = empiezaConAlfa mote pokemon || (elem a && elem e && elem i && elem o && elem u) mote pokemon

empiezaConAlfa :: String -> Bool
empiezaConAlfa mote = ( (== "Alfa") . take 4) mote

{- La experiencia de los investigadores que luego de realizar la expedición tengan el rango Galaxia. -}

experienciaDeInvestigadoresConRangoGalaxia :: Expedicion -> [Investigador] -> [Int]
experienciaDeInvestigadoresConRangoGalaxia expedicion investigadores = ((map experienciaDeInvestigador (filter esGalaxia)) . (flip realizarExpedicion expedicion)) investigadores

esGalaxia :: Investigador -> Bool
esGalaxia unInvestigador = experienciaDeInvestigador unInvestigador >= 2000

{- Los Pokemons compañeros de los investigadores que, después de la expedición, tengan al menos un nivel de 10. -}

pokemonsConNivelDiez :: Expedicion -> [Investigador] -> [Pokemon]
pokemonsConNivelDiez expedicion investigadores = ((map pokemonCompaniero (filter tieneNivel10 pokemonCompaniero investigadores)) . (flip realizarExpedicion expedicion)) investigadores

tieneNivel10 :: Pokemon -> Bool
tieneNivel10 pokemon = nivel pokemon >= 10

{- Los últimos 3 Pokemons capturados de cada investigador que, tras realizar la expedición, tengan todos sus Pokemons un nivel par. -}

pokemonsDeInvestigadoresConPokemonsDeNivelPar :: Expedicion -> [Investigador] -> [[Pokemon]]
pokemonsDeInvestigadoresConPokemonsDeNivelPar expedicion investigadoes = ( map (take 3 . reverse) pokemonsCapturados investigadores (filter todosTienenNivelPar) . (flip realizarExpedicion expedicion)) investigadores

todosTienenNivelPar :: [Pokemon] -> Bool
todosTienenNivelPar pokemons = all even nivel pokemons

----------------------------------------- Punto 5 -------------------------------------------------

-- solo la del rango Galaxia, el resto loopean
