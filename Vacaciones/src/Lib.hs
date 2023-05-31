
---------------------------- Punto 1 -------------------------------

data Turista = Turista {
    nivelDeCansancio :: Int,
    nivelDeStress :: Int,
    viajaSolo :: Bool,
    idiomas :: [String]
}

ana :: Turista
ana = Turista { nivelDeCansancio = 0, nivelDeStress = 21, viajaSolo = False, idiomas = ["espaniol"]}

beto :: Turista
beto = Turista { nivelDeCansancio = 15, nivelDeStress = 15, viajaSolo = True, idiomas = ["aleman"] }

cathy :: Turista
cathy = Turista { nivelDeCansancio = 15, nivelDeStress = 15, viajaSolo = True, idiomas = ["aleman","catalan"] }

---------------------------- Punto 2 -------------------------------

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya unTurista
    | viajaSolo unTurista = cambiarCansancio (-5) unTurista
    | otherwise = cambiarStress (-1) unTurista

type Elemento = String

apreciarElementoDelPaisaje :: Elemento -> Excursion
apreciarElementoDelPaisaje elemento unTurista = cambiarStress (- length elemento) unTurista

type Idioma = String

salirAHablarUnIdioma :: Idioma -> Excursion
salirAHablarUnIdioma unIdioma unTurista = ( acompaniado . agregarIdioma unIdioma) unTurista

type Minutos = Int

caminar :: Minutos -> Excursion
caminar minutos unTurista = (cambiarStress (- div minutos 4) . cambiarCansancio (- div minutos 4)) unTurista

data Marea = Tranquila | Moderada | Fuerte

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Tranquila unTurista = (salirAHablarUnIdioma "aleman" . apreciarElementoDelPaisaje "mar" . caminar 10) unTurista
paseoEnBarco Moderada unTurista = id unTurista
paseoEnBarco Fuerte unTurista = (cambiarCansancio 10 . cambiarStress 6) unTurista

cambiarCansancio :: Int -> Turista -> Turista
cambiarCansancio cansancio unTurista = unTurista { nivelDeCansancio = nivelDeCansancio unTurista + cansancio}

cambiarStress :: Int -> Turista -> Turista 
cambiarStress stress unTurista = unTurista { nivelDeStress = nivelDeStress unTurista + stress}

agregarIdioma :: Idioma -> Turista -> Turista
agregarIdioma idioma unTurista = unTurista { idiomas = idioma : idiomas unTurista}

acompaniado :: Turista -> Turista
acompaniado unTurista = unTurista { viajaSolo = False }

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion unaExcurion unTurista = (cambiarStressPorcentual (- 10) . unaExcurion) unTurista

cambiarStressPorcentual :: Int -> Turista -> Turista
cambiarStressPorcentual porcentual unTurista = cambiarStress (div (porcentual * nivelDeStress unTurista) 100) unTurista

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun funcion unTurista unaExcurion = deltaSegun funcion (hacerExcursion unaExcurion unTurista) unTurista

esEducativa ::Excursion -> Turista -> Bool
esEducativa unaExcursion unTurista = ((>0) . deltaExcursionSegun (length . idiomas) unTurista) unaExcursion

excursionesDesestresantes :: [Excursion] -> Turista -> [Excursion]
excursionesDesestresantes excursiones unTurista = filter (flip esDesestresante unTurista) excursiones

esDesestresante :: Excursion -> Turista -> Bool
esDesestresante unaExcursion unTurista = ((>= -3) . deltaExcursionSegun nivelDeStress unTurista) unaExcursion

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarElementoDelPaisaje "cascada", caminar 20, irALaPlaya, salirAHablarUnIdioma "malmacquiano"]

ladoB :: Excursion -> Tour
ladoB unaExcursion = [paseoEnBarco Tranquila, unaExcursion, caminar 120]

islaVecina :: Marea -> Tour
islaVecina mareaIslaVecina = [paseoEnBarco mareaIslaVecina, excursionIslaVecina mareaIslaVecina, paseoEnBarco mareaIslaVecina]

excursionIslaVecina :: Marea -> Excursion
excursionIslaVecina Fuerte = apreciarElementoDelPaisaje "lago"
excursionIslaVecina _ = irALaPlaya

hacerTour :: Turista -> Tour -> Turista
hacerTour unTurista tour = foldl (flip hacerExcursion) (cambiarStress (length tour) unTurista) tour

tourConvincente :: Turista -> [Tour] -> Bool
tourConvincente unTurista tours = any (esConvincente unTurista) tours

esConvincente :: Turista -> Tour -> Bool
esConvincente unTurista tour = (any (dejaAcompaniado unTurista) . flip excursionesDesestresantes unTurista) tour

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado unTurista unaExcursion = (not . viajaSolo . flip hacerExcursion unTurista) unaExcursion

efectividadDeTour :: Tour -> [Turista] -> Int
efectividadDeTour tour = sum . map (espiritualidadAportada tour) . filter (flip esConvincente tour)

espiritualidadAportada :: Tour -> Turista -> Int 
espiritualidadAportada tour = (negate . deltaRutina tour)

deltaRutina :: Tour -> Turista -> Int
deltaRutina tour unTurista = deltaSegun nivelDeRutina (hacerTour unTurista tour) unTurista

nivelDeRutina :: Turista -> Int
nivelDeRutina unTurista = nivelDeCansancio unTurista + nivelDeStress unTurista

playasEternas :: Tour
playasEternas = irALaPlaya: repeat irALaPlaya


