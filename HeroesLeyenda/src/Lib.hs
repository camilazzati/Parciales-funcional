
---------------------------------------------- Punto 1 ---------------------------------------------

{- Todo héroe tiene un nombre, pero por lo general nos referimos a ellos por su epíteto. todo héroe tiene un reconocimiento. llevan 
consigo diversos artefactos, que tienen una rareza, indicativos de lo valiosos que son -}

data Heroe = Heroe {
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
}

data Artefacto = Artefacto {
    nombre :: String,
    rareza :: Int
}

type Tarea = Heroe -> Heroe

---------------------------------------------- Punto 2 ---------------------------------------------

{- Hacer que un héroe pase a la historia. Esto varía según el índice de reconocimiento que tenga el
héroe a la hora de su muerte:
a) Si su reconocimiento es mayor a 1000, su epíteto pasa a ser "El mítico", y no obtiene ningún
artefacto. ¿Qué artefacto podría desear tal espécimen?
b) Si tiene un reconocimiento de al menos 500, su epíteto pasa a ser "El magnífico" y añade a sus
artefactos la lanza del Olimpo (100 de rareza).
c) Si tiene menos de 500, pero más de 100, su epíteto pasa a ser "Hoplita" y añade a sus
artefactos una Xiphos (50 de rareza).
d) En cualquier otro caso, no pasa a la historia, es decir, no gana ningún epíteto o artefacto. -}

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria unHeroe
    | reconocimiento unHeroe > 1000 = cambiarEpiteto "El Mitico" unHeroe
    | reconocimiento unHeroe >= 500 = (agregarArtefacto lanzaDelOlimpo . cambiarEpiteto "El Magnifico") unHeroe
    | reconocimiento unHeroe > 100 = (agregarArtefacto xiphos . cambiarEpiteto "Hoplita") unHeroe
    | otherwise = id unHeroe

cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto unEpiteto unHeroe = unHeroe {epiteto = unEpiteto}

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto unArtefacto unHeroe = unHeroe {artefactos = unArtefacto : artefactos unHeroe}

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "Lanza del Olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50

---------------------------------------------- Punto 3 ---------------------------------------------

{- Encontrar un artefacto: el héroe gana tanto reconocimiento como rareza del artefacto, además de
guardarlo entre los que lleva -}

encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto unArtefacto unHeroe = (agregarArtefacto unArtefacto . ganarReconocimiento (rareza unArtefacto)) unHeroe

ganarReconocimiento :: Int -> Heroe -> Heroe
ganarReconocimiento unReconocimiento unHeroe = unHeroe {reconocimiento = reconocimiento unHeroe + unReconocimiento}

{- Escalar el Olimpo: esta ardua tarea recompensa a quien la realice otorgándole 500 unidades de
reconocimiento y triplica la rareza de todos sus artefactos, pero desecha todos aquellos que luego de
triplicar su rareza no tengan un mínimo de 1000 unidades. Además, obtiene "El relámpago de Zeus"
(un artefacto de 500 unidades de rareza). -}

escalarElOlimpo :: Tarea 
escalarElOlimpo unHeroe = (agregarArtefacto relampagoDeZeus . desecharArtefactos . triplicarRarezas . ganarReconocimiento 500) unHeroe

triplicarRarezas :: Heroe -> Heroe
triplicarRarezas unHeroe =  unHeroe { artefactos = map triplicarRarezaDeArtefacto (artefactos unHeroe) }

triplicarRarezaDeArtefacto :: Artefacto -> Artefacto
triplicarRarezaDeArtefacto unArtefacto = unArtefacto {rareza = rareza unArtefacto * 3}

desecharArtefactos :: Heroe -> Heroe
desecharArtefactos unHeroe = unHeroe {artefactos = filter ((>= 1000) . rareza) (artefactos unHeroe)}

relampagoDeZeus :: Artefacto
relampagoDeZeus = Artefacto "El Relampago de Zeus" 500

{- Ayudar a cruzar la calle: incluso en la antigua Grecia los adultos mayores necesitan ayuda para ello. Los héroes que realicen esta tarea 
obtiene el epíteto "Groso", donde la última 'o' se repite tantas veces como cuadras haya ayudado a cruzar. Por ejemplo, ayudar a 
cruzar una cuadra es simplemente "Groso", pero ayudar a cruzar 5 cuadras es "Grosooooo".-}

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cantidadCuadras unHeroe = unHeroe {epiteto = "Gros" ++ replicate cantidadCuadras 'o'}

{- Matar una bestia: Cada bestia tiene una debilidad (por ejemplo: que el héroe tenga cierto artefacto, o que su reconocimiento 
sea al menos de tanto). Si el héroe puede aprovechar esta debilidad, entonces obtiene el epíteto de "El asesino de <la bestia>". 
Caso contrario, huye despavorido, perdiendo su primer artefacto. Además, tal cobardía es recompensada con el epíteto "El cobarde". -}

matarUnaBestia :: Bestia -> Tarea
matarUnaBestia unaBestia unHeroe 
    | debilidad unaBestia unHeroe = cambiarEpiteto ("El asesino de " ++ nombreBestia unaBestia) unHeroe
    | otherwise = (cambiarEpiteto "El cobarde" . perderArtefactos 1) unHeroe 

data Bestia = Bestia {
    nombreBestia :: String,
    debilidad :: Debilidad
}

type Debilidad = Heroe -> Bool

perderArtefactos :: Int -> Heroe -> Heroe
perderArtefactos cantidad unHeroe = unHeroe {artefactos = drop cantidad (artefactos unHeroe)}

---------------------------------------------- Punto 4 ---------------------------------------------

{- Modelar a Heracles , cuyo epíteto es "Guardián del Olimpo" y tiene un reconocimiento de 700. Lleva una pistola de 1000 unidades de rareza 
(es un fierro en la antigua Grecia, obviamente que es raro) y el relámpago de Zeus. Este Heracles es el Heracles antes de realizar sus doce tareas, 
hasta ahora sabemos que solo hizo una tarea. -}

heracles :: Heroe
heracles= Heroe "El Guardian del Olimpo" 700 [pistola, relampagoDeZeus] [matarUnaBestia leonDeNemea]

pistola :: Artefacto
pistola = Artefacto "Pistola" 1000

---------------------------------------------- Punto 5 ---------------------------------------------

{- Modelar la tarea "matar al león de Nemea", que es una bestia cuya debilidad es que el epíteto del
héroe sea de 20 caracteres o más. Esta es la tarea que realizó Heracles. -}

leonDeNemea :: Bestia
leonDeNemea = Bestia "Leon de Nemea" ((>= 20) . length . epiteto) 

---------------------------------------------- Punto 6 ---------------------------------------------

{- Hacer que un héroe haga una tarea. Esto nos devuelve un nuevo héroe con todos los cambios que
conlleva realizar una tarea. -}

hacerTarea :: Tarea -> Heroe -> Heroe
hacerTarea unaTarea unHeroe = unaTarea unHeroe {tareas = unaTarea : tareas unHeroe}

---------------------------------------------- Punto 7 ---------------------------------------------

{- Hacer que dos héroes presuman sus logros ante el otro. Como resultado, queremos conocer la tupla
que tenga en primer lugar al ganador de la contienda, y en segundo al perdedor. Cuando dos héroes
presumen, comparan de la siguiente manera:
- Si un héroe tiene más reconocimiento que el otro, entonces es el ganador.
- Si tienen el mismo reconocimiento, pero la sumatoria de las rarezas de los artefactos de un
héroe es mayor al otro, entonces es el ganador.
- Caso contrario, ambos realizan todas las tareas del otro, y vuelven a hacer la comparación
desde el principio. Llegado a este punto, el intercambio se hace tantas veces sea necesario
hasta que haya un ganador -}

presumir :: Heroe -> Heroe -> (Heroe, Heroe)
presumir heroe1 heroe2 
    | leGana heroe1 heroe2 = (heroe1, heroe2)
    | leGana heroe2 heroe1 = (heroe2, heroe1)
    | otherwise = presumir (realizarTareas (tareas heroe1) heroe2) (realizarTareas (tareas heroe2) heroe1)

leGana :: Heroe -> Heroe -> Bool
leGana heroe1 heroe2 = (reconocimiento heroe1 > reconocimiento heroe2) || (reconocimiento heroe1 == reconocimiento heroe2 && sumatoriaRarezas heroe1 > sumatoriaRarezas heroe2)

sumatoriaRarezas :: Heroe -> Int
sumatoriaRarezas unHeroe = (sum . map rareza . artefactos) unHeroe

realizarTareas :: [Tarea] -> Heroe -> Heroe
realizarTareas tareas unHeroe = foldl (flip ($)) unHeroe tareas

---------------------------------------------- Punto 9 ---------------------------------------------



realizarLabor :: [Tarea] -> Heroe -> Heroe
realizarLabor = realizarTareas 











