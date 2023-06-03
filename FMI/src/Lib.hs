----------------------------------------------- Punto 1 ---------------------------------------------

{- Sabemos de cada país el "ingreso per cápita" que es el promedio de lo que cada habitante necesita para subsistir, también conocemos la población 
activa en el sector público y la activa en el sector privado, la lista de recursos naturales (ej: "Minería", "Petróleo", "Industria pesada") 
y la deuda que mantiene con el FMI. -}

data Pais = Pais {
    ingresoPerCapita :: Int,
    poblacionActivaSectorPublico :: Int,
    poblacionActivaSectorPrivado :: Int,
    recursosNaturales :: [String],
    deudaConFmi :: Int
} deriving (Eq, Show)

{- Dar un ejemplo de cómo generar al país Namibia, cuyo ingreso per cápita es de 4140 u$s, la población activa del sector público es de 400.000, la 
población activa del sector privado es de 650.000, su riqueza es la minería y el ecoturismo y le debe 50 (millones de u$s) al FMI. -}

namibia :: Pais
namibia = Pais 4140 400000 650000 ["mineria", "ecoturismo"] 50000000

----------------------------------------------- Punto 2 ---------------------------------------------

type Estrategia = Pais -> Pais

{- prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses) -}

-- (div (porcentual * nivelDeStress unTurista) 100)

prestarPlata :: Int -> Estrategia
prestarPlata n unPais = modificarDeuda (div (150 * n) 100) unPais

modificarDeuda :: Int -> Pais -> Pais
modificarDeuda n unPais = unPais {deudaConFmi = deudaConFmi unPais + n} 

{- reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público y además 
que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario -}

reducirPuestosDelSectorPublico :: Int -> Estrategia
reducirPuestosDelSectorPublico cantPuestos unPais = (reducirIngresoPerCapita . reducirActivosSectorPublico cantPuestos) unPais

reducirActivosSectorPublico :: Int -> Pais -> Pais
reducirActivosSectorPublico cantPuestos unPais = unPais { poblacionActivaSectorPublico = poblacionActivaSectorPublico unPais - cantPuestos}

reducirIngresoPerCapita :: Pais -> Pais
reducirIngresoPerCapita unPais
    | (poblacionActivaSectorPublico unPais) > 100 = unPais { ingresoPerCapita = ingresoPerCapita unPais - (div (20 * (ingresoPerCapita unPais)) 100)}
    | otherwise = unPais { ingresoPerCapita = ingresoPerCapita unPais - (div (15 * (ingresoPerCapita unPais)) 100)}

{- darle a una empresa afín al FMI la explotación de alguno de los recursos naturales, esto disminuye 2 millones de dólares la deuda que el país 
mantiene con el FMI pero también deja momentáneamente sin recurso natural a dicho país. No considerar qué pasa si el país no tiene dicho recurso. -}

explotarRecursoNatural :: String -> Estrategia
explotarRecursoNatural recurso unPais = (modificarDeuda (-2) . quitarRecursoNatural recurso) unPais 

quitarRecursoNatural :: String -> Pais -> Pais
quitarRecursoNatural recurso unPais = unPais { recursosNaturales = filter (/= recurso) (recursosNaturales unPais)}

{- establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno (que se calcula como el ingreso per cápita 
multiplicado por su población activa, sumando puestos públicos y privados de trabajo) y reducir 500 puestos de trabajo del sector público. -}

-- blindaje :: Estrategia
-- blindaje unPais = (reducirActivosSectorPublico 500 . prestarPlata (pbi unPais * 0.5)) unPais

pbi :: Pais -> Int
pbi unPais = ingresoPerCapita unPais * poblacionActiva unPais

poblacionActiva :: Pais -> Int
poblacionActiva unPais = poblacionActivaSectorPrivado unPais + poblacionActivaSectorPublico unPais

----------------------------------------------- Punto 3 ---------------------------------------------

-- a
{- Modelar una receta que consista en prestar 200 millones, y darle a una empresa X la explotación de la “Minería” de un país. -}

type Receta = [Estrategia]

receta :: Receta
receta = [prestarPlata 200000000, explotarRecursoNatural "mineria"]

-- b
{- Ahora queremos aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). Justificar cómo se logra el efecto colateral. -}

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta unaReceta unPais = foldl (flip ($)) unPais unaReceta 

----------------------------------------------- Punto 4 ---------------------------------------------

-- a
{- Dada una lista de países conocer cuáles son los que pueden zafar, aquellos que tienen "Petróleo" entre sus riquezas naturales. -}

puedeZafar :: [Pais] -> [Pais]
puedeZafar paises = filter (tienePetroleo) paises

tienePetroleo :: Pais -> Bool
tienePetroleo unPais = elem "petroleo" (recursosNaturales unPais)

-- b
{- Dada una lista de países, saber el total de deuda que el FMI tiene a su favor. -}

deudaAFavor :: [Pais] -> Int
deudaAFavor paises = (sum . map deudaConFmi) paises

----------------------------------------------- Punto 5 ---------------------------------------------

{- dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, en base al siguiente criterio: 
si aplicamos una a una cada receta, el PBI del país va de menor a mayor. Recordamos que el Producto Bruto Interno surge de multiplicar el 
ingreso per cápita por la población activa (privada y pública).  -}

saberSiEstaOrdenadaDePeorAMejor :: [Receta] -> Pais -> Bool
saberSiEstaOrdenadaDePeorAMejor [recetaSola] _ = True
saberSiEstaOrdenadaDePeorAMejor (receta1 : receta2 : recetas) unPais  = revisarPbi receta1 unPais <= revisarPbi receta2 unPais && saberSiEstaOrdenadaDePeorAMejor (receta2 : recetas) unPais

revisarPbi :: Receta -> Pais -> Int
revisarPbi recetaAAplicar unPais = (pbi . aplicarReceta recetaAAplicar) unPais

----------------------------------------------- Punto 6 ---------------------------------------------

-- a 
-- no se puede, ya que se queda buscando el recurso "petroleo" infinitamenta

-- b 
-- puede evaluarse debido a la evaluacion diferida, ya que solo evalua la deuda y no le importa los recursos que tiene el pais 




