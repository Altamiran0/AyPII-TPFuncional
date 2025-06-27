-- 1
data Robot = Robot {   
        nombre :: String,
        nivelExperiencia :: Int,
        energia :: Int,
        programas :: [Programa]
    }

type Programa = Robot -> Robot

-- Instancia de "Show" que permite vizualizar un robots.
instance Show Robot where
    show ( Robot n nExp e p ) = "Robot { nombre = " ++ show n ++ ", nivelExperiencia = " ++ show nExp ++ ", energia = " ++ show e ++ ", programas = " ++ show ( length p ) ++ " }"
-- Instancia de "Eq" que permite comparar dos robots.
instance Eq Robot where
    (==) ( Robot n_1 nExp_1 e_1 p_1 ) ( Robot n_2 nExp_2 e_2 p_2 ) = n_1 == n_2 && nExp_1 == nExp_2 && e_1 == e_2 && ( length p_1 == length p_2 )
    (/=) ( Robot n_1 nExp_1 e_1 p_1 ) ( Robot n_2 nExp_2 e_2 p_2 ) = n_1 /= n_2 && nExp_1 /= nExp_2 && e_1 /= e_2 && ( length p_1 /= length p_2 )

recargaBateria :: Int -> Programa
recargaBateria n robot = robot { energia = energia robot + n }

descargaElectrica :: Programa
descargaElectrica robot 
    | energia robot > 10 = robot { energia = (energia robot) - 10}
    | otherwise = robot { energia = div (energia robot) 2 }

olvidarProgramas :: Int -> Programa
olvidarProgramas n robot  = robot { programas = drop n (programas robot) }

autoAtaque :: Programa
autoAtaque robot 
    | null (programas robot) = robot { nombre = "ERROR", nivelExperiencia = 0, energia = 0, programas = []}
    | otherwise = (head (programas robot)) robot
    
-- 2
poder :: Robot -> Int
poder robot = energia robot + ((nivelExperiencia robot) * (length (programas robot)))

danio :: Robot -> Programa -> Int
danio robot programa 
    | (energia robot) == energia (programa robot) = 0
    | otherwise = (energia (programa robot) - (energia robot))

diferenciaDePoder :: Robot -> Robot -> Int
diferenciaDePoder r1 r2 = abs (poder r1 - poder r2)

--3
type Academia = [ Robot ]

hayRobotSinProgramas :: String -> Academia -> Bool
hayRobotSinProgramas nombreRobot = any(\robot -> nombre robot == nombreRobot && null(programas robot))

esObstinado :: Robot -> Bool
esObstinado robot = length(programas robot) > 3 * nivelExperiencia robot

hayRobotsObstinados :: Academia -> Bool
hayRobotsObstinados = all esObstinado . filter((>16).nivelExperiencia)

-- 4. 
f :: Ord a1 => (a2 -> a1) -> [a2] -> a2
-- Agregado y modificado para solucionar un warning ----------
f _ [] = error "f: no se puede seleccionar el mayor de una lista vacía."
-- f x [y] = y
f _ [y] = y
---------------------------------------------------------------
f x (y1:y2:ys)
      | x y1 >= x y2 = f x (y1:ys)
      | otherwise = f x (y2 : ys)

-- La funcion selecciona un elemento de una lista aplicando sobre cada elemento la funcion que recibe y reevaluando la lista con recursividad excluyendo al elemento con menor resultado.
-- Para entenderlo mejor, supongamos que le pasamos la funcion "energia", una lista de robots y analicemos los casos posibles:
--      Si la lista solo tiene un robot entonces lo retorna. 
--      Si tiene dos o mas robots aplica la funcion sobre cada uno y compara la energia de ambos para:
--          Si el primer robot tiene mas energia entonces creo una nueva lista con el como "head" y el resto de robots como "tail". Luego la reevaluo llamando recursivamente a la funcion "f" con la funcion "x" como parametro.
--          Si es el segundo robot el que tiene mas energia entonces hago tambien creo y reevaluo la nueva lista, pero ahora con el segundo robot como "head".

-- 4a - Version mas expresiva:
seleccionarMayor :: Ord a1 => (a2 -> a1) -> [a2] -> a2
seleccionarMayor _ [] = error "seleccionarMayor: no se puede seleccionar el mayor de una lista vacía."
seleccionarMayor _ [ item ] = item
seleccionarMayor criterio ( frsItem : sndItem : rest )
         | criterio frsItem >= criterio sndItem = seleccionarMayor criterio ( frsItem : rest )
         | otherwise = seleccionarMayor criterio ( sndItem : rest )

-- 4b.1 - mejorProgramaContra:
mejorProgramaContra :: Robot -> Robot -> Programa
mejorProgramaContra victima atacante = seleccionarMayor (\ programa -> energia victima - energia ( programa victima )) ( programas atacante )

-- 4b.2 - mejorOponente:
mejorOponente :: Robot -> Academia -> Robot
mejorOponente robot = seleccionarMayor ( diferenciaDePoder robot )

--5
noPuedeDerrotarle :: Robot -> Robot -> Bool
noPuedeDerrotarle atacante victima = energia victima == energia (foldl atacarAVictima victima (programas atacante))
    where atacarAVictima vict prog = prog vict