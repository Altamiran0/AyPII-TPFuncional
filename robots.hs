data Robot = Robot {   
        nombre :: String,
        nivelExperiencia :: Int,
        energia :: Int,
        programas :: [Programa]
    }

--3
type Programa = Robot -> Robot

hayRobotSinProgramas :: String -> Academia -> Bool
hayRobotSinProgramas nombreRobot = any(\robot -> nombre robot == nombreRobot && null(programas robot))

esObstinado :: Robot -> Bool
esObstinado robot = length(programas robot) > 3 * nivelExperiencia robot

hayRobotsObstinados :: Academia -> Bool
hayRobotsObstinados = all esObstinado . filter((>16).nivelExperiencia)


instance Show Robot where
    show ( Robot nombre nivelExperiencia energia programas ) =
        "Robot { nombre = " ++ show nombre ++ ", nivelExperiencia = " ++ show nivelExperiencia ++ ", energia = " ++ show energia ++ ", #programas = " ++ show ( length programas ) ++ " }"

-- 4. 
f x [y] = y
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
seleccionarMayor _ [ elem ] = elem
seleccionarMayor criterio ( fst : snd : resto )
         | criterio fst >= criterio snd = seleccionarMayor criterio ( fst : resto )
         | otherwise = seleccionarMayor criterio ( snd : resto )

-- 4b.1 - mejorProgramaContra:
mejorProgramaContra :: Robot -> Robot -> Programa
mejorPrograma victima atacante = seleccionarMayor (\ programa -> ( energia victima ) - energia ( programa victima )) ( programas atacante )

-- 4b.2 - mejorOponente:
mejorOponente :: Robot -> Academia -> Robot
mejorOponente robot robots = seleccionarMayor (\ oponente -> ( poder oponente ) - ( poder robot )) robots
