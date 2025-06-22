--3
type Academia = [Robot]

hayRobotSinProgramas :: String -> Academia -> Bool
hayRobotSinProgramas nombreRobot = any(\robot -> nombre robot == nombreRobot && null(programas robot))

esObstinado :: Robot -> Bool
esObstinado robot = length(programas robot) > 3 * nivelExperiencia robot

hayRobotsObstinados :: Academia -> Bool
hayRobotsObstinados = all esObstinado . filter((>16).nivelExperiencia)

--5
noPuedeDerrotarle :: Robot -> Robot -> Bool
noPuedeDerrotarle atacante victima = energia victima == energia (foldl atacarAVictima victima (programas atacante))
    where atacarAVictima victima programa = programa victima