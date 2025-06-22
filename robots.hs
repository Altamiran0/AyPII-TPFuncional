type Academia = [Robot]

hayRobotSinProgramas :: String -> Academia -> Bool
hayRobotSinProgramas nombreRobot = any(\robot -> nombre robot == nombreRobot && null(programas robot))

esObstinado :: Robot -> Bool
esObstinado robot = length(programas robot) > 3 * nivelExperiencia robot

hayRobotsObstinados :: Academia -> Bool
hayRobotsObstinados = all esObstinado . filter((>16).nivelExperiencia)