data Robot = Robot {   
        nombre :: String,
        nivelExperiencia :: Int,
        energia :: Int,
        programas :: [Programa]
    }

type Programa = Robot -> Robot

instance Show Robot where
    show ( Robot nombre nivelExperiencia energia programas ) =
        "Robot { nombre = " ++ show nombre ++ ", nivelExperiencia = " ++ show nivelExperiencia ++ ", energia = " ++ show energia ++ ", #programas = " ++ show ( length programas ) ++ " }"