module Personas(initPersonaMemory,getPersona,crearPersona, main) where

data Nombre = Nom String
data Apellido = Ap String
data NombreCompleto = Nombre Apellido

data Persona = Pers {
    idP :: Integer, nombre, telefono, direccion :: String, edad :: Integer
} deriving Show

data Trabajador = Jornalero {
    persona :: Persona,
    jornal  :: Double
} | Empleado {
    persona :: Persona,
    sueldo  :: Double
}

data PersonaMemory = Mem{
    nextId   :: Integer,
    personas :: [Persona]
} deriving Show

initPersonaMemory :: PersonaMemory
initPersonaMemory = Mem 0 []

crearPersona :: PersonaMemory -> String -> String -> String ->Integer ->PersonaMemory
crearPersona m nom tel dir ed = Mem ((nextId m) + 1) $ (Pers (nextId m) nom tel dir ed):(personas m)

getPersona :: PersonaMemory -> Integer ->Maybe Persona
getPersona m idPersona = get' $ filter (\p ->(idP p)==idPersona) $ personas m
    where get' [] = Nothing
          get' [a] = Just a
          get' _ = Nothing

ingresarPersona :: IO Persona
ingresarPersona = do
    putStrLn "Nombre: "
    nom  <- getLine 
    putStrLn "Tel: "
    tel <- getLine
    putStrLn "Dir: "
    dir <- getLine
    putStrLn "id: "
    idStr <- getLine
    id <- return $ read idStr
    putStrLn "edad: "
    edadStr <- getLine
    edad <- return $ read edadStr
    return $ Pers id nom tel dir edad

main = do
    p <- ingresarPersona
    print p

