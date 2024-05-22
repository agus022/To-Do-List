module Main where
--importacion de las librerias necesarias para el proyecto 
import System.IO
import Data.Time (UTCTime, getCurrentTime, parseTimeM, defaultTimeLocale)
import Data.List (intercalate)
import System.Directory (doesFileExist)
import Data.Char (toUpper, toLower)

--definición del tipo de datos Priority
data Priority = Baja | Media | Alta deriving (Show, Read, Eq)

--representa una tarea con título, categoría, fecha de vencimiento opcional y prioridad
data Task = Task {
    title :: String,
    category :: String,
    dueDate :: Maybe UTCTime,
    priority :: Priority
} deriving (Show, Read, Eq)

--Lista de tareas
type TaskList = [Task]

--definicion de los comandos que el usuario puede ingresar: agregar, eliminar, listar y salir
data Command = Nueva | Borrar | Lista | Salir deriving (Show, Read, Eq)

--INICIAR EL PROGRAMA   
main :: IO ()
main = do
    putStrLn ">> > Bienvenido a HaskellPlanner < <<"
    tasks <- loadTasks
    mainLoop tasks

--ciclo/bucle inicial donde el usuario podra seleccionar los comandos/acciones que quiera realizar
mainLoop :: TaskList -> IO ()
mainLoop tasks = do
    putStrLn "Ingresa la accion deseada (Nueva , Borrar, Lista, Salir):"
    cmd <- getLine
    case readCommand cmd of
        Just Nueva -> do
            task <- promptNewTask
            mainLoop (task : tasks)
        Just Borrar -> do
            putStrLn "Ingresa el TITULO de la tarea a BORRAR:"
            titleToRemove <- getLine
            let tasks' = removeTask titleToRemove tasks
            mainLoop tasks'
        Just Lista -> do
            putStrLn "---- LISTA DE TAREAS ----"
            mapM_ printTask tasks
            mainLoop tasks
        Just Salir -> do
            saveTasks tasks
            putStrLn "Byeeee :)"
        Nothing -> do
            putStrLn "ERROR!!! Comando invalido."
            mainLoop tasks

--Funcion para eleiminar la tarea toamdno como referencia su titulo 
removeTask :: String -> TaskList -> TaskList
removeTask _ [] = []
removeTask titleToRemove (task:tasks) =
    if title task == titleToRemove
        then removeTask titleToRemove tasks
        else task : removeTask titleToRemove tasks

-- Función para leer y convertir un comando de entrada del usuario
readCommand :: String -> Maybe Command
readCommand str = case reads (capitalize str) of
    [(cmd, "")] -> Just cmd
    _           -> Nothing

-- Función para capitalizar una cadena: primer carácter en mayúsculas y el resto en minúsculas
capitalize :: String -> String
capitalize (x:xs) = toUpper x : map toLower xs
capitalize []     = []

-- Función para solicitar al usuario los detalles de una nueva tarea
promptNewTask :: IO Task
promptNewTask = do
    putStrLn "Ingresa el titulo de la tarea nueva:"
    title <- getLine
    putStrLn "Ingresa la categoria de la tarea:"
    category <- getLine
    putStrLn "Ingresa la fecha de vencimiento (yyyy-mm-dd) o dejalo en blanco:"
    dueDateStr <- getLine
    dueDate <- if null dueDateStr
                then return Nothing
                else Just <$> parseDate dueDateStr
    putStrLn "Ingresa la prioridad (Baja, Media, Alta):"
    priorityStr <- getLine
    let priority = readPriority priorityStr
    return Task { title = title, category = category, dueDate = dueDate, priority = priority }

-- Función para analizar una fecha desde una cadena
parseDate :: String -> IO UTCTime
parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d" str

-- Función para leer y convertir una prioridad de entrada del usuario
readPriority :: String -> Priority
readPriority str = case reads (capitalize str) of
    [(priority, "")] -> priority
    _                -> Baja

-- Función para imprimir los detalles de una tarea
printTask :: Task -> IO ()
printTask task = do
    putStrLn $ "Titulo: " ++ title task
    putStrLn $ "Categoria: " ++ category task
    putStrLn $ "Fecha Vencimiento: " ++ maybe "None" show (dueDate task)
    putStrLn $ "Prioridad: " ++ show (priority task)
    putStrLn "-------------------------"

-- Función para guardar las tareas en un archivo
saveTasks :: TaskList -> IO ()
saveTasks tasks = do
    writeFile "tareas.txt" (show tasks)

-- Función para cargar las tareas desde un archivo
loadTasks :: IO TaskList
loadTasks = do
    fileExists <- doesFileExist "tareas.txt"
    if fileExists
        then read <$> readFile "tareas.txt"
        else return []
