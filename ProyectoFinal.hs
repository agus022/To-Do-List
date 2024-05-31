module Main where
import System.IO
import Data.Time (UTCTime, getCurrentTime, parseTimeM, defaultTimeLocale, formatTime)
import Data.List (intercalate)
import System.Directory (doesFileExist)
import Data.Char (toUpper, toLower)
import Control.Monad (unless)
import Control.Exception (bracket)
import Control.Applicative ((<|>))

data Priority = Baja | Media | Alta deriving (Show, Read, Eq)

data Task = Task {
    title :: String,
    category :: String,
    dueDate :: Maybe UTCTime,
    priority :: Priority
} deriving (Show, Read, Eq)

type TaskList = [Task]

data Command = Nueva | Borrar | Editar | Lista | Filtrar | Salir deriving (Show, Read, Eq)

main :: IO ()
main = do
    putStrLn ">> > Bienvenido a HaskellPlanner < <<"
    tasks <- loadTasks
    mainLoop tasks

mainLoop :: TaskList -> IO ()
mainLoop tasks = do
    putStrLn "Ingresa la acción deseada (Nueva, Borrar, Editar, Lista, Filtrar, Salir):"
    cmd <- getLine
    case readCommand cmd of
        Just Nueva -> do
            task <- promptNewTask
            mainLoop (task : tasks)
        Just Borrar -> do
            putStrLn "Ingresa el TITULO de la tarea a BORRAR:"
            titleToRemove <- getLine
            let tasks' = removeTask titleToRemove tasks
            unless (length tasks == length tasks') $ putStrLn "Tarea borrada."
            mainLoop tasks'
        Just Editar -> do
            putStrLn "Ingresa el TITULO de la tarea a EDITAR:"
            titleToEdit <- getLine
            tasks' <- editTask titleToEdit tasks
            mainLoop tasks'
        Just Lista -> do
            putStrLn "---- LISTA DE TAREAS ----"
            mapM_ printTask tasks
            mainLoop tasks
        Just Filtrar -> do
            putStrLn "Ingresa la prioridad para filtrar las tareas (Baja, Media, Alta):"
            priorityStr <- getLine
            let priorityVal = readPriority priorityStr
            let filteredTasks = filterTasksByPriority priorityVal tasks
            putStrLn $ "---- TAREAS CON PRIORIDAD " ++ show priorityVal ++ " ----"
            mapM_ printTask filteredTasks
            mainLoop tasks
        Just Salir -> do
            saveTasks tasks
            putStrLn "Byeeee :)"
        Nothing -> do
            putStrLn "ERROR!!! Comando inválido."
            mainLoop tasks

removeTask :: String -> TaskList -> TaskList
removeTask titleToRemove = filter ((/= titleToRemove) . title)

readCommand :: String -> Maybe Command
readCommand str = case reads (capitalize str) of
    [(cmd, "")] -> Just cmd
    _           -> Nothing

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

promptNewTask :: IO Task
promptNewTask = do
    putStrLn "Ingresa el título de la tarea nueva:"
    title <- getLine
    putStrLn "Ingresa la categoría de la tarea:"
    category <- getLine
    dueDate <- promptDueDate
    putStrLn "Ingresa la prioridad (Baja, Media, Alta):"
    priorityStr <- getLine
    let priority = readPriority priorityStr
    return Task { title = title, category = category, dueDate = dueDate, priority = priority }

promptDueDate :: IO (Maybe UTCTime)
promptDueDate = do
    putStrLn "Ingresa la fecha de vencimiento (yyyy-mm-dd) o déjalo en blanco:"
    dueDateStr <- getLine
    if null dueDateStr
        then return Nothing
        else do
            parsedDate <- parseDate dueDateStr
            case parsedDate of
                Just date -> return (Just date)
                Nothing -> do
                    putStrLn "Fecha inválida. Intenta de nuevo."
                    promptDueDate

parseDate :: String -> IO (Maybe UTCTime)
parseDate str = return $ parseTimeM True defaultTimeLocale "%Y-%m-%d" str

readPriority :: String -> Priority
readPriority str = case reads (capitalize str) of
    [(priority, "")] -> priority
    _                -> Baja

printTask :: Task -> IO ()
printTask task = do
    putStrLn $ "Título: " ++ title task
    putStrLn $ "Categoría: " ++ category task
    putStrLn $ "Fecha Vencimiento: " ++ maybe "None" (formatTime defaultTimeLocale "%Y-%m-%d") (dueDate task)
    putStrLn $ "Prioridad: " ++ show (priority task)
    putStrLn "-------------------------"

saveTasks :: TaskList -> IO ()
saveTasks tasks = bracket (openFile "tareas.txt" WriteMode) hClose $ \handle ->
    hPutStrLn handle (show tasks)

loadTasks :: IO TaskList
loadTasks = do
    fileExists <- doesFileExist "tareas.txt"
    if fileExists
        then bracket (openFile "tareas.txt" ReadMode) hClose $ \handle -> do
            contents <- hGetContents handle
            let tasks = read contents
            length tasks `seq` return tasks 
        else return []

editTask :: String -> TaskList -> IO TaskList
editTask titleToEdit tasks = do
    let taskToEdit = filter ((== titleToEdit) . title) tasks
    if null taskToEdit
        then do
            putStrLn "No se encontró la tarea con ese título."
            return tasks
        else do
            putStrLn "Editando tarea:"
            editedTask <- promptEditTask (head taskToEdit)
            let updatedTasks = editedTask : removeTask titleToEdit tasks
            putStrLn "Tarea editada."
            return updatedTasks

promptEditTask :: Task -> IO Task
promptEditTask oldTask = do
    putStrLn $ "Ingresa Titulo NUEVO (titulo actual " ++ title oldTask ++ "):"
    newTitle <- getLine
    putStrLn $ "Ingresa Categoria NUEVA (categoria actual " ++ category oldTask ++ "):"
    newCategory <- getLine
    newDueDate <- promptDueDate
    putStrLn $ "Ingresa Prioridad NUEVA (prioridad actual " ++ show (priority oldTask) ++ "):"
    newPriorityStr <- getLine
    let newPriority = readPriority newPriorityStr
    return Task { title = if null newTitle then title oldTask else newTitle
                , category = if null newCategory then category oldTask else newCategory
                , dueDate = newDueDate <|> dueDate oldTask
                , priority = newPriority }

filterTasksByPriority :: Priority -> TaskList -> TaskList
filterTasksByPriority p = filter ((== p) . priority)
