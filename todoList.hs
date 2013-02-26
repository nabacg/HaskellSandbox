import System.IO
import System.Directory
import Data.List
import System.Environment

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add)
            , ("view", view)
            , ("remove", remove)
            , ("bump", bump)]

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

remove :: [String] -> IO()
remove [fileName, numberString] = do
  replaceToDoItems [fileName, numberString] (\n todoTasks -> delete (todoTasks !! n) todoTasks)

bump :: [String] -> IO()
bump [fileName, numberString] = do
  replaceToDoItems [fileName, numberString] (\n oldItems -> oldItems !! n : delete (oldItems !! n) oldItems)
  
replaceToDoItems :: [String] -> (Int -> [String] -> [String]) -> IO()
replaceToDoItems [fileName, numberString] getNewItems = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let todoTasks = lines contents
  let number = read numberString
      newTodoItems = getNewItems number todoTasks

  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle

  removeFile fileName
  renameFile tempName fileName


view :: [String] -> IO()
view [fileName] = do 
  contents <- readFile fileName
  let todoItems = lines contents
      numberedTasks = zipWith (\n t -> show n ++ " - " ++ t) [0..] todoItems
  putStr $ unlines numberedTasks

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args


mainAdd = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")


mainRemove = do
  handle <- openFile "todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks

  putStrLn "These are your TO-DO items"
  putStr $ unlines numberedTasks
  putStrLn "Which do you want to delete?"
  numberString <- getLine

  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks

  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle

  removeFile "todo.txt"
  renameFile tempName "todo.txt"


-- bump :: [String] -> IO()
-- bump [fileName, numberString] = do
--   replaceToDoItems [fileName, numberString] (\n oldItems -> oldItems !! n : delete (oldItems !! n) oldItems)
  -- handle <- openFile fileName ReadMode
  -- (tempName, tempHandle) <- openTempFile "." "temp"
  -- contents <- hGetContents handle
  -- let todoTasks = lines contents
  -- let number = read numberString
  --     newTodoItems = todoTasks !! number : delete (todoTasks !! number) todoTasks

  -- hPutStr tempHandle $ unlines newTodoItems
  -- hClose handle
  -- hClose tempHandle

  -- removeFile fileName
  -- renameFile tempName fileName
