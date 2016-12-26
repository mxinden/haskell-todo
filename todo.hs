import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

main :: IO ()
main = do
  arguments <- getArgs
  parse arguments

parse :: [String] -> IO ()
parse ("-h" : _) = usage
parse ("add" : args) = add args
parse ("view" : args) = view args 
parse ("remove" : args) = remove args 
parse [] = usage
parse _ = usage

add :: [String] -> IO ()
add [] = usage
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = usage

view :: [String] -> IO ()
view [] = usage
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> "[" ++ show (n::Int) ++ "]  " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks
view _ = usage

remove :: [String] -> IO ()
remove [] = usage
remove [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newTodoItems
      hClose tempHandle
      removeFile "todo.txt"
      renameFile tempName "todo.txt")
remove _ = usage

usage :: IO ()
usage = do
  putStrLn "Usage: todo [-h] [action] [filename] [todo]"
  putStrLn "Actions: add, view or remove"
