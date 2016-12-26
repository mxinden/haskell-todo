import System.Environment

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
 -- dispatch "view" = view
 -- dispatch "remove" = remove

main = do
  (command:argList) <- getArgs
  dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
