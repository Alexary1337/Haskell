import Data.List

showTODO :: (Int, String) -> IO ()
showTODO (n, todo) = putStrLn (show n ++ ": " ++ todo)

getCommand :: [String] -> IO ()
getCommand todos = do
    command <- getLine
    executeCommand command todos

executeCommand :: String -> [String] -> IO ()
executeCommand ('a':'d':'d':' ':todo) todos = getCommand (todos ++ [todo])
executeCommand ('d':'e':'l':' ':'-':'a':rest) todos = getCommand []
executeCommand ('d':'e':'l':' ':num ) todos =
    case deleteTODO (read num) todos of
        Nothing -> do
            putStrLn "Todo number not found"
            getCommand todos
        Just todos' -> getCommand todos'
executeCommand ('s':'o':'r':'t':' ':way ) todos =
    case sortTODOs (read way) todos of
        Nothing -> do
            putStrLn "incorrect"
            getCommand todos
        Just todos' -> getCommand todos'
executeCommand "view" todos = view todos
executeCommand "q" todos = return ()
executeCommand "help" todos = help()
--executeCommand [] todos = return ()
executeCommand  command todos = do
    putStrLn ("Invalid command: `" ++ command ++ "`")
    getCommand todos

deleteTODO :: Int -> [a] -> Maybe [a]
deleteTODO 1 (_:as) = Just as
deleteTODO n (a:as) = do
    as' <- deleteTODO (n-1) as
    return (a:as')
deleteTODO _  [] = Nothing

--sortTODOs :: Char -> [a] -> Maybe [a]
sortTODOs 'a' (a:as) = Just (quicksort (a:as))
sortTODOs 'd' (a:as) = Just $ reverse $ quicksort (a:as)
sortTODOs _ (a:as) = Nothing

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

view :: [String] -> IO ()
view todos = do
    putStrLn ""
    putStrLn "Your todo list:"
    case todos of
        [] -> putStrLn "Empty"
        _ -> mapM_ showTODO (zip [1..] todos)
    command <- getLine
    executeCommand command todos
    
help :: () -> IO ()
help () = do
    putStrLn "Help:"
    putStrLn "view - Show all todos"
    putStrLn "add <String> - Add todo element"
    putStrLn "del <Int> - Delete the todo element by index"
    putStrLn "del -a - Delete all todo elements"
    putStrLn "sort -a - Sort todos by ascending"
    putStrLn "sort -d - Sort todos by descending"
    putStrLn "q - Quit"
    getCommand []

main = do
    help()