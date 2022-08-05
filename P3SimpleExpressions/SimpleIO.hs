module SimpleIO where

printer = do
    putStrLn "Hello, Haskell"
    putStr "No Newline. See? Next part starts right here -> "
    putStrLn "In putStr and putStrLn\nyou can explicitly use newline character"
    print "Print is for all types"
    print (1 > 5)


