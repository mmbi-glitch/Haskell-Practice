module Hello(printHello) where

printHello :: IO()
printHello = do
    putStrLn "\nHello and Welcome to Haskell"
    putStrLn "\nSimple Expressions!"
