-- Haskell program that explains operators and different types of operator notation

module Main where

a = 2
b = 4

main :: IO()
main = do
    
    -- working with operator notations

    -- infix notation: '+' is an OPERATOR
    putStr "\ninfix notation (default for op): a + b = "
    print (a + b)

    -- prefix notation for operators with (): '+' is an OPERATOR
    putStr "nprefix notation for ops: (+) a b = "
    print ((+) a b)

    -- working with function notations

    -- prefix notation: 'mod' is a function
    putStr "\nprefix notation (default for functions): mod a b = "
    print (mod a b)

    -- infix notation for functions with ``: 'mod' is a function
    putStr "infix notation for functions: a `mod` b = "
    print (a `mod` b)

    putStr "\n(user-def function for mod 0 - find factors) a <-> b = "
    print (a <-> b)

    putStr "(user-def function for mod 0 - find factors) 1 <-> 1 = "
    print (1 <-> 1)

    -- order of operations defined by BINDING POWER and ASSOCIATIVITY

(<->) :: Int -> Int -> Bool 
a <-> b  
    | mod a b == 0  = True 
    | otherwise = False


