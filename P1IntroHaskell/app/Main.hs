module Main where

import Lib

 -- declare a statement like as follows: var name = value :: var type
 -- type declaration not necessary as type is automatically inferred 
x = 3
hello = "Hi Mom, I can do Haskell"


-- need to have main function
main :: IO ()
main = do 
    printHello
    print x
    print hello
    print (doubleIt 5)
    print (doubleIt 15)
    print (gt 10 15)
    print (gt 15 10)
    print (allEqual 10 10 10)
    print (square 20)


-- ~~~~ declare GLOBAL user-defined functions below main ~~~~~

-- function declaration like this:

--  1. the function prototype => <func name> :: <arg1 type> -> <arg2 type> -> <...> -> <return type>

--  2. the function definition => <func name> <args> = <func body>


-- this is a function that doubles a number

doubleIt :: Int -> Int
doubleIt x = x * 2

-- this is a function that determines if 'a' is greater than 'b'

gt :: Int -> Int -> Bool -- <- func name :: param a -> param b -> return value
gt a b = a > b

-- function that squares a number

square :: Int -> Int
square n = n * n

-- function that establishes equality between 3 numbers

allEqual :: Int -> Int -> Int -> Bool
allEqual x y z = (x == y) && (y == z)




printHello :: IO ()
printHello = do 
    putStrLn "Hello World"  
    
{- 
       ~~~~ PROGRAM NOTES ~~~~

       1. How to run Haskell (.hs) files...

        a. Use the command -> ghci <path to hs file>
           This allows you to run the file throught the GHC interpreter
           
        b. Use the command -> ghc --make <path to hs file>
           This compiles the hs file into an executable, which you can then run

        c. Use stack and Haskelly (there will be buttons in the linter)

       
    
-}


