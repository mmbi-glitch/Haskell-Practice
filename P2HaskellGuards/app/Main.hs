module Main where

import Lib ()

main :: IO ()
main = do
    print (absv 2)
    print (absv (-2))
    print (maxi3 3 3 3)
    print (maxi3 8 9 10)
    print (maxi3_2 2 9 5)
    print (facIfThen 42)
    print (fac 42)

-- *guards* = used to express conditions/cases of a function,
--            similar to piecewise functions in maths

{- A guard function defined as follows:
    
    1. function prototype (same as a normal function)
    
    2. function body -> <func name> <params>
        | <guard 1> = <expression 1>
        | <guard 2> = <expression 2>
        | <guard n> = <expression n>

        define guards with the '|' symbol
-}

-- guard function that finds absolute value of a number

absv :: Int -> Int
absv x
        | x >= 0 = x
        | x < 0 = -x
        | otherwise = x -- otherwise condition that always evaluates to true 
                        -- (should be final guard)

-- another guard function, find max value given 3 values

maxi3 :: Int -> Int -> Int -> Int
maxi3 a b c
        | (a > b) && (a > c) = a
        | b > c = b
        | otherwise = c

-- another way to write max3

maxi3_2 :: Int -> Int -> Int -> Int
maxi3_2 a b c = maxi a (maxi b c) -- note the use of brackets =>
                                  -- no brackets around ALL args, like in functions of other langs
                                  -- only around INDIVIDUAL args 
                                  -- proper way to write it would be
                                  -- (maxi (a) (maxi (b) (c)))
                                  -- but thats just so WORDY

-- function that finds max value given 2 values

maxi :: Int -> Int -> Int
maxi a b
    | a > b = a
    | otherwise = b

-- practice: the factorial function

facIfThen :: Int -> Int -- with if, then conditions
facIfThen n =
    if n == 0 then 1
    else n * facIfThen (n - 1)


fac :: Int -> Int -- with guard clauses
fac n
    | n == 0 = 1
    | otherwise = n * fac (n - 1)
