-- Haskell program that shows 
-- how to use the different types

module P4TypesConditionals.Main where

import Data.Char  -- contains built in conversion functions for chars


main :: IO()
main = do

    -- using the char type

    putStr "\n"
    putStr "char for code 65 = "
    print (chr 65) -- chr :: Int -> Char
    putStr "char code of 'A' = "
    print (ord 'A') -- ord :: Char -> Int
    putStr "char C to lowercase = "
    print (toLowerCase 'C')
    putStr "char g to uppercase = "
    print (toUpperCase 'g')
    putStr "char J to uppercase = "
    print (toUpperCase 'J')
    putStr "is ! a digit = "
    print (isItDigit '!')

    -- using the String type

    -- print String to stdout using putStr/Ln or print (which does not recognize escape chars)
    putStr "\84hi\115 is a \t line with no newline " -- escape chars work ONLY for putStr/Ln
    print "See? print() prints -> \n"
    putStrLn "This is a line with a newline"

    -- using the Int and Integer types
    putStr "factorial of 21 using Int type - results in random number (exceeds range): "
    print (factInt 21)
    putStr "factorial of 21 using Integer type: "
    print (factInteger 21)

    -- using Floats

    -- note explicit definition of 3 as Int
    -- print ((3::Int) * (5.5 :: Float)) => this will give an error that these types are not the same
    
    -- conversion from int to float (using fromIntegral function, 
    -- which converts to Num - general purpose type for numbers)
    putStr "Multiplying float with int (which gets converted to num): "
    print ((3.5 :: Float) * fromIntegral (12::Int))

    -- conversion from float to int using ceiling, round, floor, and truncate
    
    putStr "Ceiling of 22.843 = "
    print (ceiling 22.843)
    putStr "Floor of 17.234 = "
    print (floor 17.234)
    putStr "Truncation of 11111.222 = "
    print (truncate 11111.222)
    putStr "Rounding 22.5 = "
    print (round 22.5)
    putStr "Rounding 22.50000000001 = "
    print (round 22.50000000001)

    -- strictness of types --
    putStr "average of 3 numbers (truncates to int): "
    print (average 2 4 2) -- rounds to an integer bcoz return type is int
    putStr "average of 3 numbers (floating-point precision): "
    print (averageFloat 2 4 2) -- this time, gives the right floating-point answer

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --

    -- creating other char-related functions using ord and chr

offset = ord 'A' - ord 'a'

toUpperCase :: Char -> Char
toUpperCase c = chr (ord c + offset)

toLowerCase :: Char -> Char
toLowerCase c = chr (ord c - offset)

isItDigit :: Char -> Bool
isItDigit c = ('0' <= c) && (c <= '9')

    -- integer related functions
factInt::Int->Int
factInt n = product [1..n]

factInteger :: Integer -> Integer
factInteger n = product [1..n]

-- float related functions

average :: Int -> Int -> Int -> Int
-- average a b c = (a + b + c)/3 -- gives an error saying that / is meant ONLY for fractional types
average a b c = div (a + b + c) 3 -- we use the div function instead

-- average of 3 ints can be a float, thus we first need to convert int to float, then divide by int
-- in this case, we are using only floats/nums, so regular / will work 
averageFloat :: Int -> Int -> Int -> Float
averageFloat a b c = fromIntegral(a + b + c) / 3.0