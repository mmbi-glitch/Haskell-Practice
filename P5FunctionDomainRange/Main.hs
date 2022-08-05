-- Haskell program that demonstrates functions
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

main :: IO()
main = do
    putStr "\nsquareRootDebug -25.0 = "
    -- print (squareRoot (-25.0)) -- doesn't work
    print (squareRootDebug (-25.0)) -- works, but not really the best
    putStr "sqrt -25.0 = "
    print (sqrt (-25.0)) -- built in sqrt function prints Nan
    putStr "\nsquareRootError -25.0 = "
    print (squareRootError (-25.0))
    putStr "Hello!!!\n"

    -- many functions are only defined for certain input domains
-- partially defined by type of function 
-- but may also be limited to a certain range of domain values

squareRoot :: Float -> Float

-- range of an input domain can be limited by guards/if-then-else

-- first function => only defined for x >= 0 | doesn't account for values outside this range
squareRoot x
    | x >= 0 = sqrt x

-- second function => accounts for values outside range but no error

squareRootDebug :: Float -> Float
squareRootDebug x
    | x >= 0 = sqrt x
    | otherwise = -9999999.9999

-- third function => accounts for values outside range with apt error

squareRootError :: Float -> Float
squareRootError x 
    | x >= 0 = sqrt x
    | otherwise = error "sqrt only defined for positive values" -- error msgs really helpful for notifying domain restrictions

    