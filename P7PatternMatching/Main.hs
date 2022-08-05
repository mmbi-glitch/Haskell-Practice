-- Haskell program that demonstrates pattern matching

module P7PatternMatching.Main where

main :: IO()
main = do 
    putStr "add 7 9 = "
    print (add 7 9)
    putStr "addGuard 2 0 = "
    print (addGuard 2 0)


{- Pattern matching:
    
expression of function selected
    based on whether params function
    is called with matches a certain pattern

    Patterns include:
    
    literal - match specific values
    variable - match any arg
    wildcard - match any arg
    tupe - match a type
    list - match a list
-}

{- choose type of function based on use case -}

-- simple adding function that uses pattern matching

add :: Int -> Int -> Int
add a 0 = a -- 1st pattern matched only if 2nd arg is 0
add 0 b = b -- 2nd pattern matched only if 1st arg is 0
add a b = a + b -- if neither is 0, last pattern matched

-- same function with guards

addGuard :: Int -> Int -> Int
addGuard a b  
    | a == 0 = b
    | b == 0 = a
    | otherwise = a + b

-- multiply function - pattern matching with WILDCARDS

-- wildcard can be used to match arg that isnt important
multiply :: Int -> Int -> Int
multiply 0 _ = 0
multiply _ 0 = 0
multiply x y = x * y


