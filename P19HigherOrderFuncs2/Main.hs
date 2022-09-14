-- Partial Application in Haskell

module P19HigherOrderFuncs2.Main where

main :: IO()

main = do   putStrLn ("Double all elems from 1..6 = " ++ show (doubleAll [1..6]))

{-
Partial application = another way to define one function in terms of another
    A partial application of a function is when a function thats takes TWO or more args
    is applied to ONE or more args. This allows one function to be defined
    as the result of a partial function application
    Really, a function applies another function to its args
-}

-- an example of partial application is as follows
-- the function double can be defined as partial application of the function multiply

multiply :: Int -> Int -> Int
multiply x y = x * y

-- double is defined as the function multiply applied to a single param 2
double :: Int -> Int
double = multiply 2

-- another example, define a function to double all elems in a list of ints

doubleAll :: [Int] -> [Int]
doubleAll = map double



{- 
there are two different partial applications, what are they?

    first, we know that map takes 2 params (function + list), but here
    there's only 1... so the first partial application defines the first param
    to map (the function - multiply)

    and then, there's multiply, which also takes 2 params, but here 
    also 1... so the second partial defins the 1st param for multiply

but what is type of this function???

There's a simple rule in Haskell that determines type of a partial application

if the type of a function f is: t1 -> t2 -> ... -> tn

e.g. multiply :: Int -> Int -> Int
e.g. map :: (Int -> Int) -> [Int] -> [Int]

and f is applied to args (k <= n): e1::t1 -> e2::t2 -> ... -> ek::tk

e.g. multiply is applied to 2, soo...

multiply :: Int <- only that arg

e.g. map is applied to multiply 2, soo....

map :: (Int -> Int) <- only that arg

The result is given by cancelling types t1..tk: tk+1 -> tk+2 -> ... -> tk+n

so cancel the first arg in double/multiply 2, what remains is...

(multiply 2)/double :: Int -> Int

so cancel the first arg in map, what remains is...

map (multiply 2) :: [Int] -> [Int]

-}

-- 



