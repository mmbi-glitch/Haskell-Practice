module P14LazyEvaluation2.Main where

main :: IO()

main = do   putStrLn ("Find head of infinite list [1...] = " ++ show (head [1..]))
            putStrLn ("Take 5 elems from [1..] = " ++ show (take 5 [1..]))
            putStrLn ("Find head of tail of tail of [11, 14,..] = " ++ show (head (tail (tail [11,14..]))))
            putStrLn ("Find the first 5 prime nums greater than 100 = " ++ show (take 5 [x | x <- primes, x > 100]))
{-  

1 particularly nice feature of lazy evaluation: can work with infinite lists
    
Explainer:    
    
    1. with eager evaluations, args must be evaluated, so the entire list is constructed - an endless process
    2. but with lazy evaluation, only the elements that are required are evaluated.
    3. e.g. head (tail [1..]) works bcoz it knows the tail of [1..] is [2..] 
       i.e. it only needs to know the next elem in the list and then it can take that as the head

Infinite list comprehensions:

    1. List comprehensions can be intertwined with infinite lists 
       by writing a list comprehension with an infinite list source.
       e.g. get primes from an infinite list
    
-}


primes :: [Int]
primes = [x | x <- [2..], prime x]

prime :: Integral a => a -> Bool
prime n = n > 1 && ( n == 2 || null [ () | i <- [2..n-1], rem n i == 0] )




