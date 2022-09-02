{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Use product" #-}
module P17HigherOrderFuncs1.Main where
import Data.Char (isDigit)

main :: IO()

main = do   putStrLn ("Filterer all primes from [1..9] = " ++ show (filterer isPrime [1..9]))
            putStrLn ("Filter all digits from 4546fwert345 = " ++ show (filter isDigit "4546fwert345"))
            putStrLn ("ZipWither addition [1,2,3] [3,2,1] = " ++ show (zipWither (+) [1,2,3] [3,2,1]))
            putStrLn ("ZipWith addition [1,2,3] [3,2] = " ++ show (zipWith (+) [1,2,3] [3,2]))
            putStrLn ("Folder1 addition [1,2,3,4] = " ++ show (folder1 (+) [1,2,3,4]))
            putStrLn ("Folder division 2 [1,2,3] = " ++ show (folder (/) 2 [1,2,3]))
            putStrLn ("Folder1 subtraction [1,2,3] = " ++ show (folder1 (-) [1,2,3]))
            putStrLn ("Folder subtraction 2 [1,2,3] = " ++ show (folder (-) 2 [1,2,3]))
            putStrLn ("Foldel1 subtraction [1,2,3] = " ++ show (foldel1 (-) [1,2,3]))
            putStrLn ("Foldl1 subtraction [2,5,8,11] = " ++ show (foldl1 (-) [2,5,8,11]))
            putStrLn ("GetLengthHO [2,5,8,11] = " ++ show (getLengthHO [2,5,8,11]))

{-  MAP is just one higher order function that greatly reduces code 
    that needs to be written when writing functions.
    Others include: FILTER, ZIPWITH, FOLD 
-}

-- FILTER = HO function that filters/selects elems from a list
-- Only elems that have a certain property should be included in the returned list

-- could do it with list comprehension

filterer :: (a -> Bool) -> [a] -> [a]
filterer f list = [x | x <- list, f x]


isPrime :: Integral a => a -> Bool
isPrime n = n > 1 && ( n == 2 || null [ () | i <- [2..n-1], rem n i == 0] )

-- ZIPWITH = HO function that zips two lists together (combines each corresponding elems in each list using given function together and returns the resulting list)

-- could do it with pattern matching (but this assumes the lists are of equal length) 
zipWither :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWither _ [] [] = []
zipWither f (h1:t1) (h2:t2) = f h1 h2:zipWither f t1 t2

{- FOLD = HO function thats a list and func/operator 
           and reduces the list of elems down to a single value. 
           There are several different versions of a folding in 
           Haskell with slightly different behaviors.
-}

{-  folder1 folds a function into a non-empty list
    the inbuilt function is foldr1
    the fold'r' means it works right to left (from last to head)
-}

folder1 :: (a -> a -> a) -> [a] -> a

folder1 f [x] = x -- base case: folding a list with a single elem is just that elem

-- otherwise fold the tail and apply function to that value and the head of the list
folder1 f (h:t) = f h (folder1 f t)

{-

Stack trace of folder1...

folder1 f [x1, x2, x3]
-> f x1 (folder f [x2, x3])
-> f x1 (f x2 (folder f [x3]))
-> f x1 (f x2 (f x3))
-> f x1 (f x2 x3)

folder1 (-) [1, 2, 3]
= (-) 1 ((-) 2 3)
= (-) 1 (2 - 3)
= (-) 1 (-1)
= 1 - (-1)
= 2

-}

{-  folder/foldr is a bit different from folder1/foldr1; it additionally takes a starting value
    the starting value can be used to change the calculation; it also the function
    to be defined for an empty list; moreover, it allows a list of type a to be folded
    into a value of type b. This is probably because of the starting value
-}

folder :: (a -> b -> b) -> b -> [a] -> b
-- the function is f, starting value is s
folder f s [] = s -- folding an empty list is s itself
folder f s (h:t) = f h (folder f s t)

{-

Stack trace of folder...

folder f z [x1, x2, x3]
-> f x1 (folder f z [x2, x3])
-> f x1 (f x2 (folder f z [x3]))
-> f x1 (f x2 (f x3 (folder f z [])))
-> f x1 (f x2 (f x3 z))

folder (-) 2 [1, 2, 3]
= (-) 1 ((-) 2 ((-) 3 2))
= (-) 1 ((-) 2 (3 - 2))
= (-) 1 ((-) 2 (1))
= (-) 1 (2 - 1)
= (-) 1 (1)
= 1 - 1
= 0

-}

{-  
    There are also similar functions that fold the elements left-to-right instead.
    These are foldl and foldl1; note the L at the end, indicates left-to-right folding
-}


-- Foldel1 subtraction [1,2,3] = ((1-2)-3) -4


foldel1 :: (a -> a -> a) -> [a] -> a

foldel1 f [x] = x -- base case: folding a list with a single elem is just that elem

-- otherwise fold init list (list - last) and apply function to that value and the last elem of the list
foldel1 f list = f (foldel1 f (init list)) (last list)

{-

Stack trace of foldel1...

foldel1 f [x1, x2, x3]
-> f (foldel f [x1, x2]) x3
-> f (f (foldel f [x1]) x2) x3
-> f (f x1 x2) x3

foldel1 (-) [1, 2, 3]
= (-) ((-) 1 2) 3
= (-) (1 - 2) 3
= (-) (-1) 3
= -1 - 3
= -4

-}


{-
    For commutative operations, e.g. addition and multiplication, folding l-r or r-l gives same results.
    But for non-commutative operations, folding l-r or r-l gives different results, as can
    be seen in the stack traces.
-}

{- 
Many of the functions we have already looked at can be rewritten using
HO functions. 
-}

-- For instance, factorial can be written using HO functions.

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- we can use foldr (fold right to left) to rewrite the function
-- with a base of 1

factorialFold :: (Num a, Enum a) => a -> a
factorialFold n = foldr (*) 1 [1..n] -- could also use product :D

-- Also, we can find the length of the list this way

getLength :: [a] -> Int
getLength [] = 0
getLength (_:t) = 1 + getLength t

-- but we could also rewrite this using foldr

f :: a -> Int
f _ = 1

getLengthHO :: [a] -> Int
getLengthHO list = foldr (+) 0 (map f list)



