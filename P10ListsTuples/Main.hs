{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use foldr" #-}
module P10ListsTuples.Main where

{-
2 ways of building compound data structs in Haskell - Lists and Tuples.

Lists...
    • are a collection of elems of same type
    • are technically immutable (but because of pt 3, can be redefined)
    • have unspecified length
    • dont allow random access 
    • linear access through head/tail

Tuples...
    • also a collection of elems, but which may be of different types
    • are also immutable
    • have specified length (fixed num of elems) 
    • probably allow random access

-}

main :: IO()

-- lists must be a specific type
-- lists are declared with the type within square brackets
-- examples of lists:

intList = [1,2,3] :: [Integer]
charList = ['a','b', 'c'] :: [Char]
floatFloatList = [[1.2, 3.4, 5.6]] :: [[Float]]

{- type of a tuple is defined by:
    -- number of elems
    -- type of each elem
    -- order of elems
    -- e.g. (Int, String) /= (String, Int) -}

-- examples of tuples
twoBoolIntTuple = (True, 10) :: (Bool, Int)
twoStringIntTuple = ("Hello world", 5) :: (String, Int)
threeCharStringFloatTuple = ('A', "class", 159.272) :: (Char, String, Float)

main = do
    -- 'write out all elements' notation --
    putStrLn "\n ** LISTS EXAMPLES **\n"
    putStrLn ("intList = " ++ show intList)
    putStrLn ("charList = " ++ show charList)
    putStrLn ("floatFloatList = " ++ show floatFloatList)
    putStrLn "\n** WRITE OUT ALL ELEMENTS NOTATION **\n"
    putStrLn ("[1,2,3,4] = " ++ show [1,2,3,4])
    putStr ("['a', 'b', 'c', 'd'] = " ++ show ['a', 'b', 'c', 'd'])
    -- range notation --
    putStrLn "\n** RANGE NOTATION **\n"
    putStrLn ("[1..5] = " ++ show [1..5])
    putStrLn ("[2,4..10] = " ++ show [2,4..10])
    putStrLn ("['A','B'..'z'] = " ++ show ['A','B'..'z'])
    -- cons operator --
    putStrLn "\n** CONS OPERATOR **\n"
    putStrLn ("1:2:3:[] = " ++ show (1:2:3:[]))
    putStrLn ("'a':['b', 'c', 'd'] = " ++ show ('a':['b', 'c', 'd']))
    putStrLn ("2:3:[4..7] = " ++ show (2:3:[4..7]))
    putStrLn ("[2,3]:[] = " ++ show ([2,3]:[]))
    -- concatenation operator --
    putStrLn "\n** CONCAT OPERATOR **\n"
    putStrLn ("[1]++[2]++[3] = " ++ show ([1]++[2]++[3]))
    putStrLn ("['a']++['b', 'c', 'd'] = " ++ show (['a']++['b', 'c', 'd'])) -- all concatetating elems must be lists
    putStrLn ("[2,3]++[4..7] = " ++ show ([2,3]++[4..7]))
    putStrLn ("[2,3]++[] = " ++ show ([2,3]++[]))
    -- NOTE: cons and contat ops create NEW LISTS
    -- list functions --
    putStrLn "\n** LIST FUNCTIONS **\n"
    putStrLn("sumList of [3,5,6,6,7,8] = " ++ show (sumList [3,5,6,6,7,8]))
    putStrLn("lengthList of [3,5,6,6,7,8] = " ++ show (lengthList [3,5,6,6,7,8]))
    putStrLn("appendList of [3,5,6,6,7,8] && [1,2,3,5,6] = " ++ show (appendList [3,5,6,6,7,8] [1,2,3,5,6]))
    putStrLn("remove1stOccur of [3,5,6,7,8] = " ++ show (remove1stOccur 6 [3,5,6,7,8]))
    putStrLn("removeAllOccur of [3,5,6,6,7,8] = " ++ show (removeAllOccur 6 [3,5,6,6,7,8]))
    putStrLn("memberList 6 of [3,5,6,6,7,8] = " ++ show (memberList 6 [3,5,6,6,7,8]))
    putStrLn("memberList 9 of [3,5,6,6,7,8] = " ++ show (memberList 9 [3,5,6,6,7,8]))
    
    {- list comprehension ~~ set notation for lists
       follows the pattern: [expression | element <- source, condition]
       examines list of elements from source list, 
       for each elem, if condition = true, expression is evaluated and included in resulting list
    -}
    putStrLn "\n** SET NOTATION/LIST COMPREHENSION **\n"
    -- here, get all elems b from list [1,2,3,4,5] such that b is greater than 3
    putStrLn ("all elems > 3 from [1,2,3,4,5] = " ++ show [b | b <- [1,2,3,4,5], b > 3])
    -- here, get all elems x from list [1..10] such that remainder of x/2 is 1
    putStrLn ("all elems such that elem % 2 == 1 from [1..10] = " ++ show [x | x <- [1..10], x `mod` 2 == 1])
    -- list comprehensions allow multiple conditions, all of which must be true for elem to be included
    -- like here, elem must be both odd and even (result is [])
    putStrLn ("all elems both odd and even from [1..10] = " ++ show ([x | x <- [1..10], odd x, even x]))
    -- list comprehensions can do more -- you can do operations on lists with them!!
    putStrLn ("double a list [1..10] = " ++ show ([x*2 | x <- [1..10]]))
    putStrLn ("square odd elems in [1..10] = " ++ show ([x*x | x <- [1..10], odd x]))

    putStrLn "\n** TUPLES EXAMPLES **\n"
    putStrLn ("twoBoolIntTuple = " ++ show twoBoolIntTuple)
    putStrLn ("twoStringIntTuple = " ++ show twoStringIntTuple)
    putStrLn ("threeCharStringFloatTuple = " ++ show threeCharStringFloatTuple)

    -- tuple functions --
    putStrLn "\n** TUPLE FUNCTIONS **\n"
    putStrLn ("returnTuple 1 = " ++ show (returnTuple 1))
    putStrLn ("quickSort [8,2,4,7,1,3,9,6,5] = " ++ show (quickSort [8,2,4,7,1,3,9,6,5]))
    putStrLn ("quickFastSort [8,2,4,7,1,3,9,6,5] = " ++ show (quickFastSort [8,2,4,7,1,3,9,6,5]))


-- functions use patterns to match lists as args
-- standard patten is (head:tail) or (x:xs), which describes a list by its head and tail

-- sum all elems in a list
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- get the length of a list
lengthList :: [Int] -> Int
lengthList [] = 0
lengthList (h:t) = 1 + lengthList t

-- append a list to another
appendList :: [Int] -> [Int] -> [Int]
appendList [] list = list
appendList (h:t) list = h:appendList t list 

{-
append [2,3,4,5] [6,7]
= 2:(append [3,4,5] [6,7])
= 2:(3:(append [4,5] [6,7]))
= 2:(3:(4:(append [5] [6,7])))
= 2:(3:(4:(5:(append [] [6,7]))))
= 2:(3:(4:(5:[6,7])))
= 2:(3:(4:[5,6,7]))
= 2:(3:[4,5,6,7])
= 2:[3,4,5,6,7]
= [2,3,4,5,6,7]
-}

-- remove 1st occurence of elem from list --
remove1stOccur :: Int -> [Int] -> [Int]
remove1stOccur _ [] = []
remove1stOccur e (h:t) 
    | e == h    = t -- if elem to remove matches with head, return the tail
    | otherwise = h:remove1stOccur e t

removeAllOccur :: Int -> [Int] -> [Int]
removeAllOccur _ [] = []
removeAllOccur e (h:t) 
    | e == h    = removeAllOccur e t -- if elem to remove matches with head, return the tail 
                                     -- AND implement the function on the remainder of the list
    | otherwise = h : removeAllOccur e t

memberList :: Int -> [Int] -> Bool
memberList _ [] = False
memberList e (h:t) 
    | e == h = True
    | otherwise = memberList e t

-- tuples are useful for functions that need to return more than one value
-- with a tuple return value, can return all values needed

returnTuple :: Int -> (Int, Int, Int)
returnTuple x = (x, x+1,x+2)

-- tuple functions can use pattern matching to extract individual elems from tuple params
first :: (Int, Int) -> Int
first (a, b) = a

second :: (Int, Int) -> Int
second (a, b) = b

{- Quicksort sorts a list of elements by selecting a pivot value
   and splitting the list into 2 groups:
    • the elements less than the pivot
    • the elements greater than the pivot
   These two groups can then be sorted separately  
-}

{- how to quicksort:
    s1. define the pivot -- start
    s2. partition the list into 2 groups based on pivot
    s3. in the LT partition, if the head is less than the pivot, 
        then partition the list again AND append the head, 
        otherwise partition list again
    s4. same for GTE, if head is >= than pivot, partition list again AND append head,
        other just partition list again
    s5. put it all together
-}    

-- quicksort with no list comprehension --

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (h:t) = quickSort (partitionLT t h) ++ [h] ++ quickSort (partitionGTE t h)

partitionLT :: [Int] -> Int -> [Int]
partitionLT [] _ = []
partitionLT (h:t) pivot 
    | h < pivot     = h:partitionLT t pivot
    | otherwise     = partitionLT t pivot

partitionGTE :: [Int] -> Int -> [Int]
partitionGTE [] _ = []
partitionGTE (h:t) pivot
    | h >= pivot = h:partitionGTE t pivot
    | otherwise = partitionGTE t pivot

-- quicksort with list comprehension --

quickFastSort :: [Int] -> [Int]
quickFastSort [] = []
quickFastSort (h:t) = quickFastSort ([x | x <- t, x < h]) ++ [h] ++ quickFastSort ([x | x <- t, x >= h])