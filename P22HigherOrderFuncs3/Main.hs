module P22HigherOrderFuncs3.Main where

list = [234,532,2345,123,435,365]

main :: IO()

main = do   putStrLn ("sort list = " ++ show (isort (>) [234,532,2345,123,435,365]))
            putStrLn ("sort list = " ++ show (isort (<) [234,532,2345,123,435,365]))
            putStrLn ("list 3rd elem = " ++ show thirdInList)

-- lamda expressions can also be used in conjuction with other HO function
-- features such as function level definitions
-- e.g. pow4 could be written with lamda expression like this

pow4 :: Int -> Int
pow4 = (\f x -> f(f x)) (\x -> x^2)

-- lamda expressions can be defined in many ways

addNum :: Num a => a -> a -> a
addNum = \m n -> n + m -- this is one function

addNum2 :: Num a => a -> a -> a
addNum2 n = \m -> n + m -- this is two functions


-- insertion sort
-- takes a comparator function and a list
-- outputs a list
isort :: (a -> a -> Bool) -> [a] -> [a]
-- base case - if list is empty, return an empty list
isort _ [] = []
-- otherwise what we do is go into the insertion loop
-- but first we keep recurively applying isort to the tail of the list
-- so we eventually end with up with an empty list and work our way backwards
isort f (h : t) = insert f h (isort f t)

-- performs the insertion loop
-- so we get given the temp variable 
insert :: (a -> a -> Bool) -> a -> [a] -> [a]
-- if there's nothing to compare with, we just return it
insert _ x [] = [x]
-- otherwise, we have to compare
insert f x (h : t)
    | f x h = x : h : t
    | otherwise = h : insert f x t

thirdInList = list !! 2