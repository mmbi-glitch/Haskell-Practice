-- Higher Order Functions in Haskell

module P16HigherOrderFuncs1.Main where
import Data.Char (toLower)

main :: IO()

main = do   putStrLn ("HO Func - Cube all elems in [1..9] = " ++ show (cubeAll [1..9]))
            putStrLn ("ListComp + PolyMorph - Cube all elems in [1..9] = " ++ show (cubeAllPoly [1..9]))
            putStrLn ("User Def Map 1 - Cube all elems in [1..9] = " ++ show (map1 cubePoly [1..9]))
            putStrLn ("User Def Map 2 - Change letters ABCDEFG to lowercase = " ++ show (map2 toLower "ABCDEFG"))
            putStrLn ("Inbuilt Map - take sqrt of all elems in [4, 9, 16, 25] = " ++ show (map sqrt [4,9,16,25]))

{-
        HIGHER ORDER FUNCTIONS

        1. A function that takes a function as an arg or returns one as a result

        2. HO funcions are extremely useful - allow common code patterns to be reused
-}

-- Common pattern for HO functions: apply a certain function to every elem in list and return resulting list

-- For example, this cubeAll function calls cubeAll and cube repeatedly...

cube :: Int -> Int
cube x = x * x * x

cubeAll :: [Int] -> [Int]
cubeAll [] = []
cubeAll (h:t) = cube h:cubeAll t

-- could also be written using polymorphism and list comprehensions

cubePoly :: Num a => a -> a
cubePoly x = x*x*x


cubeAllPoly :: Num a => [a] -> [a]
cubeAllPoly list = [ cubePoly x | x <- list]

{- 

Problem with above functions.. 
    Specific operation of calculating cube is hardcoded into the function
    Say we wanted to replace cube with square... Then, we'd have to hardcode square function in
    Instead we want to separate specific function from logic of applying a function to every elem in list.
    HO functions allow us to do just that
-}

-- HO function that applies a function to every elem in list is map

{- A map function...
    takes a function f as 1st param
    which accepts a param of type a and returns value of type b
    the 2nd param is a list of type a and return value is a list of type b
-}

-- could be written like this 

map1 :: (a -> b) -> [a] -> [b]
map1 _ [] = []
map1 f (h:t) = f h:map1 f t

-- and like this

map2 :: (a -> b) -> [a] -> [b]
map2 f list = [f x | x <- list]

-- or just use the inbuilt function: map <func> <list>