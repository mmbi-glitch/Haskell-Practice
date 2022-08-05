module P9DataStructs.Main where

import Data.List

main :: IO()

-- example of list => a string is a list of chars
hello = "String" :: String
-- example of tuple => this is a 3-tuple of integers
threeTuple = (12, 12, 12) :: (Int, Int, Int)
-- another example of tuple => 3-tuple consisting of Char, String, Float
threeTupleDifferent = ('a', "class", 159.272) :: (Char, String, Float)
-- we can define custom type => it's a 2-tuple that represents a 2D point!
type Point2D = (Int, Int)

main = do
    putStr "3-tuple with ONLY int types: "
    print threeTuple
    putStr "3-tuple with different types: "
    print threeTupleDifferent
    putStr "Point2D 3 9 = "
    print (makePoint 3 9)
    putStr "Getting first elem from Point2D 3 9 tuple: "
    print (elemFrom2Tuple (makePoint 3 9) 0)
    putStr "Getting second elem from Point2D 3 9 tuple: "
    print (elemFrom2Tuple (makePoint 3 9) 1)
    -- putStr "Accessing non-existent elem from Point2D 3 9 tuple: "
    -- print (elemFrom2Tuple (makePoint 3 9) 2)
    putStr "Making a 2D point with custom type = "
    print (makePoint2D 2 5)
    let myPoint = makePoint2D 2 5
    putStr "Declaring var -> myPoint = "
    print myPoint
    putStr "myPoint.X = "
    print (getX myPoint)
    putStr "myPoint.Y = "
    print (getX myPoint)
    let myPoint2 = makePoint2D 8 7
    putStr "myPoint = ("
    putStr (showTuples [myPoint])
    putStr "), myPoint = ("
    putStr (showTuples [myPoint2])
    putStr ")\n"
    putStr "Adding both points = "
    print (addPoint2D myPoint myPoint2)
    putStr "Finding distance between both points = "
    print (distance myPoint myPoint2)

{- 
    2 main ways of building compound data:
        1. lists (similar to linked lists)
        2. tuples (similar to arrays )

    Both combine multiple values/elements into a single object
    and can be used to represent different types of structured data

    Tuples:
        1. combine a predetermined number of elems of some predetermined types togther
        2. immutable and may contain different types
        3. useful for returning multiple vals from function
        4. type of a tuple defined by num of elems, their types, and order of elems in tuple
-}

-- pattern matching to get indiv elems from tuple

elemFrom2Tuple :: (Int, Int) -> Int -> Int
elemFrom2Tuple (a, b) 0 = a
elemFrom2Tuple (a, b) 1 = b
elemFrom2Tuple (a, b) _ = error "Tuple index out of bounds"

-- combine 2 values into tuple function

makePoint :: Int -> Int -> (Int, Int)
makePoint x y = (x, y)

-- makePoint using the custom Point2D type :D

makePoint2D :: Int -> Int -> Point2D
makePoint2D x y = (x, y)

getX :: Point2D -> Int
getX (x, _) = x

getY :: Point2D -> Int
getY (_, y) = y

-- additional functions that use custom tuple type

addPoint2D :: Point2D -> Point2D -> Point2D
addPoint2D (x1, y1) (x2, y2) = (x1+x2, y1+y2)

distance :: Point2D -> Point2D -> Float
distance (x1, y1) (x2, y2) = 
    sqrt (fromIntegral ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)))

showTuples :: [Point2D] -> String
showTuples = unwords . fmap show . concatMap (\(x,y) -> [x,y])