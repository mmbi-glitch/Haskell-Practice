{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module P11Polymorphism.Main where

main::IO()

{-
Haskell has...

    • strong type system = binds specific data types to variables,
    gives type errors if types in an expression dont match

    • static type system = checks types of vars/expressions during compilation

    • type-inference system = if no explicit type is defined, Haskell will infer the type
      of the expression/function from the operations within it
-}

main = do
    
    putStrLn "\n** TYPE_INFERENCE **\n"
    let answer = 42 :: Int
    putStrLn ("add2 = " ++ show (add2 2 1)) -- we pass Ints in, but Haskell infers they should be Floats, so converts them implicitly
    -- putStrLn ("add2 = " ++ show (add2 2 answer)) -- if we explicitly pass an Int in, it gives error that Int does not match with Float
    putStrLn ("add2 = " ++ show (add2 2 (fromIntegral answer))) -- one way to fix is to use fromIntegral to convert Int to Num - but this is tedious
    putStrLn ("add2' = " ++ show (add2' 2 answer))
    
    putStrLn "\n** GENERAL POLYMORPHISM **\n"
    putStrLn ("lengthList [\"hello\", \"bye\"] = " ++ show (lengthList ["hello", "bye"]) )
    putStrLn ("lengthList [1, 2, 3, 4, 5] = " ++ show (lengthList [1,2,3,4,5]) )
    
    putStrLn "\n** TYPECLASS POLYMORPHISM - EQ **\n"
    putStrLn ("isEqual 1 2 = " ++ show (isEqual 2 3))
    putStrLn ("memberList 1 [2,3,1,4,5] = " ++ show (memberList 1 [2,3,1,4,5]))
    putStrLn ("memberList \"h\" [\"hello\", \"h\", \"2\"] = " ++ show (memberList "h" ["hello", "h", "2"]))
    putStrLn ("memberList \"h\" [\"hello\", \"2\"] = " ++ show (memberList "h" ["hello", "2"]))
    
    putStrLn "\n** TYPECLASS POLYMORPHISM - ORD **\n"
    putStrLn ("\"Abracadabra\" < \"Zebra\" = " ++ show ("Abracadabra" < "Zebra"))
    putStrLn ("\"Abracadabra\" `compare` \"Zebra\" = " ++ show ("Abracadabra" `compare` "Zebra"))
    putStrLn ("5 >= 2 = " ++ show (5 >= 2))
    putStrLn ("5 `compare` 2 = " ++ show (5 `compare` 2))
    putStrLn ("5 `min' 2 = " ++ show (5 `min` 2) ++ "; min 5 2 = " ++ show (min 5 2))
    
    putStrLn "\n** TYPECLASS POLYMORPHISM - ENUM **\n"
    putStrLn ("toEnum 1 :: Bool = " ++ show (toEnum 0::Bool))
    putStrLn ("toEnum 38 :: Char = " ++ show (toEnum 38::Char))
    putStrLn ("fromEnum '&' = " ++ show (fromEnum '&'))
    -- putStrLn ("enumFrom 1 = " ++ show (enumFrom 1)) 
    -- putStrLn ("enumFromThen 1 3 = " ++ show (enumFromThen 1 100000)) 
    putStrLn ("enumFromTo 1 5 = " ++ show (enumFromTo 1 5))
    putStrLn ("enumFromTo \'a\' \'v\' = " ++ show (enumFromTo 'a' 'v'))
    putStrLn ("enumFromThenTo 2 4 10 = " ++ show (enumFromThenTo 2 4 10))
    
    putStrLn "\n** USER TYPECLASSES **\n"
    putStrLn ("toString 'a' = " ++ show (toString 'a'))
    putStrLn ("toString 12 = " ++ show (toString (12 :: Int)))
    putStrLn ("size 1234565776 = " ++ show (size (1234565776 :: Int)))
    putStrLn ("size -12 = " ++ show (size (-12 :: Int)))
    printVisible (2::Int)
    printVisible 'a'
    putStrLn ("calculateSpace 1 = " ++ show (calculateSpace (1::Int)))

add2 :: Float -> Float -> Float
add2 a b = a + b

{- 
• if we rewrite the same function without a type signature
• Haskell infers a polymorphic function that accepts ANY number type   
• the type of arguments and return value are all an unspecified type 'a'
• the notation 'Num a =>' specifies a constraint that type 'a' must be in the 'Num' type class
-}

add2' :: Num a => a -> a -> a
add2' a b = a + b

{- 
• Can do user-defined polymorphic functions with this syntax
• Polymorphism = many forms
• Polymorphic function = can functon on many types
• NOTE: different to polymorphism in OO langs
  where its used in context of object inheritance structs
• NOTE: also different from function overloading
-}

-- list functions are often polymorphic
lengthList :: [a] -> Int
lengthList [] = 0
lengthList (_:t) = 1 + lengthList t

{- 
• not all functions are that general, usually some min type requirement
• for that, we can use type classes 
• type classes defines a required set of functions 
  that every instance of that type class will provide
-}

-- SOME BASIC TYPECLASSES --


{- 
*Eq* is used for types that support equality testing. 
The functions its members implement are == and /=. 
All the types we mentioned previously except for functions 
are part of Eq, so they can be tested for equality.
-}

{- 
*Ord* is used for types that have an ordering. 
All the types we covered so far except for functions are 
part of Ord. Ord covers all standard comparing functions,
e.g. >, <. >=, and <=. The compare function takes two Ord members
of same type and returns an ordering, namely a type that can be LT (<), GT (>), or EQ (==). 
The max and min functions take two Ord members and find the max or min one, respectively
To be a member of Ord, a type must first be in the *Eq* typeclass.
-}

{-
*Enum* provides operations on sequentially ordered elements of the type.
The toEnum function converts an Int into a specified type if its comparable, e.g. Char, 
or gives an error if not comparable. fromEnum converts a comparable type into an Int.
enumFrom takes a generic elem and returns an infinite list starting from that element, eg. [1..].
enumFromThen takes 2 generic elems and returns an infinite list, starting from 1st arg and with a step of 2nd arg 
enumFromTo takes 2 generic elems and returns a list starting from 1st arg and ending in 2nd
enumFromThenTo same as enumFromTo but allows a step arg
-}

{-
*Read* converts values from string with the read function, 
while *Show* does the opposite with the show function.
-}

isEqual :: Eq a => a -> a -> Bool
isEqual a b = a == b

memberList :: Eq a => a -> [a] -> Bool
memberList e [] = False
memberList e (h:t)
  | e == h      = True
  | otherwise   = memberList e t

-- Ord typeclass necessary for sorting list or determining if a list is sorted 
-- Obviously, types that have no ordering can't be sorted
sortedList :: Ord a => [a] -> Bool
sortedList [] = True
sortedList [h] = True
sortedList (h : t)
  | h <= head t   = sortedList t
  | otherwise       = False

-- a type class can be declared using the following syntax --

class Visible a where -- a typeclass with the name Visible and a signature
  -- any instance of Visible must provide these functions in the signature
  toString :: a -> String
  size :: a -> Int


-- a type can be made an instance of a type class with the following syntax

instance Visible Char where -- Char is an instance of Visible
-- because it provides implementations of the necessary functions
  toString ch = [ch]
  size _      = 1

instance Visible Int where
  toString int = show int
  size int
    | int == 0 = 0
    | int == -1 = 0
    | otherwise = 1 + size (int `div` 10)

-- doesn't work for NUMS X__X
instance Visible a => Visible [a] where
  toString [x] = toString x
  toString (h:t) = toString h ++ toString t
  size [x] = size x
  size (h:t) = size h + size t

-- now, we can write our own functions that depend on the Visible typeclass

printVisible :: Visible a => a -> IO()
printVisible v = putStr ("printVisible " ++ toString v ++ " = " ++ show (toString v) ++ "\n")

calculateSpace :: Visible a => a -> Int
calculateSpace = size