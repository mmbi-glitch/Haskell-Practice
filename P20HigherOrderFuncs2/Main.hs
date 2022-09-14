-- Function Args and Currying in Haskell

module P20HigherOrderFuncs2.Main where

main :: IO()

main = do   putStrLn ("multiply 2 by 5 in curried form = " ++ show (curryMultiply 2 5))
            putStrLn ("multiply 2 by 5 in UNcurried form = " ++ show (uncurryMultiply (2, 5)))
            putStrLn ("get 100 from atm (uncurried) = " ++ show (atm ("Smile", 76847435, 100.0)))
            putStrLn ("get 100 from atm (curried) = " ++ show (cAtm "HSBC" 19072655 101.05))
            putStrLn ("sort in ascending order = " ++ show (qsortG (<) [1,6,3,9,7,2,4,8,5]))
            putStrLn ("sort in descending order = " ++ show (qsortG (>) [1,6,3,9,7,2,4,8,5]))

{-
As we saw in partial applications, it is not always clear how many args a 
function takes. E.g. multiply appears to take 2 args but when applied to one 
argument, it returned another function.
-}

-- full multiply function takes 2 args
multiply :: Int -> Int -> Int
multiply x y = x * y

{- 
partial application of multiply, the implementation of which is double, 
takes only one arg! why is it like this? Well, think about it.
We already provide one arg to double, so it knows what x is.
and then we have the partial application, which already has another arg
given to it - y = 2; and that's how it works. Break into multiple partial functions! :D
-}

double :: Int -> Int
double = multiply 2

{-
    FACT: all functions in Haskell take ONE argument.
    Functions are written as though they take multiple arguments.
    But it's actually just ONE.
-}

{-
    CURRYING

When a function (like multiply) is applied to multiple arguments, 
it's actually applied to only arg. If the result of this application
is another function, then it is this function that is applied to the next argument.

For example, with multiply, it actually is...

multiply :: Int -> (Int -> Int)

say you provide only one input x, 2, to multiply.

multiply 2 then returns a function because there's only part of the puzzle. 

multiply 2 = 2 * y <- It needs y

can express it terms of a nameless function, or lambda calculus

multiply x = \y = x * y

So it's actually... (multiply 2) y

So, thus, we provide another arg in double. Thus, multiply 2 returns a function that is then applied 
to the next argument.

double 2 = multiply 2 2 = 2 * 2 = 4

This representation of functions is called a curried form (after Haskell Curry, after whom the lang is named).

A function that takes 2 or more args can be curried by transforming it
into functions that take one arg at a time.

E.g. x = f(a,b,c) becomes:
        h = g(a)
        i = h(b)
        x = i(c)

Or, with the multiply function, we have
    r = multiply(x, y) = multiply(2, 2) becomes:
        double = multiply(x) = multiply 2 = 2 * y <- returns a function 
        r = double(y) = double(5) = 2 * 5 = 10 <- returns a value
-}

-- a function that multiplies 2 ints together would normally 
-- be written as follows - this is the curried form.

curryMultiply :: Int -> Int -> Int
curryMultiply x y = x*y

-- the uncurried version of this function bundles the arguments together

uncurryMultiply :: (Int, Int) -> Int
uncurryMultiply (x,y) = x*y

{-

Benefits of Currying

1. notation is simpler
2. multiple args don't need to be given to the function as a bundled tuple
3. allows partial application => powerful way to express functions in terms of each other 

Haskell provides functions that convert functions between curried and uncurried forms
An implementation of this is given below.

Good resource: https://www.youtube.com/watch?v=psmu_VAuiag - Currying ComputerPhile
-}

curry :: ((a, b) -> c) -> a -> b -> c
curry g x y = g (x, y) -- basically, says convert this to curried form but then return it as uncurried

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y -- basically, says convert this to uncurried form but then return it as uncurried

-- EXAMPLES OF CURRYING

-- umm, an atm function?

-- what's wrong with the function below?
-- it inputs everything at once! 
-- that's now how an atm works in real life
-- in real life, an atm machine is a curried function
-- it first takes the card, then the pin, and finally the amount
-- not everything together
atm :: (String, Int, Double) -> Double
atm (card, pin, amount)
  | card == "Natwest" = amount - amount*0.05
  | card == "HSBC" = amount - amount*0.01
  | otherwise = amount

-- curried form of the atm function
cAtm :: String -> Int -> Double -> Double
cAtm card pin amount
  | card == "Natwest" = amount - amount*0.05
  | card == "HSBC" = amount - amount*0.01
  | otherwise = amount


-- below is a quicksort function, sorts a list in typeclass Ord
-- into ascending order

qsort :: Ord a => [a] -> [a] 
{-# SPECIALISE qsort :: Ord a => [a] -> [a] #-}
qsort [] = []
qsort [x] = [x]
qsort list = qsort [x | x <- list, x < h] ++ [x | x <- list, x == h] ++qsort [x | x <- list, x > h]
    where h = head list

-- how would we make it a general sort function?
-- easy, make it take in a comparator function!

qsortG :: (a-> a -> Bool) -> [a] -> [a]
qsortG f [] = []
qsortG f [x] = [x]
qsortG f list = qsortG f [x | x <- list, f x h] ++ [x | x <- list, not (f x h) && not (f h x)] ++ qsortG f [x | x <- list, f h x]
    where h = head list