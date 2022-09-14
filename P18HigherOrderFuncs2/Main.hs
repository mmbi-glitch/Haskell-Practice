-- Function Composition in Haskell

{- 
This lecture, we'll look at how functions can also be returned
as the result of other functions, which allows us to define functions 
as values within the program
-}

module P18HigherOrderFuncs2.Main where

main :: IO()

main = do   putStrLn ("add 1 twice to 2 = " ++ show (twice add1 2))
            putStrLn ("add 1 4 times to 2 = " ++ show (applyNTimes add1 2 4))

{- 
Functions returned as values can then be
passed as arguments to other HO functions.
This can be achieved by the means of partial 
application and operator sections

1 reason functional langs are called 'functional':
we can treat functions as data, which
allows to give a function-level definition of a function
basically, can define a function in terms of other functions
instead of how it operates on parameters

example of a function-level def as follows
function pow4 defined in terms of another function pow2
-}

pow2 :: Int -> Int
pow2 x = x*x

pow4 :: Int -> Int
pow4 x = (pow2 . pow2) x -- could reduce it to pow4 = pow2 . pow2

-- the function level def above makes use of the function composition op '.'
-- which defines application of one function to another.

{-  

Consider following expression: f g x 
where f = func 1, g = func 2, x = value/input

Two ways to interpret it:

1. Imperative way: f (g x)  

function g is evaluated for input x, resulting return val
is calced and then passed as input to function f, which is then
calced as well.

2. Functional way: (f . g) x 

function f is applied to the function g; result of this itself
is another function that is then applied to input x. 

-}

{-

OFC, not every pair of funcs can be composed.
Since the output of func g becomes input of f, the types must match.
Resulting function has the INPUT type of g and the OUTPUT type of f.

Function composition defined below:

           f           g        (f.g)
(.) :: (b -> c) -> (a -> b) -> (a -> c)

The function composition op is associative (brackets don't matter).
The following 2 exprs are equal:

    f . (g . h) = (f . g) . h

The functions are applied in the order h, followed by g, followed by f,
written this way...

    f . g . h = f . (g . h)

-}

{-
    2 COMMON MISTAKES WITH FUNCTION COMPOSITION

1. Binding power of a function application vs a function composition.
        
        Operators are applied in the order of binding power and 
        function applications bind more tightly than any other operator,
        including function composition. For example, f . g x simplifies
        to f . (g x) because function application binds first.
        Now if the result of (g x) is a value, not a function, 
        then it doesn't match the input type of the composition operator
        and causes an error.

2. Function application and composition are easily confused.

        Function composition combines 2 functions whereas an application
        combines a function and an arg (which may be a function)

        f . x = f is composed with the function x

        f x = f is applied to x, so x must be an arg

-}

-- adds 1 to a number

add1 :: Int -> Int
add1 x = x + 1

-- example of function composition...
-- we can def a func 'twice', which applies a function to an arg twice

twice :: (a -> a) -> (a -> a)
twice f = f . f

{-
Stack trace of twice add1 2

twice add1 x
-> (add1 x . add1 x)
-> ((x + 1) + 1)

twice add1 2
-> ((2 + 1) + 1)
-> ((3) + 1)
-> 4
-}

-- we can extend this to apply a function 'n' times

applyNTimes :: (a -> a) -> Int -> (a -> a)
applyNTimes f n
    | n == 1 = f
    | n > 1 = f . applyNTimes f (n - 1)
    | otherwise = error "n must be > 1"
