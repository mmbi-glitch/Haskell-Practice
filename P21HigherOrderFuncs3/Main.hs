-- Advanced Functionality of Higher Order Functions in Haskell

module P21HigherOrderFuncs3.Main where

main :: IO()

main = do   print "Hello"
            -- Approach #1: use the powSelf function
            putStrLn ("raise all numbers in list to the power of itself: " ++ show (map powSelf [1..7]))
            -- Approach #2: use a lamda expression
            putStrLn ("raise all numbers in list to the power of itself: " ++ show (map (\x -> x^x) [1..7]))
            -- Lambda expressions can also have multiple params, like any other function
            putStrLn ("Sqrt Sqrt 2: " ++ show ((\f x -> f (f x)) sqrt 2))

-- HO functions allow functions to be treated as values and either 
-- passed to or returned from other functions
-- Simple expressions can be used with HO functions but 
-- what about more complex functionality?

-- E.g. easy to use map to square all elements in a list,
-- but what about the element to the power of itself

-- (First approach ^^ check in main)
-- We could just use a function that takes a single number 
-- and calculates the number to the power of itself.

powSelf x = x ^ x

-- Downside of this approach is its a single use function
-- what if we want powpowSelf? then, we need to create another 'single use' function

-- Second approach?
-- Use lambda expressions 
-- Instead of naming and defining an entire function, 
-- lambda expressions allow a function to be defined and used immediately.
-- lambda expressions are based on lambda-calculus function abstractions.

{-
        LAMBDA CALCULUS λ

Haskell, like many functional langs, is based on lambda calculus.
In lambda calculus, expressions built from lambda expressions.
A lambda expression is a function that introduces an abstraction
or a function application that specializes an abstraction.

bound variables in lambda calculus = params in functions

Following is the identity function: λx.x

We have a single bound variable, x.
And then a body expression, which is just x. 
When applied to an arg, the bound variable x will 
be substituted with the arg.

Following is the self-application function: λs.(s s)

We have a single bound variable s. 
The body expression is an application of s to s.
This will be useful for writing a self-replicating/recursive lambda function.

We can apply these functions to each other to specialize the abstraction.
The following is the application of the identity function to the self-application function...

(λx.x (λs.(s s))) -- we have the identity function, which takes in the self-application function as the parameter
(λs.(s s) -- the result is of course, the self application function itself

Applying the other way round...

(λs.(s s) λx.x) -- here, the self application function takes in the identity function as a param
(λx.x λx.x) -- this simplifies to the identity function applied to itself
λx.x -- which finally simplies to the identity function itself

How do we represent all this in Haskell?
In Haskell, a lambda expression can be used to anonymously define and use 
a function. The '\' is used to represent the 'λ' and the '->' to represent the '.' 
-}

-- Second approach: use lambda calculus
-- The function to raise each element of a list to its own power
-- would be written as map (\x -> x^x) [1..7]
-- as you can see, the function is defined and used, but does not use any namespace
-- throughout the rest of the module - great for single use functions!

