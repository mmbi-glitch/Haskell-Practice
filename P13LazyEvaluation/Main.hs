-- Haskell program that explains lazy evaluation --

{- 

2 main types of evaluating functions with params:

    1. eager evaluation / applicative order reduction:
        • used in imperative languages
        • params are evaluated first and then the values are passed to the function
        • params are ALWAYS evaluated (regardless of function using them or not)
        • expressions are strictly applied in order they are defined 
        • languages that use eager evaluation have strict semantics

    2. lazy evaluation / normal order reduction
        • used in lots of languages (Haskell, Python)
        • params are only evaluated when their value is actually needed
        • possible because order of evaluation does not change result of each expression 
        • languages that use lazy evaluation have non-strict semantics
-}

module P13LazyEvaluation.Main where

{-

Main idea of lazy evaluation = put off evaluating an argument to a function
until that argument's value is actually needed. Thus, only values that are 
actually required will be evaluated.

Example of Eager vs Lazy evaluation:

consider the following function:

f x y = x + y

-- with eager evaluation, the params are evaluated first --

> f (9 - 3) (f 34 3)
= f (6) (34 + 3)
= f (6) (37)
= (6 + 37)
= 43

-- with lazy evaluation, the expression is substituted in for the argument
-- and only evaluated when needed

> f (9 - 3) (f 34 3)
= (9 - 3) + (f 34 3) 
= (9 - 3) + (34 + 3) <- now it MUST be evaluated to continue
= 6 + 37
= 43

Conclusion:

1. Lazy evaluation tries to apply the outermost function application first and works its way inward
2. Eager evaluation does the opposite - works innermost to outermost

-}

main :: IO()
main = do   print "Hello"

-- lazy evaluation allows computation to be avoided 
-- let's see how it works with the following 'select' function

select :: Integral a => a -> a -> a -> a
select n x y
    | n > 0     = x
    | otherwise = y

{-

-- eager evaluation, evaluates params first --

> select (5-3) (fib 11) (fib 1000)
= select 2 89 268638100244853...
= 89

-- lazy evaluation, evaluate params when needed

> select (5 - 3) (fib 11) (fib 1000)
= select 2 (fib 11) (fib 1000)
= fib 11
= 89 

Note: it may seem lazy evaluation could result in duplicated computation, but
      but this is NOT the case. A duplicated arg will never be evaluated more 
      than once.  

-}

-- lazy evaluation allows us to understand the real value of functions like the following:

multiply :: (Eq p, Num p) => p -> p -> p
multiply x 0 = 0
multiply 0 y = 0
multiply x y = x*y

-- ^with lazy evaluation, we can avoid evaluating the whole argument; 
-- e.g. if 2nd arg is 0, then no need to see what's x, just return 0

