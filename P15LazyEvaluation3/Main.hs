{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module P15LazyEvaluation3.Main where

main :: IO()

main = do   print "Hello"
            print ("Add two heads of two lists = " ++ show (f [1..3] [1..3]))

{-
    
Proper Lazy Evaluation = Don't Ask Haskell to Evaluate Infinite List

    1.  Important to make sure functions don't ever result in evaluation of an 
        infinite list
    2.  If that happens, it's not always clear if the program is stuck  
        trying to construct an infinite list.
    3.  It's important to understand how lazy evaluation really works
        with regards to pattern matching, guards, and so on
-}

-- Given function with multiple patterns, args must be evaluated to determine which pattern to use
-- trick here is that args will NOT be fully evaluated, only to see if they match the patterns or not

f :: [Int] -> [Int] -> Int
f [] [1]         = 0 
f (h:t) []          = 0
f (h1:t1) (h2:t2)   = h1 + h2

{-
-- e.g. 

f [1..3] [1..3]             (not enough info for 1st)
= f (1:[2..3]) [1..3]       (can't match 1st pattern)
= f (1:[2..3]) [1..3]       (not enough info for 2nd)
= f (1:[2..3]) (1:[2..3])   (doesn't match 2nd pattern)
= 1 + 1                     (matches 3rd pattern)
-}

{- 

Similar process with guards and if-then-else...

    1. conditions/guards are tested in order they are defined 
    2. args are evaluated to point where condition/guard can be evaluated to T or F 

-}

{-
With operators, the exact evaluation process depends on the particular operator.

    1. and operator (&&) will avoid evaluating 2nd arg if 1st is false

        True && x = x       <= need to evaluate x
        False && x = False  <= no need to evaluate x 

    2. addition oeprator (+) needs to evaluate both args to return result

    3. equality operator for lists don't need to evaluate entire list
       to return a result of False for the input [] and (h:t).
       
*Generally, operators avoid evaluation wherever possible*
-}

{-
With expressions involving multiple functions, they are evaluated from the "outside in" and from "left to right"
    
    f1 e1 (f2 e2 17)
    f1 e1 + f2 e2

    In above examples, f1 is always evaluated first
-}

-- Write a function to compare 2 lists and determine if they are equal
listEqual :: Eq a => [a] -> [a] -> Bool
listEqual [] []         = True
listEqual (h1:t1) []    = False
listEqual [] (h2:t2)    = False
listEqual (h1:t1) (h2:t2) 
    | h1 == h2          = listEqual t1 t2
    | otherwise         = False
    
{-
What will be result?
>   listEqual [1..]   ([1..3] ++ [5..])           (insufficient info)
=   listEqual 1:[2..] 1:([2..3] ++ [5..])       (4th pattern, 1st guard)
... continues until 1st list in 2nd arg is empty
=   listEqual 3:[4..] 3:([] ++ [5..])           (4th pattern, 1st guard)
=   listEqual [4..]     ([] ++ [5..])           (insufficient info)
=   listEqual 4:[5..]   ([5..])                 (insufficient info)
=   listEqual 4:[5..]   5:[6..]                 (4th pattern, 2nd guard)
=   False
-}