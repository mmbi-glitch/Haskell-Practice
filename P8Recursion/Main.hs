module P8Recursion.Main where

main :: IO()

main = do
    putStr "Total Sales to Week 6: "
    print (totalSales 6)
    putStr "Max Sales to Week 6: "
    print (maxSales 6) 
    putStr "Factorial 5: "
    print (factWithError 5)
    -- putStr "Factorial -5: "
    -- print (factWithError (-5))
    putStr "fiboSeq 0: "
    print (fiboSeq 0)
    putStr "fib 5: "
    print (fib 5)
    putStr "fiboSeq 2: "
    print (fiboSeq 2)
    putStr "fiboSeq 5: "
    print (fiboSeq 5)
    putStr "fiboSeq 9: "
    print (fiboSeq 9)
    putStr "fiboSeq -2: "
    print (fiboSeq (-2))

    {-  

    example: consider the following data - weekly sales for a company...

    week 0 = 150 sales
    week 1 = 59 sales
    week 2 = 78 sales
    week 3 = 181 sales
    week 4 = 73 sales
    week 5 = 25 sales
    week 6 = 51 sales

    Given a week n
    - calculate total sales up to week n
    - find max sales up to week n

-}

-- first represent data

sales :: Int -> Int
sales 0 = 150
sales 1 = 59
sales 2 = 78
sales 3 = 181
sales 4 = 73
sales 5 = 25
sales 6 = 51
sales _ = 0

totalSales :: Int -> Int

totalSales n   
    | n == 0    = sales 0
    | otherwise = totalSales (n-1) + sales n

maxSales :: Int -> Int
 
maxSales 0  = sales 0 -- always important to identify base case
maxSales n  = max (sales n) (maxSales (n-1)) -- and recurisve case

{- 

    Trace of the function call for maxSales

    maxSales 4
    = max (sales 4) (maxSales 3)
    = max 73 (max (sales 3) (maxSales 2))
    = max 73 (max 181 (max (sales 2) (maxSales 1)))
    = max 73 (max 181 (max 78 (max (sales 1) (maxSales 0))))= max 73 (max 181 (max 78 (max 59 (sales 0))))
    = max 73 (max 181 (max 78 (max 59 150)))
    = max 73 (max 181 (max 78 150))
    = max 73 (max 181 150)
    = max 73 181
    = 181

-}

factWithError :: Int -> Int
factWithError 0 = 1
factWithError n 
    | n > 0      = n * factWithError (n-1) 
    -- any invalid input should also be defined and produce an error --
    | otherwise     = error "fact only defined for n > 0" 


-- fiboSeq follows the sequence : 0 1 1 2 3 5 8 13 21 34 ...

fiboSeq :: Int -> Int
fiboSeq 0 = 0
fiboSeq 1 = 1
fiboSeq n
    | n >= 0 =  fiboSeq(n-1) + fiboSeq(n-2)
    | otherwise = error "fiboSeq only defined for n >= 0"

-- fibq follows the sequence : 1 1 2 3 5 8 13 21 34 ...

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n
    | n > 1 = fib (n-1) + fib (n-2)
    | otherwise = error "fib only defined for n >= 0"