-- Haskell program explaining I/O

module P12InputOutput.Main where

import System.IO

import Data.Char(toLower)

{- Haskell functions are pure functions, while IO functions are not
   
    IO functions have side-effects, return different values, must be evalauted in order
   
    e.g. a function that reads an integer does not return the same value each time

    Haskell uses a monadic approach to IO functions

    One way to think about a IO function is that it takes 'state of world'
    as a param and returns a modded version of this 'world'

    type IO() = Haskell notation for a function that performs OUTput (returns modded version of 'world')

    type IO a = Haskell notation for a function that performs INput (returns a value AND modded version of 'world') -}

-- e.g. main is an IO() function
main = do putStr "Hello\n"
        --   put3Times "Welcome to Haskell x3\n"
        --   put3Times' "Hello Without Sugar :D x3\n"
        --   putNtimes "This is 'do' recursion x4\n" 4
        --   echoLines
        --   echoLines'
          readPalindrome
          int <- getInt -- To get the Int, you need to perform the IO action, which you can do with the <- operator in do notation.
          print int
          printIntSum 
          


-- easiest way to use IO() functions is using 'do' notation (syntactic sugar for chaining several IO actions)

-- this function outputs the same string 3 times (do notation)
put3Times :: String -> IO()
put3Times str = do putStr str
                   putStr str
                   putStr str

-- this function outputs the same string 3 times (with the >> binding op)
put3Times' :: String -> IO()
put3Times' str = putStr str
                   >> putStr str
                        >> putStr str

-- binding op >> combines 2 IO actions in sequence by passing the 'world' between them
-- (>>) :: IO a -> IO b -> IO b
-- (action1 >> action2) world0 = (b, world2)
-- where (a, world1) = action1 world0
-- (b, world2) = action2 world1


-- do notation can be used with recursion, like so
putNtimes :: String -> Int -> IO()
putNtimes str 1 = putStr str
putNtimes str n = do putStr str
                     putNtimes str (n-1)

-- do notation can also be used to perform input
-- this entails 'naming' the result of the input actions and passing the names on
-- like so

echoLines :: IO()
echoLines = do hSetBuffering stdout NoBuffering
               putStr "Enter your name: "
               line <- getLine
               putStr ("Pleased to meet you, " ++ line ++ "!\n")

-- like previous example, this is just syntactic sugar for the binding op >>=
echoLines' :: IO()
echoLines' = hSetBuffering stdout NoBuffering 
             >> putStr "Enter your name: " 
             >> getLine >>= \line 
             -> putStr ("Hello, " ++ line ++ "!\n")

-- example: write an IO function that reads a line of input and tests
-- whether it is a palindrome

testPalindrome :: String -> Bool
testPalindrome list = remove list ' ' == reverse (remove list ' ')

remove :: String -> Char -> String
remove [] _ = []
remove (h:t) char
    | h == char = remove t char
    | otherwise = h:remove t char

testPalindromeInt :: [Int] -> Bool
testPalindromeInt list = list == reverse list

readPalindrome :: IO()
readPalindrome = do putStr "Enter a word/sentence: "
                    hFlush stdout
                    line <- getLine
                    putStr ("Is this word/sentence a palindrome: " ++ show (testPalindrome line) ++ "!\n")

readPalindromeInt :: IO()
readPalindromeInt = do putStr "Enter a list of ints: "
                       hFlush stdout
                       line <- getLine
                       let int = read line :: Int
                       putStr ("Is this list of ints a palindrome: " ++ show (testPalindromeInt (read line :: [Int])) ++ "!\n")



-- When we have input/output functions that use 
-- repetition or produce an IO value as a result, we 
-- need some way of representing this.
-- The return construct allows us to return a value 
-- as an IO value.
-- return :: a -> IO a
-- using the return() construct to return from a function

echo :: IO ()
echo = do   line <- getLine
            if line == "" then
                return ()
            else
                do putStr (line ++ "\n")
                   echo

-- return can also be used to return vals from input function
-- like so

getInt = do     putStr "Enter a number (0 to quit): "
                hFlush stdout
                line <- getLine
                return (read line :: Int)



-- using Input function to repeatedly read integers until user inputs 0
readIntList :: IO [Int]
readIntList = do    h <- getInt
                    if h == 0 then
                        return []
                    else 
                        do t <- readIntList
                           return (h:t) 

printIntSum = do    list <- readIntList
                    putStrLn ("The actual list is = " ++ show list)
                    putStrLn ("The summed list = " ++ show (sum list)) 