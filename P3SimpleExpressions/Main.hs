module Main where

import Hello -- if u want to import all functions from a module, don't use brackets
import SimpleIO(printer) -- if u want specific funcs, declare in brackets

main :: IO()
main = do 
    Hello.printHello

    -- simple arithmetic

    putStrLn "\n--- Simple Arithmetic ---\n"
    
    putStr "5 * 2 = "
    print (5 * 2) -- multiplication
    putStr "5 ^ 2 = "
    print (5 ^ 2) -- power 
    putStr "(1-5)^(3*2-4) = "
    print ((1-5)^(3*2-4)) -- advanced power
    putStr "sqrt 4 = "
    print (sqrt 4) -- square root 
    putStr "succ 5 = "
    print (succ 5) -- successive number
    putStr "(truncate 6.59, round 6.59) = "
    print (truncate 6.59, round 6.59) -- multiple funcs in one print
    putStr "not (5<3) = "
    print (not (5 < 3))
    putStr "gcd 21 14 = "
    print (gcd 21 14)

    -- simple IO (note the <- symbol = assign a name to the result of an I/O action)

    -- output
    putStrLn "\n--- Simple IO ---\n"
    SimpleIO.printer

    let d = "hello"
    print d

    -- input 
    g <- getLine -- use getLine to read in a string
    print g

    putStrLn "What is 2 + 2?"
    x <- readLn -- use readLn to read in any type
    if x == 3
        then putStrLn "You're right!"
        else putStrLn "You're wrong!"


