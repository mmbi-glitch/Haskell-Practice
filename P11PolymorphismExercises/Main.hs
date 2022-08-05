module P11PolymorphismExercises.Main where

main :: IO()

main = do
    putStrLn "\n** EQUALS3 FUNCTION **\n"
    putStrLn ("equals3 hello hell hel = " ++ show (equals3 "hello" "hell" "hel"))
    putStrLn ("equals3 hello hello hel = " ++ show (equals3 "hello" "hello" "hel"))
    putStrLn ("equals3 hello hello hello = " ++ show (equals3 "hello" "hello" "hello"))
    putStrLn ("equals3 1 2 3 = " ++ show (equals3 1 2 3))
    putStrLn ("equals3 1 1 3 = " ++ show (equals3 1 1 3))
    putStrLn ("equals3 1 1 1 = " ++ show (equals3 1 1 1))


-- polymorphic equality function

equals3 :: Eq a => a -> a -> a -> Bool
equals3 a b c
    | a == b && a == c    = True
    | otherwise           = False

unzipListTuples :: [(a,b)] -> ([a],[b])
unzipListTuples [] = ([], [])
unzipListTuples list = (firstAll list, secondAll list)

firstAll :: [(a,b)] -> [a]
firstAll [] = []
firstAll ((h,_):t) = h:firstAll t

secondAll :: [(a,b)] -> [b]
secondAll [] = []
secondAll ((_,h):t) = h:secondAll t 

unzip2 :: [(a,b)] -> ([a],[b])
unzip2 [] = ([], [])
unzip2 ((h1,h2):t) = (h1:t1, h2:t2)
    where (t1,t2) = unzip2 t
