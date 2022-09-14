module P24Monads.Main where

-- creates a new data type called Expr
-- with 2 constructors
-- the first one builds expressions from integers (takes Int as param)
-- the second one builds expressions from the division of two subexpressions (takes 2 Exprs as params)
data Expr = Val Int | Div Expr Expr

main :: IO()
main = do   let expr1 = Val 1
            let expr2 = Div (Val 6) (Val 0)
            print (eval expr1)
            print (eval expr2)

-- eval is a function that evaluates a Expr for us

-- the first iteration of eval was this (but this doesn't take care of division by 0)
-- eval :: Expr -> Int
-- eval (Val n) = n
    -- x and y are as complicated as you make them, so need a recursive definition
-- eval (Div x y) = (eval x) `div` (eval y)

-- so, we should use Maybe and cases to take care that division by 0 doesn't cause a crash
-- eval :: Expr -> Maybe Int
-- eval (Val n) = Just n
-- -- this is wordy but needed - maybe we can simplify this?
-- eval (Div x y) = case eval x of
--                     Nothing -> Nothing
--                     Just n -> case eval y of
--                                 Nothing -> Nothing
--                                 Just m -> safeDiv n m

-- Using the sequence operator >>= and lambda notation, we can make this simpler
-- could make it simpler by getting rid of 2nd lambda and using partial application
-- but not simple enough!
-- eval :: Expr -> Maybe Int
-- eval (Val n) = return n
-- eval (Div x y) =    eval x >>= (\n ->
--                     eval y >>= (\m ->
--                     safeDiv n m)) 

-- last iteration of eval : use the do notation!
eval :: Expr -> Maybe Int
eval (Val n) = return n -- this basically is the same as Just --> it converts a Data Type 'a' of any type to a Maybe value 'a'
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safeDiv n m


-- helper function for safe division
safeDiv :: Int -> Int -> Maybe Int
safeDiv x y
    | y == 0 = Nothing
    | otherwise = Just(x `div` y)

-- A monad is some kind of type constructor, like Maybe or List or something else,
-- together with two functions, return and >>= (do is syntactic sugar for this) 
-- that have these types
-- return :: a -> Maybe a
-- >>= :: Maybe a -> (a -> Maybe b) -> Maybe b
-- A monad gives you a way to have impure side effects, like I/O, 
-- in a pure programming lang like Haskell
-- I guess you can think of a monad as a function that has side effects?!

{-
a monad is three things together:
1. some data in a wrapper

one explanation I saw in a tutorial for one of the python libraries was, "a monad is just a tuple", and (while it's actually just 1/3 of a monad) this point is what they meant.  it doesn't have to be a tuple, but that's one way to represent it.  in the video, this was what he was talking about when mentioned type constructors, because types are how Haskell represents it.  one way seen it done in Python is with an object—the object wraps the data.  you might have, for example:

class Failure():
    self.value = 9
    self.failed = False

2. something to wrap the data up

in the python object example this would be the dunder init: it creates the instance and stores the data inside the wrapper.  this is the 'return' he was talking about; it's also called 'unit'.  you might have something like:

def \_\_init\_\_(self, value, failed=False):
    self.value = value
    self.failed = failed

3. something to apply a function that works on the value to the whole monad

this is bind, >>=.

in the Python object example, bind might look like:

def bind(self, f):
    if self.failed:
        return self
    else:
        try:
            x = f(self.data)
            return Failure(x)
        except:
            return Failure(None, True)

so:

Failure("9").bind(int).bind(partial(operator.mul, 2).value 
# "18", no problem

Failure("dog").bind(int).bind(partial(operator.mul, 2).value 
# None, but no problem from trying to multiply it by 2—it never got there

the point is that bind abstracts out the part where you have to unwrap the data before you can do something with it.  this is analogous to how map or reduce factor the looping out of various tasks.
-}

-- Good monad resource: https://www.youtube.com/watch?v=VgA4wCaxp-Q