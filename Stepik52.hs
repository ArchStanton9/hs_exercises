module Stepik52 where

import Data.Char
import Data.List
import Control.Monad

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = \x -> Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (xs ++ ys) c where
    Log xs b = f x
    Log ys c = g b

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

-- test1 = add1Log 3
-- test2 = mult2Log 3
-- test3 = execLoggers 3 add1Log mult2Log

returnLog :: a -> Log a
returnLog = Log []


bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log xs a) f = Log (ys ++ xs ++ ys) b
    where Log ys b = f a


-- test1 = Log ["nothing done yet"] 0 `bindLog` add1Log
-- test2 = Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log

instance Monad Log where 
    return = returnLog
    (>>=) = bindLog

-- instance Functor Log where
--     fmap = liftM
    
instance Functor Log where
    fmap f a = a >>= (\x -> return (f x))

instance Applicative Log where
    pure = return
    (<*>) = ap

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . return

test = execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]

testf = [(+1), (^2)]

test_law1 = (Log ["power 2"] 4) >>= add1Log >>= mult2Log
test_law2 = (Log ["power 2"] 4) >>= (\x -> add1Log x >>= mult2Log)