module Stepik51 where

import Data.Char
import Data.List

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = \x -> Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (xs ++ ys) c where
    Log xs b = f x
    Log ys c = g b

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

test1 = add1Log 3
test2 = mult2Log 3
test3 = execLoggers 3 add1Log mult2Log