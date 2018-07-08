module Stepik1 where

import Data.Char
import Data.Function

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y 
    then 10 * digitToInt x + digitToInt y
    else 100
    
dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p1 - fst p2 ) ^ 2 + (snd p1 - snd p2 ) ^ 2

test = putStrLn "Test!"


doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2) 


seqA :: Integer -> Integer
seqA n = let
    helper (r1, r2, r3) n 
        | n == 0 = r2
        | otherwise = helper (r2, r3, r3 + r2 - 2 * r1) (n - 1)
    in helper (0, 1, 2) n


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (acc (abs x, 0) (len x), len x) where 
    len s = toInteger $ (length . show) (abs s)
    d10 x = x `div` 10
    m10 x = x `mod` 10
    unhook n = (toInteger $ d10 (n - m10 n), m10 n)
    acc (p1, p2) 0 = p2
    acc (p1, p2) n = acc ((fst $ unhook p1), p2 + (snd $ unhook p1)) (n-1)



{- integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = acc f (a, step, 0) 1000 where 
    step x0 xn = (xn - x0) `div` 1000
    tarea (x0, x1) h = (x0 + x1) `div` 2 * h 
    acc fx (x0, x1, s) 0 = s
    acc fx (x0, x1, s) n = (x0 + step, x1 + step, s + tarea (fx x0, fx x1) step) (n - 1)
    -}

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let 
    step = (b - a) / 1000
    tarea (x0, x1) h = (f x0 + f x1) / 2 * h 
    start = min a b
    acc (x0, x1, s) 0 = s
    acc (x0, x1, s) n = acc (x0 + step, x1 + step, s + tarea (x0, x1) step) (n - 1)
    in signum (b - a) *(acc (start, start + step, 0) 1000)

integration2 f a b = h * (0.5 * (f a + f b) + g (n-1) 0) where
    n = 1000
    h = (abs b - abs a) / n
    g 0 s = s
    g n s = g (n-1) (s + f (b - h * n))
