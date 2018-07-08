module Stepik31 where

import Data.Function
import Data.Char
import Data.List

coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change a = [x | x <- (tuples stab) ++ stab, sum x == a] where
    stab = [[s] | s <- coins]
    mix xs ys = [x:y | x <- xs, y <- ys]
    tuples acc
        | all (\x -> sum x > a) acc = acc
        | otherwise = mix coins acc ++ tuples (mix coins acc)

-- change :: (Ord a, Num a) => a -> [[a]]
-- change a = [x | x <- tuples [[s] | s <- coins], sum x == a] where
--     mix xs ys = [x:y | x <- xs, y <- ys]
--     tuples acc
--         | all (\x -> sum x > a) acc = acc
--         | otherwise = mix coins acc ++ tuples (mix coins acc)


concatList :: [[a]] -> [a]
concatList = foldr (++) []

lengthList :: [a] -> Int
lengthList = foldr (\x s -> s + 1) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if x `mod` 2 == 1 then x + s else s) 0

meanList :: [Double] -> Double
meanList = uncurry (/) . foldr (\x (s,c) -> (s + x, c + 1)) (0,0)

evenOnly :: [a] -> [a]
evenOnly xs = foldr (\(a,n) x -> if even n then a : x else x ) [] (zip xs [1..])

lastElem :: [a] -> a
lastElem = foldl1 seq

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g (c1, c2) = if fromEnum c1 <= fromEnum c2 then  Just (c2, (c1, pred c2)) else Nothing

-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

-- revRange = unfoldr g where
--     g@(c1, c2) = if pred c2 == c1
--         then Nothing
--         else Just (c1, (c1, pred c2))

test = revRange ('a','z')