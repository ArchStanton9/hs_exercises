module Stepik54 where

import Data.Char
import Data.List
import Control.Monad

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)


-- isNumber' :: String -> Bool
-- isNumber' = all isDigit

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken s 
    | all isDigit s = Just (Number (read s))
    | otherwise = Nothing


tokenize :: String -> Maybe [Token]
tokenize input = foldr parse (Just []) (map asToken (words input)) where
    parse (Just x) (Just xs) = Just (x:xs)
    parse _       Nothing    = Nothing
    parse Nothing       _    = Nothing


data Board = Black | White deriving (Show, Eq)

nextPositions :: Board -> [Board]
nextPositions White = [White, Black]
nextPositions Black = [Black, White]


nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred = do
    if n == 0 then (if pred b then [b] else []) else do
        x <- nextPositions b;
        if n < 0 then [] else
            if n == 1 then (if pred x then return x else []) else
                nextPositionsN x (n - 1) pred
    

-- test n = nextPositionsN White n (\x -> True)

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple n = do 
    x <- [1..n]
    y <- [1..n]
    z <- [1..n]
    True <- [x^2 + y^2 == z^2]
    True <- [x <= y]
    return (x, y, z)


test1 = pythagoreanTriple 5
test2 = pythagoreanTriple 0
test3 = pythagoreanTriple 10

compr n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2 && x <= y]
