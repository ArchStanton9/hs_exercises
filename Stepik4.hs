module Stepik31 where

import Data.Function
import Data.Char
import Data.List

data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green" 
    show Blue = "Blue" 

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

emptyOrSingleton :: Bool -> a -> [a]
emptyOrSingleton False _ = []
emptyOrSingleton True x = [x]

isEqual :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
isEqual (a, b) (a', b') = a == a' && b == b'


data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Info = GT
cmp Error Warning = GT
cmp Info Warning = LT
cmp Info Error = GT
cmp Warning Error = LT
cmp Warning Info = GT
cmp _ _ = EQ


data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = distanceToOrigin (Point (x2-x1) (y2-y1))



data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle x y) = x * y

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b ) = a == b
isSquare _                = False


data Result = Fail | Success

doSomeWork :: Int -> (Result,Int)
doSomeWork n = (Success, 0)

processData :: Int -> String
processData a = case doSomeWork a of
    (Success, _) -> "Success"
    (Fail, n) -> "Fail: " ++ (show n)


data Result' = Fail' Int | Success'

instance Show Result' where
    show (Success') = "Success"
    show (Fail' n) = "Fail: " ++ show n

doSomeWork' :: Int -> Result'
doSomeWork' a = case doSomeWork a of
    (Success, _) -> Success'
    (Fail, n)    -> Fail' n


data Bit = Zero | One deriving Show
data Sign = Minus | Plus deriving Show
data Z = Z Sign [Bit] deriving Show

add :: Z -> Z -> Z
add a b = intToZ $ (zToInt a) + (zToInt b)

mul :: Z -> Z -> Z
mul = intToZ $ (zToInt a) * (zToInt b)

a = [One]
b = [One,Zero,One]

zToInt :: Z -> Integer
zToInt (Z Plus  [One] ) = 1
zToInt (Z Minus [One] ) = (-1)
zToInt (Z _     [Zero]) = 0
zToInt (Z sign  bs    ) = fst $ foldr f (0,0) (reverse bs) where
    f x (n,p) = (n + (zToInt (Z sign [x])) * 2 ^ p , p + 1)

intToZ :: Integer -> Z
intToZ n = Z (intToSign n) (intToBits n)

intToBits :: Integer -> [Bit]
intToBits n = unfoldr f n where
    f n | n < 0          = f (-n)
        | n == 0         = Nothing
        | n `mod` 2 == 1 = Just (One,  n `div` 2)
        | otherwise      = Just (Zero, n `div` 2)

intToSign :: Integer -> Sign
intToSign n
    | n < 0     = Minus
    | otherwise = Plus