module Stepik2 where

import Data.Function

--multSecond = g `on` h
--g = (*)
--h = snd


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

sum3squares = (\x y z -> x+y+z) `on3` (^2)


--doItYourself = f . g . h

--f = logBase 2
--g = (^ 3)
--h = max 42


templ :: a -> (a,b) -> a -> (b,a,a)
templ a1 p a2 = (snd p, a1, a2)
-- templ a1 p a2 = (snd p, a2, a1)
-- templ a1 p a2 = (snd p, fst p, a2)
-- templ a1 p a2 = (snd p, fst p, a1)
-- templ a1 p a2 = (snd p, a1, fst p)
-- templ a1 p a2 = (snd p, a2, fst p)

-- uncurry (flip const)
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- flip :: (a -> b -> c) -> b -> a -> c
-- const :: a -> b -> a
-- 
-- flip' :: (a -> b -> a) -> b -> a -> a
-- uncur :: (b -> a -> a) -> (b , a) -> a
--

-- curry uncurry flip (,) const
-- (a, b) -> (b, a)

-- a -> b -> (b, a)

swap p = f (g h) p
f = uncurry
g = flip
h = (,)

{-

class Printable p where
    toString :: p -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString _ = "unit type"


instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

-}

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool
    
class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool
    
class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a 
        | doesEnrageMork a = stomp a
        | doesEnrageGork a = stab a
        | doesEnrageGork a && doesEnrageMork a = stomp (stab a)
        | otherwise = a

{-    
        stompOrStab = morkAct . gorkAct where 
        morkAct a
            | doesEnrageMork a = stomp a
            | otherwise = a
        gorkAct a
            | doesEnrageGork a = stab a
            | otherwise = a
-}

ip = show a ++ show b ++ show c ++ show d

a = 127.2
b = 24.1
c = 20.1
d = 2

class (Bounded a, Enum a, Eq a) => SafeEnum a where
    ssucc :: a -> a
    ssucc a
        | a == maxBound = minBound
        | otherwise = succ a
    
    spred :: a -> a
    spred a
        | a == minBound = maxBound
        | otherwise = pred a

instance SafeEnum Bool

--instance SafeEnum Num
