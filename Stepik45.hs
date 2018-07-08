module Stepik45 where

import Data.Monoid



data List a = Nil | Cons a (List a) deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x y) = x : (fromList y)

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)


data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero    = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero    b = b
add (Suc a) b = Suc (add a b)

mul :: Nat -> Nat -> Nat
mul Zero       _ = Zero
mul (Suc Zero) b = b
mul (Suc a)    b = b `add` (mul a b)


fac :: Nat -> Nat
fac Zero       = Suc Zero
fac (Suc Zero) = Suc Zero
fac (Suc a)    = (Suc a) `mul` (fac a)

s1 = Suc Zero
s2 = Suc (Suc Zero)
s3 = Suc (Suc (Suc Zero))


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

height :: Tree a -> Int
height (Leaf a) = 0
height (Node x y) = 1 + max (height x) (height y)

size :: Tree a -> Int
size (Leaf a) = 1
size (Node x y) = 1 + (size x) + (size y)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf a) = (1, a)
    go (Node x y) = sumT (go x) (go y)
    sumT (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

t1 = (Node (Leaf 4) (Leaf 5)) :: Tree Int
t2 = Node (Leaf 1) (Leaf 2)

t3 = Node t1 t2
t4 = Node t3 (Leaf 11)

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ex = expand' (Val 0) ex where
    expand' x y 
        | x == y = y
        | otherwise = expand' y (expand'' y)
    expand'' ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
    expand'' (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
    expand'' (e1 :+: e2) = expand e1 :+: expand e2
    expand'' (e1 :*: e2) = expand e1 :*: expand e2
    expand'' e = e
    




testExpt = (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)

-- type Endo a = a -> a

-- func :: Endo (Endo Int) -> Int
-- func x = x (1+) 2
-- test = func (\x y -> x y)


newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Semigroup Xor where

instance Monoid Xor where
      mempty = (Xor False)
      mappend (Xor a) (Xor b) = Xor (a /= b)

-- F F -> F
-- F T -> T
-- T F -> T
-- T T -> F


newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Semigroup (Maybe' a) where

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' Nothing
    mappend (Maybe' Nothing) a = a
    mappend a (Maybe' Nothing) = a
    mappend (Maybe' a) (Maybe' b) = Maybe' (a `mappend` b)

test = Maybe' (Just [3]) `mappend` Maybe' (Just [6])