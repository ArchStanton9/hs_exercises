module Stepik51 where

import Data.Char
import Data.List

data Point3D a = Point3D a a a deriving Show


instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

test = fmap (+ 1) (Point3D 5 6 7)


data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
    deriving Show

instance Functor GeomPrimitive where
    fmap f (Point p) = Point (fmap f p)
    fmap f (LineSegment p1 p2) = LineSegment (fmap f p1) (fmap f p2)

-- test1 = fmap (+ 1) $ Point (Point3D 0 0 0)
-- test2 = fmap (+ 1) $ LineSegment (Point3D 0 0 0) (Point3D 1 1 1)


data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show


instance Functor Tree where
    fmap f (Leaf m) = Leaf (fmap f m)
    fmap f (Branch a b c) = Branch (fmap f a) (fmap f b) (fmap f c)

-- test1 = words <$> Leaf Nothing
-- test2 = words <$> Leaf (Just "a b")

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
    fmap f (Entry (k1, k2) v) = Entry (k1, k2) (f v) 

instance Functor (Map k1 k2) where
    fmap f (Map a) = Map (fmap (fmap f) a)

elist = [(Entry (1,2) 5), (Entry (3,6) 7)]


test1 = fmap (map toUpper) $ Map []
test2 = fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]