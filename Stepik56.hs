module Stepik56 where

import Data.Char
import Data.List
import Control.Monad
import Control.Monad.Writer


data Reader r a = Reader { runReader :: (r -> a) }

instance Functor (Reader r) where
    fmap f a = undefined

instance Applicative (Reader r) where
    pure = return
    (<*>) = undefined

instance Monad (Reader r) where
    return x = Reader $ \_ -> x
    m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

asks:: (r -> a) -> Reader r a
asks = Reader

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \e -> runReader m (f e)

users = [("user", "123456"), ("x", "hi"), ("root", "123456")]

type User = String
type Password = String
type UsersTable = [(User, Password)]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = let bad u = "123456" == snd u
    in asks $ map fst . filter bad

test = runReader usersWithBadPasswords users

-- type Shopping = Writer (Sum Integer) ()

-- shopping1 :: Shopping
-- shopping1 = do
--     purchase "Jeans"   19200
--     purchase "Water"     180
--     purchase "Lettuce"   328

-- purchase :: String -> Integer -> Shopping
-- purchase item cost = writer $ (,) () (Sum cost)

-- total :: Shopping -> Integer
-- total = getSum . execWriter



type Shopping = Writer ([String], Sum Integer) ()

shopping1 :: Shopping
shopping1 = do
    purchase "Jeans"   19200
    purchase "Water"     180
    purchase "Lettuce"   328

purchase :: String -> Integer -> Shopping
purchase item cost = writer $ (,) () ([item], Sum cost)

total :: Shopping -> Integer
total = getSum . snd . execWriter

items :: Shopping -> [String]
items = fst . execWriter
