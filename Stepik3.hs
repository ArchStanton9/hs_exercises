module Stepik3 where
import Data.Char

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y lst = x : y : lst 

nTimes:: a -> Int -> [a]
nTimes a n = helper [] n where 
    helper lst 0 = lst
    helper lst i = helper (a : lst) (i - 1)
    
oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs)
    | mod x 2 == 1 = x : oddsOnly xs
    | otherwise = oddsOnly xs


isPalindrome :: Eq a => [a] -> Bool
isPalindrome [_] = True
isPalindrome [] = True
isPalindrome (x:xs)
    | x == last xs = isPalindrome $ init xs
    | otherwise = False


sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 xs ys [] = sum3 xs ys [0]  
sum3 xs [] zs = sum3 xs [0] zs
sum3 [] ys zs = sum3 [0] ys zs
sum3 (x:xs) (y:ys) (z:zs) = (x + y + z) : sum3 xs ys zs


appendElem :: Eq a => a -> [[a]] -> [[a]]
appendElem a [] = [[a]]
appendElem a [[]] = [[a]]
appendElem a [b:bs]
    | a == b = [a:b:bs]
    | otherwise = [a] : [b:bs]

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems [x] = [[x]]
groupElems (x:xs)
    | x == head xs =
        let (y:ys) = groupElems xs
        in ((x : y) : ys)
    | otherwise = [x] : groupElems xs


readDigits :: String -> (String, String)
readDigits x = (num, str) where
    num = takeWhile isDigit x
    str = dropWhile isDigit x
       
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x:xs)
    | p1 x = x : filterDisj p1 p2 xs
    | p2 x = x : filterDisj p1 p2 xs
    | otherwise = filterDisj p1 p2 xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort(filter (<x) xs) ++ [x] ++ qsort(filter (>=x) xs)


squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

-- perms :: [a] -> [[a]]
-- perms [x] = [[x]]
-- perms [x, y] = [[x,y], [y,x]]
-- perms (x:xs) = perms xs ++ concatMap (\y -> perms[y, x]) xs


-- pairs :: [a] -> [[a]]
-- pairs [x] = [[x]]
-- pairs [x, y] = [[x,y], [y,x]]
-- pairs (x:xs) = concatMap (\y -> perms[y, x]) xs ++ pairs xs

simplePerms :: [a] -> [[a]]
simplePerms [x] = [[x]]
simplePerms (x:xs) = concatMap (\z -> [reverse z, z]) (map (\y -> x : y) (perms xs))

pp :: [a] -> [[a]]
pp [x] = [[x]]
pp (x:xs) = concatMap (\y -> [x : y, x : reverse y]) [xs]


perms :: [a] -> [[a]]
perms [x] = [[x]]
perms (x:xs) = concatMap (\z -> mv z) (map (\y -> x : y) (perms xs))

mv :: [a] -> [[a]]
mv a = helper [a] (length a - 1) where 
    helper v 0 = v
    helper v n = helper ((last d : init d) : v) (n - 1)
        where d = head v


delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

txt = "Abc IS not ABC"
        

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 x y z = zipWith3 m3 x y z where 
    m3 a b c = max a $ max b c



-- [1,2,3],[3,2,1],[1,3,2],[2,3,1]]
--[1,2]
--[2,1]

--[1,2,3]
--[3,1,2]
--[2,3,1]

nats n = n : nats (n + 1)

fibStream :: [Integer]
fibStream = zipWith (+) (0:fibStream) (0:1:fibStream)

repeat1 = iterate repeatHelper

repeatHelper = id

data Odd = Odd Integer 
  deriving (Eq, Show)

instance Enum Odd where
    succ (Odd x) = Odd(x + 2)
    pred (Odd x) = Odd(x - 2)
    toEnum x = Odd (2*(toInteger x) - 1)
    fromEnum (Odd x) = (fromInteger x + 1) `div` 2
    enumFrom (Odd x) = (Odd x) : enumFrom (Odd (x + 2))
    enumFromThen (Odd x) (Odd y) = (Odd x) : enumFromThen (Odd y) (Odd (y + (y - x)))
    enumFromTo (Odd x) (Odd y)
        | x > y = []
        | x == y = [Odd x]
        | otherwise = takeWhile (/= succ (Odd y)) (enumFrom(Odd x))
    enumFromThenTo (Odd x) (Odd y) (Odd z)
        | x <= y && (y - (y - x)) > z = []
        | x > y && (y - (y - x)) < z = []
        | otherwise = Odd x : enumFromThenTo (Odd y) (Odd (y + (y - x))) (Odd z)

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m 
    | m `mod` 2 == 0 = Odd (n + m)
    | otherwise      = error "addEven: second parameter cannot be odd"

-- Большое число, которое не поместится в Int
baseVal = 9900000000000000000

-- Генератор значений для тестирования
testVal n = Odd $ baseVal + n
-- для проверки самих тестов. Тесты с 0..3 не должны выполняться
-- testVal = id

test0 = succ (testVal 1) == (testVal 3)
test1 = pred (testVal 3) == (testVal 1)
-- enumFrom
test2 = take 4 [testVal 1 ..] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- enumFromTo
-- -- По возрастанию
test3 = take 9 [testVal 1..testVal 7] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- -- По убыванию
test4 = take 3 [testVal 7..testVal 1] == []
-- enumFromThen
-- -- По возрастанию
test5 = take 4 [testVal 1, testVal 5 ..] == [testVal 1,testVal 5,testVal 9,testVal 13]
-- -- По убыванию
test6 = take 4 [testVal 5, testVal 3 ..] == [testVal 5,testVal 3,testVal 1,testVal (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 = [testVal 1, testVal 5 .. testVal 11] == [testVal 1,testVal 5,testVal 9]
-- -- По убыванию
test8 = [testVal 7, testVal 5 .. testVal 1] == [testVal 7,testVal 5,testVal 3,testVal 1]
-- -- x1 < x3 && x1 > x2
test9 = [testVal 7, testVal 5 .. testVal 11] == []
-- -- x1 > x3 && x1 < x2
test10 = [testVal 3, testVal 5 .. testVal 1] == []

test11 = take 4 [testVal 5, testVal 5 .. ] == replicate 4 (testVal 5)
test12 = take 4 [testVal 5, testVal 5 .. testVal 11] == replicate 4 (testVal 5)
test13 = take 4 [testVal 5, testVal 5 .. testVal 5] == replicate 4 (testVal 5)
test14 = [testVal 5, testVal 5 .. testVal 3] == []
test15 = [testVal 5, testVal 1 .. testVal 5] == [testVal 5]
test16 = toEnum (fromEnum (Odd 3)) == Odd 3
-- Это сомнительный тест. Скорее всего, его нет на stepik
test17 = fromEnum(Odd 3) + 1 == fromEnum(Odd 5)

testList = [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, 
            test11, test12, test13, test14, test15, test16, test17]
allTests = zip [0..] testList
-- Список тестов с ошибками
badTests = map fst $ filter (not . snd) allTests
