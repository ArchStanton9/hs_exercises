
import Data.Char(isDigit)
import Data.List
import Text.Read


data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt ( x2 `sd` x1 + y2 `sd` y1) where
    sd a b = (a - b) ^ 2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x2 - x1) + abs (y2 - y1)

getCenter :: Double -> Coord Int -> Coord Double
getCenter step (Coord x y) = let
    move a = step * fromIntegral a + step / 2.0 
    in Coord (move x) (move y)

getCell :: Double -> Coord Double -> Coord Int
getCell step (Coord x y) = let
    move a = floor $ a / step 
    in Coord (move x) (move y)


findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs)
    | isDigit x = Just x
    | otherwise = findDigit xs


findDigitOrX :: [Char] -> Char
findDigitOrX xs = case findDigit xs of
    Nothing -> 'X'
    Just c  -> c


maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x



data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

parsePerson :: String -> Either Error Person
parsePerson str = case readDict str of
    (Right d) -> readAge d $ readLastName d $ readFirstName d Person{}
    (Left er)    -> Left er

-- firstName = John
-- lastName = Connor
-- age = 30

l1 = "firstName = test"

readFirstName :: [(String, String)] -> Person -> Either Error Person
readFirstName dict p = case lookup "firstName" dict of
    Nothing  -> Left IncompleteDataError
    Just str -> Right p {firstName = str} 
    
readLastName :: [(String, String)] -> Either Error Person -> Either Error Person
readLastName _    (Left er) = Left er
readLastName dict (Right p) = case lookup "lastName" dict of
    Nothing  -> Left IncompleteDataError
    Just str -> Right p {lastName = str} 


readAge :: [(String, String)] -> Either Error Person -> Either Error Person
readAge _    (Left er) = Left er
readAge dict (Right p) = case lookup "age" dict of
    Nothing  -> Left IncompleteDataError
    Just str -> case readMaybe str :: Maybe Int of
        Nothing -> Left (IncorrectDataError str)
        Just a  -> Right p {age = a} 

readDict :: String -> Either Error [(String, String)]
readDict str 
        | any malformed ls = Left ParsingError
        | otherwise = Right $ map (\x -> (readKey x, readValue x)) ls
        where 
            ls = lines str
            readValue = drop 2 . dropWhile (/= '=')
            readKey = init . takeWhile (/= '=')
            malformed s = length (filter (\x -> x == '=') s) /= 1

descriptors = [
    ("firstName", "John"),
    ("lastName", "Conor"),
    ("age", "30")
    ]


testPerson = readAge descriptors $ readLastName descriptors $ readFirstName descriptors Person{}

lfull = "firstName = John\nlastName = Connor\nage = 30a"
test = parsePerson lfull


eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing