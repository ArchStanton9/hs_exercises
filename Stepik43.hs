import Data.Time.Clock
import Data.Time.Format

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString Error   = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info    = "Info"

logEntryToString :: LogEntry -> String
logEntryToString entry = concat 
    [ timeToString $ timestamp entry , ": "
    , logLevelToString $ logLevel entry , ": "
    , message entry ]


data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 { lastName = lastName p1}

bill = Person { firstName = "Bill", lastName = "Clinton", age = 51}
dave = Person { firstName = "Dave", lastName = "Mustain", age = 52}

abbrFirstName :: Person -> Person
abbrFirstName p = p { firstName = trim $ firstName p } where
    trim s
        | length s < 2 = s 
        | otherwise    = head s : "."

