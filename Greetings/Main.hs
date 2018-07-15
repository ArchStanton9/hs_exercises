module Main where

import System.IO

main :: IO ()
main = do
    putStrLn "What is your name?";
    putStr "Name: "
    hFlush stdout
    name <- getLine;
    case name of
        []  -> main
        str -> putStrLn $ "Hi, " ++ name ++ "!"