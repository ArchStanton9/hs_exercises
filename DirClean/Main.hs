module Main where

import System.IO
import System.Directory
import Control.Monad
import Data.List


test a = filter (isSubsequenceOf a) ["vova", "biba", "vasyan"]

-- deleteFile name = do
--     putStrLn ("Removing file: " ++ name);
--     removeFile name;

-- deleteFiles name = do
--     xs <- getDirectoryContents ".";
--     let files = filter (isSubsequenceOf name) xs;
--     mapM deleteFile files
--     return ()


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    putStr "Substring: "
    substr <- getLine;
    if null substr 
        then putStrLn "Canceled"
        else do
            content <- getDirectoryContents ".";
            let files = filter (isSubsequenceOf substr) content; 
            mapM putStrLn ["Removing file: " ++ f | f <- files];
            mapM removeFile files;
            return ()