module Main where

import System.Environment (getArgs)
import Data.Char (isDigit)
import Lib

help :: String
help = "\nhelp         - display this list of commands" ++
       "\ndistribution - display word length distribution" ++
       "\ncountAll     - count the words in the file" ++
       "\ncountIndex n - count words of length n" ++
       "\nshowIndex n  - display all words of length n"

areDigits :: String -> Bool
areDigits []     = True
areDigits (x:xs)
  | isDigit x = areDigits xs
  | otherwise = False

process :: String -> IO ()
process x = do
  file <- readFile x
  let allWords = lines file
  let sorted = take 50 $ byLength $ allWords
  putStrLn "Type 'help' to see a list of commands\n"
  action <- getLine
  case words action of
    ["help"]          -> putStrLn help
    ["distribution"]  -> print $ map length sorted
    ["countAll"]      -> print $ length allWords
    ["countIndex", n] -> if areDigits n then
                           print $ length $ sorted !! (read n :: Int)
                         else
                           print "Not a valid index\n"
    ["showIndex", n]  -> if areDigits n then
                           print $ sorted !! (read n :: Int)
                         else
                           print "Not a valid index"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> process x
    _   -> putStrLn "You need to provide a filename"
