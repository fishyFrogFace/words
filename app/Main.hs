module Main where

import System.Environment
import Lib

count :: String -> IO ()
count x = do
  file <- readFile x
  print . take 50 $ byLength $ lines file

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> count x
    _   -> putStrLn "You need to provide a filename"
