module Lib
    ( byLength
    ) where

byLength :: [[a]] -> [Int]
byLength []  = []
byLength lst = let genres = 0:genres in bl lst genres

bl :: [[a]] -> [Int] -> [Int]
bl [] res     = res
bl (x:xs) res = bl xs $ update 0 (length x) res

update :: Int -> Int -> [Int] -> [Int]
update inc n (x:xs)
  | inc == n  = x+1:xs
  | otherwise = x:update (inc+1) n xs
