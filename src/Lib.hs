module Lib
    ( byLength
    ) where

byLength :: [[a]] -> [[[a]]]
byLength []  = []
byLength lst = let genres = []:genres in bl lst genres

bl :: [[a]] -> [[[a]]] -> [[[a]]]
bl [] res     = res
bl (x:xs) res = bl xs $ update 0 x res

update :: Int -> [a] -> [[[a]]] -> [[[a]]]
update inc n (x:xs)
  | inc == (length n)  = (n:x):xs
  | otherwise        = x:update (inc+1) n xs

