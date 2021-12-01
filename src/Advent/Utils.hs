module Advent.Utils where

stringToList :: String -> [Int]
stringToList str = map (\x -> read x :: Int) $ lines str
