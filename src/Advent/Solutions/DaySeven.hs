module Advent.Solutions.DaySeven where

import Data.List.Split (splitOn)

daySeven :: IO ()
daySeven = do
  input <- map read . splitOn "," <$> readFile "./inputs/daySeven"
  print $ partOne input
  print $ partTwo input

partOne :: [Int] -> Int
partOne = fuelCost constant

partTwo :: [Int] -> Int
partTwo = fuelCost arithmatic

fuelCost :: (Int -> Int -> Int) -> [Int] -> Int
fuelCost f xs = minimum $ map (cost f) [1 .. maximum xs]
  where
    cost f x = sum $ map (f x) xs

constant :: Int -> Int -> Int
constant c x = abs (c - x)

arithmatic :: Int -> Int -> Int
arithmatic c x = sum [1 .. constant c x]
