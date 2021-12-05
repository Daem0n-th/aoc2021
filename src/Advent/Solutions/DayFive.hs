module Advent.Solutions.DayFive where

import Data.List
import Data.List.Split (splitOn)

type Vent = (Int, Int)

type VentLine = ((Int, Int), (Int, Int))

dayFive :: IO ()
dayFive = do
  input <- map parse . lines <$> readFile "./inputs/dayFive"
  print $ partOne input
  print $ partTwo input

parse :: String -> VentLine
parse = tuple . map (tuple . map read . splitOn ",") . splitOn " -> "
  where
    tuple x = (head x, last x)

partOne :: [VentLine] -> Int
partOne = overlappingVents . filter (\x -> vertical x || horizontal x)

partTwo :: [VentLine] -> Int
partTwo = overlappingVents

overlappingVents :: [VentLine] -> Int
overlappingVents = length . filter (\x -> length x > 1) . group . sort . concatMap vents

vents :: VentLine -> [Vent]
vents ((x1, y1), (x2, y2)) = zip (enum x1 x2) (enum y1 y2)
  where
    enum i j
      | i > j = reverse [j .. i]
      | i < j = [i .. j]
      | otherwise = repeat i

horizontal :: VentLine -> Bool
horizontal ((x1, _), (x2, _)) = x1 == x2

vertical :: VentLine -> Bool
vertical ((_, y1), (_, y2)) = y1 == y2