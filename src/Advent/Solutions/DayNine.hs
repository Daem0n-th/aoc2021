module Advent.Solutions.DayNine where

import Data.List (sortOn)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Ord (Down (Down))
import Debug.Trace (trace)

dayNine :: IO ()
dayNine = do
  input <- lines <$> readFile "./inputs/dayNine"
  print $ partOne (parse input)
  print $ partTwo (parse input)

parse :: [String] -> [[Int]]
parse = map (map (\x -> read [x]))

partOne :: [[Int]] -> Int
partOne arr = sum $ map (succ . el arr) (localMinimas arr)

partTwo :: [[Int]] -> Int
partTwo = product . take 3 . sortOn Down . basinSizes

basinSizes :: [[Int]] -> [Int]
basinSizes arr = map (length . basinPoints arr []) (localMinimas arr)

basinPoints :: [[Int]] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
basinPoints arr ep p = if el arr p < 9 && notElem p ep then foldl (basinPoints arr) (p : ep) (adjacentCoords arr p) else ep

localMinimas :: [[Int]] -> [(Int, Int)]
localMinimas arr = filter (isLocalMinima arr) (indices arr)

indices :: [[Int]] -> [(Int, Int)]
indices arr = [(x, y) | x <- [0 .. length arr -1], y <- [0 .. length (head arr) -1]]

isLocalMinima :: [[Int]] -> (Int, Int) -> Bool
isLocalMinima arr p = all (center <) (adjacent arr p)
  where
    center = el arr p

adjacent :: [[Int]] -> (Int, Int) -> [Int]
adjacent arr p = map (el arr) (adjacentCoords arr p)

adjacentCoords :: [[b]] -> (Int, Int) -> [(Int, Int)]
adjacentCoords arr (x, y) = [(pred x, y), (succ x, y), (x, pred y), (x, succ y)]

el :: [[Int]] -> (Int, Int) -> Int
el arr (x, y)
  | invalid arr (x, y) = 9
  | otherwise = arr !! x !! y

invalid :: [[Int]] -> (Int, Int) -> Bool
invalid arr (x, y) = x < 0 || y < 0 || x >= length arr || y >= length (head arr)