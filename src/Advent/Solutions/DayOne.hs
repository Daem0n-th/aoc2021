module Advent.Solutions.DayOne where

import Advent.Utils (stringToList)

dayOne :: IO ()
dayOne = do
  input <- readFile "./inputs/dayOne"
  let inputList = stringToList input
  putStrLn $ "Part One : " ++ show (partOne inputList)
  putStrLn $ "Part Two : " ++ show (partTwo inputList)

partOne :: [Int] -> Int
partOne [] = 0
partOne cs@(x : _) = snd (foldl func (x, 0) cs)
  where
    func (p, c) x = if x > p then (x, succ c) else (x, c)

partTwo :: [Int] -> Int
partTwo = partOne . slidingSum 3
  where
    slidingSum _ [] = []
    slidingSum n cs@(_ : xs)
      | length cs <= n = [sum cs]
      | otherwise = sum (take n cs) : slidingSum n xs
