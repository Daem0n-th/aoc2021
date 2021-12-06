module Advent.Solutions.DaySix where

import Control.Lens.Combinators (element)
import Control.Lens.Setter ((+~))
import Data.List.Split (splitOn)

daySix :: IO ()
daySix = do
  input <- parse . map read . splitOn "," <$> readFile "./inputs/daySix"
  print $ partOne input
  print $ partTwo input

parse :: [Int] -> [Int]
parse = foldl (\acc x -> addAt x 1 acc) zeros
  where
    zeros = replicate 9 0

partOne :: [Int] -> Int
partOne = sum . atDay 80

partTwo :: [Int] -> Int
partTwo = sum . atDay 256

atDay :: Int -> [Int] -> [Int]
atDay 0 xs = xs
atDay n xs = atDay (pred n) (fishGrowth xs)

fishGrowth :: [Int] -> [Int]
fishGrowth = (\x -> addAt 6 (last x) x) . rotateL
  where
    rotateL [] = []
    rotateL (x : xs) = xs ++ [x]

addAt :: (Num a, Traversable t) => Int -> a -> t a -> t a
addAt i n = element i +~ n
