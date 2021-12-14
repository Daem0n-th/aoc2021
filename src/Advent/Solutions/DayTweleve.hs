module Advent.Solutions.DayTweleve where

import Data.Char (isLower)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Map.Lazy (Map, empty, insertWith, (!))

type Cave = String

dayTweleve :: IO ()
dayTweleve = do
  input <- lines <$> readFile "./inputs/dayTweleve"
  print $ partOne (parse input)
  print $ partTwo (parse input)

parse :: [String] -> Map Cave [Cave]
parse = foldl (\acc (a, b) -> addCave (addCave acc (a, [b])) (b, [a])) empty . map (tuple . splitOn "-")
  where
    addCave m c = uncurry (insertWith (++)) c m
    tuple xs = (head xs, last xs)

partOne :: Map Cave [Cave] -> Int
partOne xs = length $ explore isExplored xs [] "start"

partTwo :: Map Cave [Cave] -> Int
partTwo xs = length $ explore isExploredTwo xs [] "start"

explore :: ([Cave] -> Cave -> Bool) -> Map Cave [Cave] -> [Cave] -> Cave -> [[Cave]]
explore f m p c
  | c == "end" = [p ++ ["end"]]
  | otherwise = concatMap (explore f m (p ++ [c])) (filter (not . f (p ++ [c])) (m ! c))

isSmall :: Cave -> Bool
isSmall = all isLower

isExplored :: [Cave] -> Cave -> Bool
isExplored p c = isSmall c && c `elem` p

isExploredTwo :: [Cave] -> Cave -> Bool
isExploredTwo p c
  | c == "start" = "start" `elem` p
  | isSmall c && c `elem` p = containsTwice
  | otherwise = False
  where
    containsTwice = smallCaves /= nub smallCaves
    smallCaves = filter isSmall p