module Advent.Solutions.DayTen where

import Data.Either (lefts, rights)
import Data.List (sort)
import Data.Maybe (mapMaybe)

type Stack = [String]

push :: Stack -> String -> Stack
push xs x = x : xs

pop :: Stack -> Stack
pop [] = []
pop (_ : xs) = xs

dayTen :: IO ()
dayTen = do
  input <- lines <$> readFile "./inputs/dayTen"
  print $ partOne input
  print $ partTwo input

partOne :: [String] -> Int
partOne = sum . map pointsOne . lefts . map (parse [])

partTwo :: [String] -> Int
partTwo = (\x -> x !! (length x `div` 2)) . sort . map (foldl (\acc x -> acc * 5 + x) 0 . map (pointsTwo . closing)) . rights . map (parse [])

parse :: Stack -> String -> Either String Stack
parse s [] = Right s
parse s (x : xs) = case handle s [x] of
  Right st -> parse st xs
  Left st -> Left st

handle :: Stack -> String -> Either String Stack
handle s x
  | x `elem` ["(", "[", "{", "<"] = Right (push s x)
  | otherwise = if closing (head s) == x then Right $ pop s else Left x

closing :: String -> String
closing x = case x of
  "(" -> ")"
  "[" -> "]"
  "{" -> "}"
  _ -> ">"

pointsOne :: String -> Int
pointsOne x = case x of
  ")" -> 3
  "]" -> 57
  "}" -> 1197
  _ -> 25137

pointsTwo :: String -> Int
pointsTwo x = case x of
  ")" -> 1
  "]" -> 2
  "}" -> 3
  _ -> 4