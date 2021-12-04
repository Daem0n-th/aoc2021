module Advent.Solutions.DayFour where

import Data.List (groupBy, transpose)
import Data.List.Split (splitOn)

data Cell = Cell (Int, Int) Int Bool
  deriving (Eq, Show)

type Board = [Cell]

dayFour :: IO ()
dayFour = do
  (x : xs) <- splitOn "\n\n" <$> readFile "./inputs/dayFour"
  let num = map read $ splitOn "," x :: [Int]
  let puzzles = map parsePuzzle xs
  print $ partOne num puzzles
  print $ partTwo num puzzles

parsePuzzle :: String -> Board
parsePuzzle p = [Cell i v False | (i, v) <- zip index values]
  where
    index = [(x, y) | x <- [0 .. 4], y <- [0 .. 4]]
    values = (map read . words) p

partOne :: [Int] -> [Board] -> Int
partOne n p =
  case getWinner mp of
    Nothing -> partOne (tail n) mp
    Just x -> calcScore (head n) x
  where
    mp = map (`markBoard` head n) p

partTwo :: [Int] -> [Board] -> Int
partTwo n p =
  case getOnlyWinner mp of
    Nothing -> partTwo (tail n) $ filter (not . hasWon) mp
    Just x -> calcScore (head n) x
  where
    mp = map (`markBoard` head n) p

getWinner :: [Board] -> Maybe Board
getWinner boards = case filter hasWon boards of
  [] -> Nothing
  (x : _) -> Just x

getOnlyWinner :: [Board] -> Maybe Board
getOnlyWinner boards =
  case filter hasWon boards of
    [] -> Nothing
    [x] -> if [x] == boards then Just x else Nothing
    _ -> Nothing

hasWon :: Board -> Bool
hasWon xs = any bingo (getRows xs ++ getCols xs)
  where
    bingo = all (\(Cell _ _ p) -> p)

getRows :: Board -> [[Cell]]
getRows = groupBy sameRow
  where
    sameRow (Cell (x, _) _ _) (Cell (y, _) _ _) = x == y

getCols :: Board -> [[Cell]]
getCols = transpose . getRows

markBoard :: Board -> Int -> Board
markBoard b v = map (mark v) b
  where
    mark x c@(Cell i y _) = if x == y then Cell i y True else c

calcScore :: Int -> Board -> Int
calcScore x b = x * sum (map (\(Cell _ x _) -> x) $ filter (\(Cell _ _ p) -> not p) b)