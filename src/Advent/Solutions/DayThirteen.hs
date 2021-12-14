module Advent.Solutions.DayThirteen where

import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (groupBy, maximumBy, nub, partition, sortOn)
import Data.List.Split (splitOn)

type Point = (Int, Int)

type Paper = [Point]

type Fold = (Char, Int)

dayThirteen :: IO ()
dayThirteen = do
  input <- parse <$> readFile "./inputs/dayThirteen"
  print $ partOne input
  prettyPrint $ partTwo input

parse :: String -> (Paper, [Fold])
parse = bimap parsePoints parseFolds . tuple . splitOn "\n\n"
  where
    parsePoints = map (tuple . map read . splitOn ",") . lines
    parseFolds = map (bimap last read . tuple . splitOn "=") . lines
    tuple x = (head x, last x)

partOne :: (Paper, [Fold]) -> Int
partOne (p, fs) = length $ fold (head fs) p

partTwo :: (Paper, [Fold]) -> Paper
partTwo (p, fs) = foldl (flip fold) p fs

fold :: Fold -> Paper -> Paper
fold (t, n) = uncurry merge . partition (\(x, y) -> if t == 'x' then x < n else y < n)
  where
    merge a b = nub (a ++ map flipPoints b)
    flipPoints (a, b) = if t == 'x' then (2 * n - a, b) else (a, 2 * n - b)

prettyPrint :: Paper -> IO ()
prettyPrint xs = mapM_ (putStrLn . concatMap (\p -> if p `elem` xs then "#" else ".")) $ groupBy (\a b -> snd a == snd b) $ sortOn snd [(x, y) | x <- [0 .. mx], y <- [0 .. my]]
  where
    mx = fst $ maximumBy (compare `on` fst) xs
    my = snd $ maximumBy (compare `on` snd) xs