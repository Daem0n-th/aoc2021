{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Advent.Solutions.DayFourteen where

import Data.Bifunctor (bimap, first)
import Data.List.Split (splitOn)
import Data.Map.Lazy (Map, assocs, empty, fromList, fromListWith, insertWith, (!))

type Rules = Map String String

type Polymer = Map String Int

type Residue = (String, Int)

dayFourteen :: IO ()
dayFourteen = do
  input <- parse <$> readFile "./inputs/dayFourteen"
  print $ partOne input
  print $ partTwo input

parse :: String -> ((Polymer, Residue), Rules)
parse = bimap parsePolymer parseRules . tuple . splitOn "\n\n"
  where
    parsePolymer xs = first (fromListWith (+)) $ (\x -> (init x, last x)) $ zipWith (\a b -> ([a, b], 1)) xs (drop 1 $ cycle xs)
    parseRules = fromList . map (tuple . splitOn " -> ") . lines
    tuple x = (head x, last x)

partOne :: ((Polymer, Residue), Rules) -> Int
partOne ((p, x), r) = diff $ calcFrequency (x : assocs (nPolymerize 10 p r))

partTwo :: ((Polymer, Residue), Rules) -> Int
partTwo ((p, x), r) = diff $ calcFrequency (x : assocs (nPolymerize 40 p r))

diff :: [Int] -> Int
diff xs = maximum xs - minimum xs

nPolymerize :: Int -> Polymer -> Rules -> Polymer
nPolymerize 0 p _ = p
nPolymerize n p r = nPolymerize (pred n) (polymerize r p) r

polymerize :: Rules -> Polymer -> Polymer
polymerize r p = foldl (step r) empty (assocs p)

step :: Rules -> Polymer -> (String, Int) -> Polymer
step r np (x, n) = insertWith (+) b n (insertWith (+) a n np)
  where
    [a, b] = transform x (r ! x)

transform :: String -> String -> [String]
transform a b = [head a : b, b ++ tail a]

calcFrequency :: [(String, Int)] -> [Int]
calcFrequency = map (\(_, a) -> div a 2) . assocs . foldl updateFreq empty

updateFreq :: Map String Int -> (String, Int) -> Map String Int
updateFreq m (xs, n) = insertWith (+) (init xs) n (insertWith (+) (tail xs) n m)
