module Advent.Solutions.DayEight where

import Data.List (sort, (\\))
import Data.List.Split (splitOn)

dayEight :: IO ()
dayEight = do
  input <- lines <$> readFile "./inputs/dayEight"
  print $ partOne input
  print $ partTwo input

partOne :: [String] -> Int
partOne = length . filter (`elem` n) . digits
  where
    digits = concatMap (map length . words . last . splitOn " | ")
    n = [2, 4, 3, 7]

partTwo :: [String] -> Int
partTwo = sum . map decode

decode :: String -> Int
decode x = foldl (\acc x -> acc * 10 + x) 0 $ map (getKey digitMap . sort) $ words $ head b
  where
    (a : b) = splitOn " | " x
    digitMap = createMap $ map sort $ words a

createMap :: [String] -> [(Int, String)]
createMap xs = bm ++ map (cat5 bm) (onlyLen 5) ++ map (cat6 bm) (onlyLen 6)
  where
    onlyLen a = filter (\x -> length x == a) xs
    bm = baseMap xs

baseMap :: [String] -> [(Int, String)]
baseMap xs = map (func xs) [(1, 2), (4, 4), (7, 3), (8, 7)]
  where
    func xs (a, b) = (a, head $ filter (\x -> length x == b) xs)

cat5 :: [(Int, String)] -> String -> (Int, String)
cat5 bm x
  | get 1 `isIn` x = (3, x)
  | (get 4 \\ get 1) `isIn` x = (5, x)
  | otherwise = (2, x)
  where
    get = getEle bm

cat6 :: [(Int, String)] -> String -> (Int, String)
cat6 bm x
  | (get 8 \\ get 1) `isIn` x = (6, x)
  | get 4 `isIn` x = (9, x)
  | otherwise = (0, x)
  where
    get = getEle bm

getEle :: Eq a => [(a, b)] -> a -> b
getEle xs x = snd $ head $ filter (\(a, _) -> a == x) xs

getKey :: Eq b => [(a, b)] -> b -> a
getKey xs x = fst $ head $ filter (\(_, b) -> b == x) xs

isIn :: String -> String -> Bool
isIn a b = all (`elem` b) a