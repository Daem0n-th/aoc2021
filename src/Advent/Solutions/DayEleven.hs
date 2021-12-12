module Advent.Solutions.DayEleven where

import Data.Bifunctor (Bifunctor (first))

type Octopus = (Int, Bool)

type Cavern = ([Octopus], Int)

dayEleven :: IO ()
dayEleven = do
  input <- concat . lines <$> readFile "./inputs/dayEleven"
  print $ partOne (parse input, 0)
  print $ partTwo (parse input, 0)

parse :: String -> [Octopus]
parse = map (\x -> (read [x], False))

partOne :: Cavern -> Int
partOne = snd . simulate 100
  where
    simulate 0 c = c
    simulate n c = simulate (pred n) (singleStep c)

partTwo :: Cavern -> Int
partTwo = simulate 0
  where
    simulate n c
      | all (\(x, _) -> x == 0) (fst c) = n
      | otherwise = simulate (succ n) (singleStep c)

singleStep :: Cavern -> Cavern
singleStep = stage3 . stage2 . stage1

stage1 :: Cavern -> Cavern
stage1 = first (map (first succ))

stage2 :: Cavern -> Cavern
stage2 (xs, n)
  | any aboutToFlash xs = stage2 (result, n)
  | otherwise = (xs, n)
  where
    result = foldl handleFlash xs (flashPoints xs)

handleFlash :: [Octopus] -> Int -> [Octopus]
handleFlash s i = incAdj i $ flash i s

flash :: Int -> [Octopus] -> [Octopus]
flash i xs = map (\x -> if i == x then (fst $ xs !! x, True) else xs !! x) [0 .. length xs -1]

incAdj :: Int -> [Octopus] -> [Octopus]
incAdj i xs = map (\x -> if x `elem` adjPoints (length xs) i then first succ (xs !! x) else xs !! x) [0 .. length xs -1]

adjPoints :: Int -> Int -> [Int]
adjPoints l x =
  [x + m, x - m]
    ++ ( if x `mod` m < 1
           then []
           else [x - 1, x - m -1, x + m -1]
       )
    ++ (if x `mod` m >= m -1 then [] else [x + 1, x - m + 1, x + m + 1])
  where
    m = round $ sqrt $ fromIntegral l

flashPoints :: [Octopus] -> [Int]
flashPoints xs = filter (\x -> aboutToFlash (xs !! x)) [0 .. length xs -1]

calcFlashes :: [Octopus] -> Int
calcFlashes = length . filter snd

aboutToFlash :: Octopus -> Bool
aboutToFlash (val, flashed) = val > 9 && not flashed

stage3 :: Cavern -> Cavern
stage3 (xs, n) = (map reset xs, n + calcFlashes xs)
  where
    reset (x, _) = if x > 9 then (0, False) else (x, False)
