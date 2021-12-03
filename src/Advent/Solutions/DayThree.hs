module Advent.Solutions.DayThree where

import Data.Bits (Bits (complement))
import Data.Function
import Data.List

dayThree :: IO ()
dayThree = do
  input <- lines <$> readFile "./inputs/dayThree"
  print $ partOne input
  print $ partTwo input

partOne :: [String] -> Int
partOne x = gamma x * epsilon x
  where
    epsilon = binStrToDec . flipBits . mostCommonColumn
    gamma = binStrToDec . mostCommonColumn

partTwo :: [String] -> Int
partTwo x = o2Rating x * co2Rating x
  where
    o2Rating = binStrToDec . head . cfilter mostCommonColumn 0
    co2Rating = binStrToDec . head . cfilter (flipBits . mostCommonColumn) 0

cfilter :: ([String] -> String) -> Int -> [String] -> [String]
cfilter _ _ [x] = [x]
cfilter f i x = cfilter f (i + 1) (filter func x)
  where
    func xs = f x !! i == xs !! i
    mCom = map mostCommon . transpose

mostCommonColumn :: [String] -> String
mostCommonColumn = intercalate "" . map mostCommon . transpose

mostCommon :: String -> String
mostCommon = return . head . maximumBy (compare `on` length) . group . sort

flipBits :: String -> String
flipBits = map func
  where
    func '0' = '1'
    func '1' = '0'
    func _ = error "Invalid literal"

binStrToDec :: String -> Int
binStrToDec [] = 0
binStrToDec ('0' : xs) = binStrToDec xs
binStrToDec ('1' : xs) = (2 ^ length xs) + binStrToDec xs
binStrToDec _ = error "Invalid literal for binary string"
