module Main where

import Advent.Solutions
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    [] -> die "No arguments given!"
    [x] -> solutionOf $ read x
    _ -> die "Multiple arguments!"

solutionOf :: Int -> IO ()
solutionOf day = case day of
  1 -> dayOne
  2 -> dayTwo
  3 -> dayThree
  4 -> dayFour
  5 -> dayFive
  6 -> daySix
  7 -> daySeven
  8 -> dayEight
  9 -> dayNine
  10 -> dayTen
  11 -> dayEleven
  12 -> dayTweleve
  13 -> dayThirteen
  14 -> dayFourteen
  _ -> die "Invalid Day!"