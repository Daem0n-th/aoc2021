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
  _ -> die "Invalid Day!"