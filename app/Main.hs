module Main where
import System.Environment ( getArgs )
import Advent.Solutions (dayOne)
import System.Exit (die)

main :: IO ()
main = do
    arg <- getArgs
    case arg of
        [ ] -> die "No arguments given!"
        [x] -> solutionOf $ read x 
        _   -> die "Multiple arguments!"

solutionOf :: Int -> IO ()
solutionOf day = case day of
    1 -> dayOne
    _ -> error "Invalid Day!"
          