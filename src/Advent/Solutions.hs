module Advent.Solutions where

import Advent.Utils ( stringToList )

dayOne :: IO ()
dayOne = do
    input <- readFile "./inputs/dayOne"
    let inputList = stringToList input
    putStrLn $ "Part One : " ++ show (solutionPartOne inputList)
    putStrLn $ "Part Two : " ++ show (solutionPartTwo inputList)


solutionPartOne :: [Int] -> Int 
solutionPartOne l = snd (foldl func (head l, 0) l)
    where func (p,c) x = if x > p then (x, succ c) else (x, c)

solutionPartTwo :: [Int] -> Int 
solutionPartTwo = solutionPartOne . slidingSum 3

slidingSum :: Int -> [Int] -> [Int]
slidingSum _ [] = []
slidingSum n l@(x:xs)
    | length l <= n = [ sum l ]   
    | otherwise = sum (take n l) : slidingSum n xs  
