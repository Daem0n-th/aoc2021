module Advent.Solutions.DayTwo where

dayTwo :: IO ()
dayTwo = do
  input <- readFile "./inputs/dayTwo"
  let il = lines input
  print $ partOne il
  print $ partTwo il

partOne :: [String] -> Int
partOne = uncurry (*) . foldl func (0, 0)
  where
    func (h, d) x =
      case words x of
        ["forward", v] -> (h + read v, d)
        ["down", v] -> (h, d + read v)
        ["up", v] -> (h, d - read v)
        _ -> undefined

partTwo :: [String] -> Int
partTwo = uncurry (*) . (\(a, b, _) -> (a, b)) . foldl func (0, 0, 0)
  where
    func (h, d, a) x =
      case words x of
        ["forward", v] -> (h + read v, d + (read v * a), a)
        ["down", v] -> (h, d, a + read v)
        ["up", v] -> (h, d, a - read v)
        _ -> undefined
