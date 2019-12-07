module Day1 where


readInput :: IO [Int]
readInput = readFile "data/Day1-input.txt" >>= return . fmap read . lines

day1Part1 = readInput >>= return . foldl1 (+) . fmap ((subtract 2) . (`quot` 3))

calcFuel mass
  | fuel > 0  = fuel + calcFuel fuel
  | otherwise = 0
  where
    fuel = mass `quot` 3 - 2

day1Part2 = readInput >>= return . foldl1 (+) . fmap calcFuel
