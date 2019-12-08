module Main where

import Day1
import Day2

main :: IO ()
main = do
  -- Day1
  day1Part1 >>= print
  day1Part2 >>= print
  -- Day2
  day2Part1 >>= print
  day2Part2 >>= print
