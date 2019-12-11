module Main where

import Day1
import Day2
import Day3
import Day4
import Day5

main :: IO ()
main = do
  -- Day1
  day1Part1 >>= print
  day1Part2 >>= print
  -- Day2
  day2Part1 >>= print
  day2Part2 >>= print
  -- Day3
  day3Part1 >>= print
  day3Part2 >>= print
  -- Day4
  return day4Part1 >>= print
  return day4Part2 >>= print
  -- Day5
  day5
