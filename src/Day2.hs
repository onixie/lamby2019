module Day2 where

import Data.Char
import Text.ParserCombinators.ReadP

code :: ReadP Int
code = read <$> munch isDigit

program :: ReadP [Int]
program = (code `sepBy` char ',') <* optional (char '\n') <* eof

readInput :: IO [Int]
readInput = do
  (op:x:y:vs, "") <- readFile "data/Day2-input.txt" >>= return . last . readP_to_S program
  return (op:12:2:vs)

interpret' pc pp
  | pp !! pc == 99 = pp
  | pp !! pc == 1  = interpret' nextPC $ eval (+) pp pc
  | pp !! pc == 2  = interpret' nextPC $ eval (*) pp pc
  | otherwise = undefined
  where
    eval op pp pc =
      let r = deref pp (pc + 1) `op` deref pp (pc + 2) in
        update pp (pc + 3) r
    update pp pc v =
      let (h, x:t) = splitAt (pp !! pc) pp in
        h ++ (v:t)
    deref pp pc = pp !! (pp !! pc)
    nextPC = pc + 4

interpret pp = interpret' 0 pp

day2Part1 = readInput >>= return . head . interpret

day2Part2 = do
  (op:x:y:vs) <- readInput
  return . head $ [100 * noun + verb | noun <- [0..99],
                                       verb <- [0..99],
                                       (head . interpret $ op:noun:verb:vs) == 19690720]
