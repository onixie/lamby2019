{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import Data.Char
import Text.ParserCombinators.ReadP
import Text.Printf

pa <++> pb = ((++) <$> pa <*> pb)

code :: ReadP Int
code = read <$> (option "" (string "-") <++> munch isDigit)

program :: ReadP [Int]
program = (code `sepBy` char ',') <* optional (char '\n') <* eof

readInput = do
  (i, "") <- readFile "data/Day5-input.txt" >>= return . last . readP_to_S program
  return i

interpret' :: Int -> [Int] -> IO [Int]
interpret' ip icp = case opc'm icp ip of
  (99, _) -> return icp
  (1, ms) -> printf "%d ADD (MODE=%s)\n" ip (show ms) >> eval (+) icp ip ms >>= interpret' (ip+4)
  (2, ms) -> printf "%d MUL (MODE=%s)\n" ip (show ms) >> eval (*) icp ip ms >>= interpret' (ip+4)
  (3, _)  -> printf "%d IN\n"  ip                     >> readIn   icp ip    >>= interpret' (ip+2)
  (4, _)  -> printf "%d OUT="  ip                     >> printOut icp ip    >>  interpret' (ip+2) icp
  (opc,_) -> printf "%d ERROR" >> undefined
  where
    eval op icp ip ms = do
--        print $ get icp (ip+1) (ms!!0) `op` get icp (ip+2) (ms!!1)
        return . update icp (ip+3) $ get icp (ip+1) (ms!!0) `op` get icp (ip+2) (ms!!1)
    readIn   icp ip = return $ update icp (ip+1) 1 --readLn >>= return . update icp pi
    printOut icp ip = print $ deref icp (ip+1)
    update icp ip nv =
      let (h, x:t) = splitAt (icp !! ip) icp in
        h ++ (nv:t)
    deref icp ip = icp !! (icp !! ip)
    get icp ip 0 = deref icp ip
    get icp ip 1 = icp !! ip

opc'm :: [Int] -> Int -> (Int, [Int])
opc'm pp ip = let (ms, op) = (pp !! ip) `quotRem` 100 in
                (op, pad $ md ms)
  where
    md 0  = []
    md ms = let (ms', m) = ms `quotRem` 10 in m:md ms'
    pad ms = take 2 (ms++repeat 0)

interpret pp = interpret' 0 pp

day5Part1 = readInput >>= interpret
