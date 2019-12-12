{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import Data.Char
import Text.ParserCombinators.ReadP
import Text.Printf

pa <++> pb = (++) <$> pa <*> pb

code :: ReadP Int
code = read <$> (option "" (string "-") <++> munch isDigit)

program :: ReadP [Int]
program = (code `sepBy` char ',') <* optional (char '\n') <* eof

readICPFrom path = do
  (i, "") <- last . readP_to_S program <$> readFile path
  return i

interpretFrom :: Int -> [Int] -> IO [Int]
interpretFrom ip icp = case opc'm icp ip of
  (99, _) -> return icp
  (1, ms) -> printf "%3d ADD (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3)) >> eval (+) icp ip ms >>= interpretFrom (ip+4)
  (2, ms) -> printf "%3d MUL (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3)) >> eval (*) icp ip ms >>= interpretFrom (ip+4)
  (3,  _) -> printf "%3d IN               %d=" ip           (icp!!(ip+1))                                    >> readIn   icp ip    >>= interpretFrom (ip+2)
  (4, ms) -> printf "%3d OUT (MODE=%s) %d=" ip (show ms) (icp!!(ip+1))                                    >> printOut icp ip ms >>  interpretFrom (ip+2) icp
  (5, ms) -> printf "%3d JNZ (MODE=%s) %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2))                  >> interpretFrom (jmpIf (/=0) icp ip ms) icp
  (6, ms) -> printf "%3d JEZ (MODE=%s) %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2))                  >> interpretFrom (jmpIf (==0) icp ip ms) icp
  (7, ms) -> printf "%3d LE  (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3)) >> cmp (<)  icp ip ms >>= interpretFrom (ip+4)
  (8, ms) -> printf "%3d EQ  (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3)) >> cmp (==) icp ip ms >>= interpretFrom (ip+4)
  (opc,_) -> printf "%3d ERROR" >> undefined
  where
    eval op icp ip ms  = return . update icp (ip+3) $ get icp (ip+1) (head ms) `op` get icp (ip+2) (ms!!1)
    readIn  icp ip     = update icp (ip+1) <$> readLn
    printOut icp ip ms = print $ get icp (ip+1) (head ms)
    update icp ip nv  =
      let (h, x:t) = splitAt (icp !! ip) icp in
        h ++ (nv:t)
    deref icp ip = icp !! (icp !! ip)
    get icp ip 0 = deref icp ip
    get icp ip 1 = icp !! ip
    jmpIf f icp ip ms = if f $ get icp (ip+1) (head ms) then get icp (ip+2) (ms!!1) else ip+3
    cmp f icp ip ms   = return . update icp (ip+3) $ if f (get icp (ip+1) (head ms)) (get icp (ip+2) (ms!!1)) then 1 else 0

opc'm :: [Int] -> Int -> (Int, [Int])
opc'm pp ip = let (ms, op) = (pp !! ip) `quotRem` 100 in
                (op, pad $ md ms)
  where
    md 0  = []
    md ms = let (ms', m) = ms `quotRem` 10 in m:md ms'
    pad ms = take 2 (ms++repeat 0)

interpret = interpretFrom 0

day5 = readICPFrom "data/Day5-input.txt" >>= interpret >> return ()
