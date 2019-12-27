{-# LANGUAGE BangPatterns #-}
module Day19 where

import Conduit
import Control.Monad.State (StateT, void, when, forM_)
import Control.Monad.Parallel (forM)
import Day5 (readICPFrom)
import Day7 (readICPFromC)
import Day9 (interpretC, run)
import Graphics.Gloss
import Data.Ratio
import Data.Map as M (fromList, Map, lookup)
import Text.Printf

day19C :: ConduitT Int Int (StateT Int IO) [Int]
day19C = readICPFromC "./data/Day19-input.txt" >>= interpretC

day19Part1 n = fmap fst <$> forM [(x, y) | x <- [0..n-1], y <- [0..n-1]] deploy
  where
    deploy (x, y) = run $ (yield x >> yield y) .| void day19C .| sumC

day19Part2' n = fmap fst <$> forM [(x, y) | x <- [0..n-1], y <- [0..n-1]] deploy
  where
    deploy (x, y) = run $ (yield x >> yield y) .| void day19C .| do
      s <- sumC
      return ((x, y), s)

visualizeInConsole n beams = let bmmap = fromList beams :: Map (Int, Int) Int in
  forM_ [0..n-1] (\y -> (forM [0..n-1] (\x -> case M.lookup (x, y) bmmap of
                                                Just 1 -> putChar '#'
                                                _      -> putChar '.')) >> putChar '\n')

slopeRange beam =
  let slopes = fmap (\((x,y),_) -> y % x) . filter ((0/=).snd.fst) $ filter ((1==).snd) beam in
  (minimum slopes, maximum slopes)

-- min: 34 % 25, max: 46 % 29
-- min: 53 % 39, max: 73 % 46
-- min: 125 % 92,max: 100 % 63

day19Part2'' n = fmap fst <$> forM [(x, y) | y <- [0..n-1], x <- [floor (fromIntegral y*1.35)..ceiling (fromIntegral y*1.59)]] deploy
  where
    deploy (x, y) = run $ (yield x >> yield y) .| void day19C .| do
      s <- sumC
      return ((x, y), s)


visualize beams = display (InWindow "Day19" (800,800) (0,0)) black (rendered beams)
  where
    rendered beams = Pictures (uncurry draw <$> beams)
    draw (x, y) r =
      let fx = fromIntegral x
          fy = fromIntegral y in
        scale 1 (negate 1) $
        if r == 1
        then color (makeColorI 255 215 0 255) $
             line [(0,0), (fx, fy)]
        else color (greyN 0.5) $
             translate fx fy   $
             circle 0.5

-- 100 % 63 ~ x % 100 -> x = 159
-- maxx - minx = 100 + 159
-- (maxx - minx) / y = 1.59 - 1.35
-- y = 259 / 0.24 = 1079

rectangle beams = let ys = (snd.fst) <$> filter ((1==).snd) beams in
  (minimum ys, maximum ys)

check ipc xs ys = fmap fst <$> forM [(x, y) | x <- xs, y <- ys ] deploy
  where
    deploy (x, y) = run $ (yield x >> yield y) .| void day19C .| do
      Just s <- await
      return ((x, y), s)

day19Part2 = do
  !ipc <- readICPFrom "./data/Day19-input.txt"
  forM [1500..1550] $ \x -> do
   (_, maxY) <- rectangle <$> check ipc [x]    [floor (0.63*fromIntegral x)     ..ceiling (0.74*fromIntegral x)]
   (minY, _) <- rectangle <$> check ipc [x+99] [floor (0.63*fromIntegral (x+99))..ceiling (0.74*fromIntegral (x+99))]
   if (maxY - minY == 99)
     then printf "found %s\n" (show (x, minY))
     else return () --print (x, (minY, maxY))
