module Day19 where

import Conduit
import Control.Monad.State (StateT, void)
import Control.Monad.Parallel (forM)
import Day7 (readICPFromC)
import Day9 (interpretC, run)


day19C :: ConduitT Int Int (StateT Int IO) [Int]
day19C = readICPFromC "./data/Day19-input.txt" >>= interpretC

day19Part1 n = fmap fst <$> forM [(x, y) | x <- [0..n-1], y <- [0..n-1]] deploy
  where
    deploy (x, y) = run $ (yield x >> yield y) .| void day19C .| sumC
