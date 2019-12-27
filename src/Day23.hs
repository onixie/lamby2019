{-# LANGUAGE BangPatterns #-}
module Day23 where

import Conduit
import Control.Monad.State (StateT, void, when, forM_)
import Day5 (readICPFrom)
import Day7 (readICPFromC)
import Day9 (interpretC, run)
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as STM
import Data.Maybe

computer :: [Int] -> Int -> STM.TBQueue Int -> ConduitT Int Int (StateT Int IO) [Int]
computer (!ipc) addr nic = (yield addr >> runAt nic) .| interpretC ipc
  where
    runAt :: STM.TBQueue Int -> ConduitT Int Int (StateT Int IO) ()
    runAt nic = do
      p <- liftIO . STM.atomically $ STM.tryReadTBQueue nic
      yield (fromMaybe (negate 1) p)
