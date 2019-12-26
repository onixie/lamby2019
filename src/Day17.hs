{-# LANGUAGE FlexibleContexts #-}
module Day17 where

import Conduit
import Control.Monad.State
import Day7 (readICPFromC)
import Day9 (interpretC, run)
import Control.Lens
import Data.Array.Repa

day7C :: ConduitT Int Int (StateT Int IO) [Int]
day7C = readICPFromC "data/Day17-input.txt" >>= interpretC

viewScaffold :: ConduitT Int Int (StateT Int IO) ()
viewScaffold = awaitForever $ \v -> do
    liftIO $ putChar (v ^. enum)
    yield v

captureScaffoldView :: ConduitT i Void (StateT Int IO) (Array U DIM2 Int)
captureScaffoldView = do
  sv <- yield 0 .| void day7C .| viewScaffold .| sinkList
  let (row, col) = getDim sv
  return $ fromListUnboxed (Z :. row :. col :: DIM2) (sv ^.. traversed.filtered (/=10))
  where
    getDim sv =
      let lf = sv ^.. folded.filtered (==10) . withIndex in
        (length lf-1, head lf ^. _1)

day17Part1 = do
  (arr,_) <- run captureScaffoldView
  let Z :. r :. c = extent arr
  let aps = [(x, y) | x <- [1..c-2], y <- [1..r-2], isIntersection (x, y) arr ]
  return . sum $ aps & traversed %~ uncurry (*)
  where
    isIntersection (x, y) arr =
      all (==35) [arr ! (Z :. y   :. x  )
                 ,arr ! (Z :. y+1 :. x  )
                 ,arr ! (Z :. y-1 :. x  )
                 ,arr ! (Z :. y   :. x+1)
                 ,arr ! (Z :. y   :. x-1)
                 ]
