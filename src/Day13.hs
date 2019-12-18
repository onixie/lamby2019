module Day13 where

import Conduit
import Control.Monad
import Control.Monad.State
import Data.Map (Map, empty, fromList, insert, lookup, size, toList)
import Data.Maybe
import Day7 (feedback, readICPFromC)
import Day9 (interpretC, run)
import Prelude hiding (lookup, Empty)

import Diagrams.Prelude as DP hiding (size, Empty)
import Diagrams.Backend.SVG

data Tile = Empty | Wall | Block | Paddle | Ball deriving (Show, Eq)
type Position = (Double, Double)
type Game = Map Position Tile

day13C ::  ConduitT Integer Void (StateT Integer IO) Game
day13C = do
    icp <- readICPFromC "data/Day13-input.txt"
    void (interpretC icp) .| play empty
  where
    play :: Map Position Tile -> ConduitT Integer Void (StateT Integer IO) Game
    play game = do
      maybeX   <- await
      maybeY   <- await
      maybeTID <- await
      if isNothing maybeX || isNothing maybeY || isNothing maybeTID
      then return game
      else do
        let (Just x)   = maybeX
        let (Just y)   = maybeY
        let (Just tid) = maybeTID

        play $ insert (fromIntegral x, fromIntegral y) (case tid of
          0 -> Empty
          1 -> Wall
          2 -> Block
          3 -> Paddle
          4 -> Ball) game

day13CPart1 = do
  (game,_) <- run $  yield 0 .| day13C
  return . length . filter ((Block==).snd) $ toList game

visualize m = renderSVG "./out/Day13-result.svg" (dims2D 500 500) (rotate halfTurn scene :: Diagram B)
  where
    bound m = (minimum m, maximum m)
    listOf obj = map (p2 . fst) . filter (( obj==).snd) $ toList m
    scene   = listOf Block  `atPoints` repeat (square 0.5 # fc pink # scaleX 1.5)
           <> listOf Wall   `atPoints` repeat (square 1   # fc orange)
           <> listOf Ball   `atPoints` repeat (circle 0.2 # fc blue)
           <> listOf Paddle `atPoints` repeat (square 0.5 # fc white # scaleX 1.5)

draw = run (yield 0 .| day13C) >>= \(g,_) -> visualize g
