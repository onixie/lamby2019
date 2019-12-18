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
type Score = Integer

day13C :: Bool -> ConduitT Integer Void (StateT Integer IO) (Game, Score)
day13C free = do
    icp <- readICPFromC "data/Day13-input.txt"
    let icp' = if free then 2:drop 1 icp else icp
    void (interpretC icp') .| play empty 0
  where
    play :: Game -> Score -> ConduitT Integer Void (StateT Integer IO) (Game, Score)
    play game score = do
      maybeX   <- await
      maybeY   <- await
      maybeTID <- await

      liftIO $ visualize game
      if isNothing maybeX || isNothing maybeY || isNothing maybeTID
      then return (game, score)
      else do
        let (Just x)   = maybeX
        let (Just y)   = maybeY
        let (Just tid) = maybeTID

        if x == -1 && y == 0
        then play game tid -- tid is the new score
        else play (update game x y tid) score
    update game x y tid =
      (flip.) insert (fromIntegral x, fromIntegral y) game $
        case tid of
          0 -> Empty
          1 -> Wall
          2 -> Block
          3 -> Paddle
          4 -> Ball

visualize m = renderSVG "./out/Day13-result.svg" (dims2D 500 500) (rotate halfTurn scene :: Diagram B)
  where
    bound m = (minimum m, maximum m)
    listOf obj = map (p2 . fst) . filter (( obj==).snd) $ toList m
    scene   = listOf Block  `atPoints` repeat (square 0.5 # fc pink # scaleX 1.5)
           <> listOf Wall   `atPoints` repeat (square 1   # fc orange)
           <> listOf Ball   `atPoints` repeat (circle 0.2 # fc blue)
           <> listOf Paddle `atPoints` repeat (square 0.5 # fc white # scaleX 1.5)

day13CPart1 = do
  ((game,_),_) <- run $ yield 0 .| day13C False
  -- visualize g
  return . length . filter ((Block==).snd) $ toList game

day13CPart2 = do
  ((game,score),_) <- run $ joystick .| day13C True
--  visualize game
  return score
  where
    joystick = do
      c <- liftIO $ getChar
      yield (case c of
        'j' -> negate 1
        'k' -> 0
        'l' -> 1
        _   -> 0)
      joystick
