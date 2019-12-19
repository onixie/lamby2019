module Day13 where

import Conduit
import Control.Monad
import Control.Monad.State
import Control.Concurrent (threadDelay)
import Data.Map (Map, empty, fromList, insert, lookup, size, toList)
import Data.Maybe
import Data.IORef
import Data.Ratio
import Day7 (feedback, readICPFromC)
import Day9 (interpretC, run)
import Prelude hiding (lookup, Empty)

import Diagrams.Prelude as DP hiding (size, Empty)
import Diagrams.Backend.SVG
--import Diagrams.Backend.Rasterific

import System.IO

data Tile = Empty | Wall | Block | Paddle | Ball deriving (Show, Eq)
type Position = (Integer, Integer)
type Game = Map Position Tile
type Score = Integer
type Toranomaki = IORef (Maybe Integer)

day13C :: Bool -> Toranomaki -> ConduitT Integer Void (StateT Integer IO) (Game, Score)
day13C free tora = do
    icp <- readICPFromC "data/Day13-input.txt"
    let icp' = if free then 2:drop 1 icp else icp
    void (interpretC icp') .| play empty Nothing 0
  where
    play :: Game -> Maybe Position -> Score -> ConduitT Integer Void (StateT Integer IO) (Game, Score)
    play game bpos score = do
      maybeX <- await
      maybeY <- await
      maybeZ <- await

      if isNothing maybeX || isNothing maybeY || isNothing maybeZ
      then return (game, score)
      else do

        let (Just x) = maybeX
        let (Just y) = maybeY
        let (Just z) = maybeZ -- tile id or score

        if x == -1 && y == 0  -- update score
        then do
          liftIO $ print score
          play game bpos z
        else do
          let obj = toObj z

          if isNothing bpos || obj /= Ball
          then return ()
          else liftIO . visualize game $ let Just bxy = bpos in
                                           if bxy == (x, y)
                                           then mempty
                                           else arrowBetween (p2 $ toD bxy) (p2 $ toD (x,  y))

          if obj /= Ball && Just (x, y) == bpos -- Skip
          then play game bpos score
          else if obj == Ball && bpos /= Nothing -- update ball, clear prev
          then do
            let Just bxy@(bx,by) = bpos
            liftIO . writeIORef tora . Just $ case x `compare` bx of
              LT -> -1
              GT -> 1
              EQ -> 0
            play (insert (x, y) obj (insert bxy Empty game)) (Just (x, y)) score
          else if obj == Ball && bpos == Nothing -- update ball only
          then do
            liftIO $ writeIORef tora Nothing
            play (insert (x, y) obj game) (Just (x, y)) score
          else play (insert (x, y) obj game) bpos score

toObj tid = case tid of
  0 -> Empty
  1 -> Wall
  2 -> Block
  3 -> Paddle
  4 -> Ball

visualize m btrace = renderSVG "./out/Day13-result.svg" (dims2D 500 500) (rotate halfTurn scene :: Diagram B)
  where
    bound m = (minimum m, maximum m)
    listOf obj = map (p2 . toD . fst) . filter (( obj==).snd) $ toList m
    scene   = listOf Wall   `atPoints` repeat (square 1   # fc orange)
           <> listOf Block  `atPoints` repeat (square 0.5 # fc pink # scaleX 1.5)
           <> listOf Paddle `atPoints` repeat (square 1 # fc white # scaleX 1.5)
           <> listOf Ball   `atPoints` repeat (circle 0.2 # fc blue)
           <> btrace -- # (with & headStyle %~ fc orange . opacity 0.75)

toD (x, y) = (fromIntegral x, fromIntegral y)

day13CPart1 = do
  cheatsheet <- newIORef Nothing
  ((game,_),_) <- run $ yield 0 .| day13C False cheatsheet
  -- visualize g
  return . length . filter ((Block==).snd) $ toList game

day13CPart2 = do
  cheatsheet <- newIORef Nothing
  ((game,score),_) <- run $ joystick cheatsheet .| day13C True cheatsheet
--  visualize game
  return score
  where
    joystick cheatsheet = do
      liftIO $ hSetBuffering stdin NoBuffering
      c <- liftIO getChar
      case c of
        'j' -> yield 1
        'k' -> yield 0
        'l' -> yield $ negate 1
        ' ' -> autoYield cheatsheet
        _   -> return ()
      joystick cheatsheet
    autoYield cheatsheet = do
      maybeCS <- liftIO $ readIORef cheatsheet
      if isNothing maybeCS
      then return ()
      else do
           let (Just cs) = maybeCS
           yield cs
