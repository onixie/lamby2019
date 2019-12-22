{-# Language TemplateHaskell #-}
module Day15 where

import Conduit
import Control.Monad
import Control.Monad.State
import Control.Lens hiding ((#))

import Data.Map (Map, empty, fromList, insert, lookup, size, toList)
import Data.Maybe
import Day7 (feedback, readICPFromC)
import Day9 (interpretC, run)
import Day13 (toD)
import Prelude hiding (lookup, Empty, map)
import Text.Printf
import Diagrams.Prelude as DP hiding (size, Empty, Start, position)
import Diagrams.Backend.SVG
--import Diagrams.Backend.Rasterific

import System.IO
import System.Random

data Object = Origin | Space | Wall | OxygenSystem deriving (Show, Eq)
type Position = (Integer, Integer)

data Move = North | South | West | East deriving (Show, Eq)
data Status = Start | Blocked | Moved | Found  deriving (Show, Eq)

instance PrintfArg Status where
  formatArg x fmt | fmtChar (vFmt 'S' fmt) == 'S' =
    formatString (show x) (fmt { fmtChar = 's', fmtPrecision = Nothing })
  formatArg _ fmt = errorBadFormat $ fmtChar fmt

data Droid = Droid { _movement :: Maybe Move, _position :: Position, _map :: Map Position Object, _lastStatus :: Status, _fly :: Bool } deriving (Show, Eq)
makeLenses ''Droid

rcp :: ConduitT Move Status (StateT Integer IO) ()
rcp = do
    icp <- readICPFromC "data/Day15-input.txt"
    accept .| void (interpretC icp) .| report
  where
    accept = awaitForever $ \cmd ->
      do liftIO (print cmd)
         yield $ case cmd of
           North -> 1 :: Int
           South -> 2
           West  -> 3
           East  -> 4
    report = awaitForever $ \ code -> do
      let status = case (code :: Int) of
                     0 -> Blocked -- Hit a wall
                     1 -> Moved   -- Move 1 step in the requested direction
                     2 -> Found   -- Move 1 step in the requested direction; find the oxygen system
      liftIO $ print status
      yield status

visualize :: Droid -> IO ()
visualize d = renderSVG "./out/Day15-result.svg" (dims2D 500 500) (scene :: Diagram B)
  where
    bound m = (minimum m, maximum m)
    listOf obj = fmap (p2 . toD . fst) . filter (( obj==).snd) $ toList (d ^. map)
    scene      = listOf Wall `atPoints` repeat (square 1 # fc orange)
              <> listOf Origin `atPoints` repeat (circle 0.2 # fc yellow)
              <> listOf OxygenSystem `atPoints` repeat (circle 0.2 # fc blue)
              <> [p2 (toD (d ^. position))] `atPoints` repeat (circle 0.2 # fc red)

day15C = do
  liftIO $ hSetBuffering stdin NoBuffering
  feedback Start $ control (Droid Nothing (0,0) empty Start True) 0 .| rcp
  where
    control droid n = do
      d   <- sync droid
      let n' = if d ^. lastStatus == Blocked then n else n+1
      liftIO $ print n'
      d'  <- tryMove (Just North) d >>= tryMove (Just South) >>= tryMove (Just East) >>= tryMove (Just West)
      d'' <- move (d & map .~ d' ^. map)
      if droid ^. lastStatus == Found
      then return ()
      else control d'' n'

    sync droid = do
      if isNothing (droid ^. movement) && (droid ^. lastStatus /= Start)
      then return droid
      else do status <- await
--              liftIO $ print status
              case status of
                Just Start   -> return $ updateMap Origin (droid & lastStatus .~ Start)
                Just Moved   -> return $ updatePos (droid & lastStatus .~ Moved)
                Just Found   -> return . updateMap OxygenSystem $ updatePos (droid & lastStatus .~ Found)
                Just Blocked -> return . updateMapRaw Wall (droid & lastStatus .~ Blocked) $ updatePos droid ^. position

    updatePos droid = case droid ^. movement of
      Just North -> droid & position . _2 %~ (+1)
      Just South -> droid & position . _2 %~ (subtract 1)
      Just West  -> droid & position . _1 %~ (subtract 1)
      Just East  -> droid & position . _1 %~ (+1)
      Nothing    -> droid
    updateMap    obj droid     = droid & map %~ (insert (droid ^. position) obj)
    updateMapRaw obj droid pos = droid & map %~ (insert pos obj)
    move droid = do
      liftIO $ visualize droid
      if droid ^. lastStatus == Moved && not (isNothing (droid ^. movement)) && droid ^. fly
      then yield (droid ^?! movement.folded) >> return droid
      else do
        c <- liftIO getChar
        let (m,f) = case c of
                  'j' -> (Just West , False)
                  'k' -> (Just South, False)
                  'l' -> (Just East , False)
                  'i' -> (Just North, False)
                  'J' -> (Just West , True)
                  'K' -> (Just South, True)
                  'L' -> (Just East , True)
                  'I' -> (Just North, True)
                  _   -> (Nothing, False)
        unless (isNothing m) $ yield (m^?!folded)
        return (droid & movement .~ m & fly .~ f)
    autoMove droid =
      case droid ^. lastStatus of
        Found   -> return droid
        _ -> do --liftIO $ visualize droid
                m <- liftIO (pick ([North, South, East, West] ++ concat (replicate 5 (droid ^.. movement . traversed))))
                yield m
                return (droid & movement .~ Just m)
    tryMove (Just move) droid = yield move >> tryMove Nothing (droid & movement .~ Just move)
    tryMove Nothing     droid = do
        droid <- sync droid
        case droid ^. lastStatus of
          Moved   -> case droid ^. movement of
                       Just North -> yield South >> sync (droid & movement .~ Just South)
                       Just South -> yield North >> sync (droid & movement .~ Just North)
                       Just West  -> yield East  >> sync (droid & movement .~ Just East)
                       Just East  -> yield West  >> sync (droid & movement .~ Just West)
                       Nothing    -> undefined
          _ -> return droid
pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

day15Part1 = run $ day15C .| awaitForever return
