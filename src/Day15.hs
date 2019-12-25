{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyCase #-}
{-# Language TemplateHaskell #-}
module Day15 where

import Conduit
import Control.Monad
import Control.Monad.State
import Control.Lens hiding ((#))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.Char (toUpper)
import Data.Map (Map, empty, fromList, insert, lookup, size, toList)
import Data.Maybe
import Data.IORef
import Day7 (feedback, readICPFromC)
import Day9 (interpretC, run)
import Day13 (toD)
import Prelude hiding (lookup, Empty, map)
import Text.Printf
import Diagrams.Prelude as DP hiding (size, Empty, Start, position, at)
import Diagrams.Backend.SVG
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Game as GG
import qualified Graphics.Gloss.Data.ViewPort as GV

--import Diagrams.Backend.Rasterific

import System.IO
import System.Random

makePrisms ''GG.Event
makePrisms ''GG.Key

data Object = Origin | Space | Wall | OxygenSystem | Oxygen deriving (Show, Eq)

instance Semigroup Object where
  x <> Space = x
  Space <> x = x
  x <> y = y

instance Monoid Object where
  mempty = Space

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
    accept = awaitForever $ \m ->
      do liftIO (print m)
         yield $ moveToIntCode m
    report = awaitForever $ \ c -> do
      let status = intCodeToStatus c
      liftIO $ print status
      yield status

day15C = do
  liftIO $ hSetBuffering stdin NoBuffering

  let initDroid = Droid Nothing (0,0) empty Start False

  key    <- liftIO $ newTBQueueIO 1
  report <- liftIO $ newTBQueueIO 1

  -- UI
  liftIO . forkIO $ GG.playIO (G.InWindow "Day15" (800, 800) (200,200)) G.white 100
    initDroid (return . draw)
    (\ e w -> do when (e ^? _EventKey . _2 == Just GG.Down) . liftIO . atomically $
                   writeTBQueue key (fromMaybe ' '   (e ^? _EventKey . _1 . _Char),
                                     fromMaybe GG.Up (GG.shift <$> e ^? _EventKey . _3))
                 return w
    )
    (\ t w -> do newDroid <- liftIO . atomically $ tryReadTBQueue report
                 return $ if isNothing newDroid then w else fromJust newDroid
    )

  -- logic
  feedback Start $ control 0 key report initDroid .| rcp

control n key report droid = do
  d <- update droid
  m <- lookAround d
  let newDroid = d & map .~ m

  unless (d ^. lastStatus == Start) . liftIO . atomically $ writeTBQueue report newDroid

  let n' = if newDroid ^. lastStatus == Blocked then n else n+1
  liftIO $ print n'

  if droid ^. lastStatus == Found
  then return ()
  else move key report newDroid >>= control n' key report

update droid = do
  if (isNothing (droid ^. movement) && (droid ^. lastStatus /= Start)) || droid ^. lastStatus == Found
  then return droid
  else do status <- await
          case status of
            Just Start   -> return $ updateMap                                 (droid & lastStatus .~ Start  )  Origin
            Just Moved   -> return $ updateMap   (updatePos                    (droid & lastStatus .~ Moved  )) Space
            Just Found   -> return $ updateMap   (updatePos                    (droid & lastStatus .~ Found  )) OxygenSystem
            Just Blocked -> return $ updateMapAt (updatePos droid ^. position) (droid & lastStatus .~ Blocked)  Wall
            _            -> return droid

updatePos droid  = case droid ^. movement of
  Just East  -> droid & position . _1 %~ (+1)
  Just North -> droid & position . _2 %~ (+1)
  Just West  -> droid & position . _1 %~ (subtract 1)
  Just South -> droid & position . _2 %~ (subtract 1)
  _          -> droid

updateMap   droid         = updateMapAt (droid ^. position) droid
updateMapAt pos droid obj = if isNothing (droid ^. map . at pos)
                            then droid & map . at pos .~ (Just obj)
                            else droid & map . at pos %~ (mappend (Just obj))


move key report droid = do
  liftIO $ visualize droid
  if droid ^. lastStatus == Moved && not (isNothing (droid ^. movement)) && droid ^. fly
  then yield (droid ^?! movement.folded) >> return droid
  else do
    (k, s) <- liftIO . atomically $ readTBQueue key -- k <- liftIO getChar
    when (k == 'f') $ fillOxygen droid report >>= liftIO . print

    let (m, f) = keyToMove (if s == GG.Down then toUpper k else k)

    if droid ^. lastStatus == Found
    then return (droid & movement .~ Nothing)
    else do unless (isNothing m) $ yield (m ^?! folded)
            return (droid & movement .~ m & fly .~ f)

autoMove droid =
  case droid ^. lastStatus of
    Found   -> return droid
    _ -> do --liftIO $ visualize droid
            m <- liftIO (pick ([North, South, East, West] ++ concat (replicate 5 (droid ^.. movement . traversed))))
            yield m
            return (droid & movement .~ Just m)

lookAround d = if d ^. lastStatus == Found then return (d^. map)
           else tryMove (Just North) d
           >>=  tryMove (Just South)
           >>=  tryMove (Just East )
           >>=  tryMove (Just West )
           >>=  return . (^. map)

tryMove (Just move) droid = yield move >> tryMove Nothing (droid & movement .~ Just move)
tryMove Nothing     droid = do
    droid <- update droid
    case droid ^. lastStatus of
      Moved   -> case droid ^. movement of
                   Just North -> yield South >> update (droid & movement .~ Just South)
                   Just South -> yield North >> update (droid & movement .~ Just North)
                   Just West  -> yield East  >> update (droid & movement .~ Just East)
                   Just East  -> yield West  >> update (droid & movement .~ Just West)
                   Nothing    -> undefined
      _ -> return droid

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

moveToIntCode :: Move -> Int
moveToIntCode North = 1
moveToIntCode South = 2
moveToIntCode West  = 3
moveToIntCode East  = 4

intCodeToStatus :: Int -> Status
intCodeToStatus 0 = Blocked
intCodeToStatus 1 = Moved
intCodeToStatus 2 = Found

keyToMove 'j' = (Just West , False)
keyToMove 'k' = (Just South, False)
keyToMove 'l' = (Just East , False)
keyToMove 'i' = (Just North, False)
keyToMove 'J' = (Just West , True)
keyToMove 'K' = (Just South, True)
keyToMove 'L' = (Just East , True)
keyToMove 'I' = (Just North, True)
keyToMove _   = (Nothing   , False)

visualize :: Droid -> IO ()
visualize d = renderSVG "./out/Day15-result.svg" (dims2D 500 500) (scene :: Diagram B)
  where
    listOf' = fmap (p2 . toD) . flip listOf d
    scene   = listOf' Wall                `atPoints` repeat (square 1   # fc orange)
           <> listOf' Origin              `atPoints` repeat (circle 0.2 # fc yellow)
           <> listOf' OxygenSystem        `atPoints` repeat (circle 0.2 # fc blue)
           <> [p2 (toD (d ^. position))]  `atPoints` repeat (circle 0.2 # fc red)

draw d = GV.applyViewPortToPicture vp . G.Pictures $ concat
  [(p G.translate (G.color G.orange     $ G.rectangleSolid 1 1)) <$> listOf' Wall
  ,(p G.translate (G.color(G.greyN 0.95)$ G.rectangleSolid 1 1)) <$> listOf' Space
  ,(p G.translate (G.color G.green      $ G.circle 0.2))         <$> listOf' Origin
  ,(p G.translate (G.color G.aquamarine $ G.circle 0.2))         <$> listOf' OxygenSystem
  ,(p G.translate (G.color G.red        $ G.circle 0.2))         <$> [toD (d^.position)]]
  where
    listOf' = fmap toD . flip listOf d
    p = flip . uncurry
    vp = GV.viewPortInit { GV.viewPortScale = 400 / fromInteger (max mx my), GV.viewPortTranslate = toD (d ^. position) & both %~ negate }
    (mx, my) = case bound d of
       (Just x1, Just y1, Just x2, Just y2) -> (x2 - x1, y2 - y1)
       _ -> (1, 1)

listOf obj droid = fmap fst . filter ((obj==).snd) $ toList (droid ^. map)
bound d = (minimumOf (folded._1._2) (toList (d ^. map)), minimumOf (folded._1._1) (toList (d ^. map)), maximumOf (folded._1._2) (toList (d ^. map)), maximumOf (folded._1._1) (toList (d ^. map)))

day15Part1 = run $ day15C .| awaitForever return

fillOxygen droid report = do
    fillMore (0::Int) droid $ listOf OxygenSystem droid
  where
    fillMore n d [] = return n
    fillMore n d os = do
      let newOS = filter (\o -> droid ^. map . at o /= Just Wall) $ concatMap tryFill os
      let newD  = foldl  (\d o -> d & map . ix o .~ Oxygen) d newOS
      liftIO . atomically $ writeTBQueue report newD
      fillMore (n+1) d newOS
    tryFill (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
