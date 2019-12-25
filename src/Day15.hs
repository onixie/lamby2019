{-# Language TemplateHaskell #-}
module Day15 where

import Conduit
import Control.Monad
import Control.Monad.State
import Control.Lens hiding ((#))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Char (toUpper)
import Data.Map (Map, empty, fromList, insert, lookup, size, toList)
import Data.Maybe
import Data.IORef
import Data.List (sort, group)
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

data Move = North | South | West | East | Stay deriving (Show, Eq)
data Status = Blocked | Moved | Found  deriving (Show, Eq)

data Droid = Droid { _movement :: Maybe Move, _position :: Position, _map :: Map Position Object, _lastStatus :: Maybe Status, _fly :: Bool } deriving (Show, Eq)
makeLenses ''Droid

rcp :: ConduitT Move (Maybe Status) (StateT Integer IO) ()
rcp = do
    icp <- readICPFromC "data/Day15-input.txt"
    accept .| void (interpretC icp) .| report
  where
    accept = awaitForever $ \m -> do
      --liftIO (print m)
      yield $ moveToIntCode m
    report = awaitForever $ \ c -> do
      let status = intCodeToStatus c
      --liftIO $ print status
      yield (Just status)

day15C = do
  liftIO $ hSetBuffering stdin NoBuffering

  let initDroid = Droid Nothing (0,0) empty Nothing False

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
  feedback Nothing $ control 0 key report initDroid .| rcp

control n key report droid = do
  d <- update droid
  m <- lookAround d
  let newDroid = d & map .~ m

  when (newDroid ^. lastStatus /= Nothing) . liftIO . atomically $ writeTBQueue report newDroid

  let n' = if newDroid ^. lastStatus == Just Moved && newDroid ^. movement /= Just Stay then n+1 else n
  liftIO $ printf "%s moves\n" (show n')

  move key report newDroid >>= control n' key report

update droid = do
  if droid ^. movement == Just Stay
  then return droid
  else do status <- await
          case fromMaybe Nothing status of
            Nothing      -> return $ updateMap   droid Origin
            Just Moved   -> return $ updateMap   (updatePos                    (droid & lastStatus .~ Just Moved  )) Space
            Just Found   -> return $ updateMap   (updatePos                    (droid & lastStatus .~ Just Found  )) OxygenSystem
            Just Blocked -> return $ updateMapAt (updatePos droid ^. position) (droid & lastStatus .~ Just Blocked)  Wall

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
  if droid ^. lastStatus == Just Moved && not (isNothing (droid ^. movement) || droid ^. movement == Just Stay) && droid ^. fly
  then yield (droid ^?! movement.folded) >> return droid
  else do
    (k, s) <- liftIO . atomically $ readTBQueue key -- k <- liftIO getChar
    when (k == 'f') $ fillOxygen droid report >>= liftIO . print

    let (m, f) = keyToMove $ if s == GG.Down then toUpper k else k
    when (m /= Just Stay) $ yield (m ^?! folded)
    return $ droid & movement .~ m
                   & fly      .~ f

autoMove droid =
  case droid ^. lastStatus of
    Just Found   -> return droid
    _ -> do --liftIO $ visualize droid
            m <- liftIO (pick ([North, South, East, West] ++ concat (replicate 5 (droid ^.. movement . traversed))))
            yield m
            return (droid & movement .~ Just m)

lookAround d = tryMove North d
           >>= tryMove South
           >>= tryMove East
           >>= tryMove West
           >>= return . (^. map)

tryMove move droid = yield move >> stepBack (droid & movement .~ Just move)
  where
    stepBack droid = do
      droid <- update droid
      case droid ^. lastStatus of
        Just Blocked -> return droid
        _            -> case droid ^. movement of
                          Just North -> yield South >> update (droid & movement .~ Just South)
                          Just South -> yield North >> update (droid & movement .~ Just North)
                          Just West  -> yield East  >> update (droid & movement .~ Just East)
                          Just East  -> yield West  >> update (droid & movement .~ Just West)
                          Just Stay  -> return droid

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

moveToIntCode :: Move -> Int
moveToIntCode North = 1
moveToIntCode South = 2
moveToIntCode West  = 3
moveToIntCode East  = 4
moveToIntCode Stay  = undefined

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
keyToMove _   = (Just Stay , False)

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
  ,(p G.translate (G.color G.cyan       $ G.rectangleSolid 1 1)) <$> listOf' Oxygen
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
      liftIO $ printf "%d mins\n" n
      let newOS = fmap head . group . sort .
                  filter (\o -> d ^. map . at o /= Just Wall
                             && d ^. map . at o /= Just Oxygen
                             && d ^. map . at o /= Nothing) $ concatMap tryFill os
      let newD  = foldl  (\d o -> d & map . at o .~ Just Oxygen) d newOS
      liftIO . atomically $ writeTBQueue report newD
      liftIO $ threadDelay 5000
      fillMore (n+1) newD newOS
    tryFill (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
