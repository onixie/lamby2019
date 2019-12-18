module Day11 where

import Conduit
import Control.Monad
import Control.Monad.State
import Data.Map (Map, empty, fromList, insert, lookup, size, toList)
import Data.Maybe
import Day7 (feedback, readICPFromC)
import Day9 (interpretC, run)
import Prelude hiding (lookup)

import Diagrams.Prelude hiding (size)
import Diagrams.Backend.SVG

data Dir = U | D | L | R deriving (Show, Eq)

day11C :: Integer -> ConduitT () Integer (StateT Integer IO) ()
day11C sp = do
    icp <- readICPFromC "data/Day11-input.txt"
    void $ feedback sp (void (interpretC icp) .| respond U (0, 0) empty)
  where
    respond dir pos@(x,y) map = do
      maybeColor <- await
      maybeMove  <- await
      if isNothing maybeColor || isNothing maybeMove
      then do
        liftIO $ visualize map
        yield . fromIntegral $ size map
      else do
        let (Just c) = maybeColor
        let (Just m) = maybeMove
        let newDir = case dir of
              U -> if m == 0 then L else R
              L -> if m == 0 then D else U
              D -> if m == 0 then R else L
              R -> if m == 0 then U else D
        let newPos = case dir of
              U -> if m == 0 then (x-1, y) else (x+1, y)
              L -> if m == 0 then (x, y-1) else (x, y+1)
              D -> if m == 0 then (x+1, y) else (x-1, y)
              R -> if m == 0 then (x, y+1) else (x, y-1)
        yield $ Data.Maybe.fromMaybe 0 (lookup newPos map)
        respond newDir newPos $ insert pos c map

day11Part1 = fst <$> run (day11C 0 .| await)

visualize m = let points = map (p2 . fst) . filter ((1==).snd) $ toList m in
    renderSVG "./out/Day11-result.svg" (dims2D 500 500) (points `atPoints` repeat (circle 0.2 # fc white) :: Diagram B)
  where
    bound m = (minimum m, maximum m)

day11Part2 = fst <$> run (day11C 1 .| await)
