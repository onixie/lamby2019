{-# LANGUAGE FlexibleContexts #-}
module Day17 where

import Conduit
import Control.Monad.State
import Day7 (readICPFromC)
import Day9 (interpretC, run)
import Control.Lens
import Data.Array.Repa
import Data.List (group, sort, minimumBy, maximumBy)
import Data.Function (on)

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

data Dir = N | S | W | E deriving (Show, Eq, Ord)
toDir 94  = N
toDir 118 = S
toDir 62  = E
toDir 60  = W

toStepper N = (id, (subtract 1))
toStepper S = (id, (+1))
toStepper E = ((+1), id)
toStepper W = ((subtract 1), id)

data Turn = L | R deriving (Show, Ord, Eq)
turn L N = W
turn R N = E
turn L W = S
turn R W = N
turn L S = E
turn R S = W
turn L E = N
turn R E = S

data Move = T Turn Dir | F Int (Int, Int) deriving (Eq, Ord)

instance Show Move where
  show (T t _) = show t
  show (F s _) = show s

findRobot map = let Z :. r :. c = extent map in
  [((x, y), toDir c) | x <- [0..c-1]
                     , y <- [0..r-1]
                     , let c = map ! (Z :. y :. x)
                     , any (c==) [94, 62, 60, 118]
  ]

steps pos stepper map = steps' pos stepper map 0
  where
    Z :. r :. c = extent map
    steps' (x, y) s@(sx, sy) map n
      | sx x < 0 || sx x >= c || sy y < 0 || sy y >= r = ((x, y), n)
      | otherwise = if map ! (Z :. sy y :. sx x) == 35
                    then steps' (sx x, sy y) s map (n+1)
                    else ((x, y), n)

buildPath robot@(pos, dir) map =
  let (newPos, s) = steps pos (toStepper dir) map in
    if s /= 0 then F s newPos:buildPath (newPos, dir) map
    else let dir'        = turn L dir
             (newPos, s) = steps pos (toStepper dir') map in
           if s /= 0 then T L dir':F s newPos:buildPath (newPos, dir') map
           else let dir'        = turn R dir
                    (newPos, s) = steps pos (toStepper dir') map in
                  if s /= 0 then T R dir':F s newPos:buildPath (newPos, dir') map
                  else []

day17Part2 = do
  (map, _) <- run captureScaffoldView
  let [robot@(pos, dir)] = findRobot map
  let path = buildPath robot map
  return path

removeMatched [] t = t
removeMatched s  t = case match s t 0 of
  Just p  -> removeMatched s (take p t Prelude.++ drop (p+length s) t)
  Nothing -> t
  where
    match [] _ n = Just n
    match _ [] n = Nothing
    match sss@(s:ss) (t:ts) n = if s == t then match ss ts n else match sss ts (n+1)

findPatterns path =
  let len = length path in
    fmap head . group $ sort [pat
    | s <- [0..len],
      l <- [1..20],
      let pat = drop s $ take l path,
      let newPath = removeMatched pat path,
      newPath /= path
    ]

findMainR path =
  let pats = findPatterns path
      len  = length pats - 1in
  [ (pats!!i, pats!!j, pats!!k)
  | i <- [0..len]
  , j <- [i..len]
  , k <- [j..len]
  , ([]==) . removeMatched (pats!!k) . removeMatched (pats!!j) . removeMatched (pats!!i) $ path
  ]

-- R,6,R,6,R,8,L,10,L,4
-- R,6,L,10,R,8
-- L,4,L,4,L,12
