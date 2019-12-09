module Day3 where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.List

data Move = R Int | L Int | U Int | D Int deriving (Show)
type Path = [Move]

move dir = read <$> (char dir *> munch isDigit)

path = choice [ U <$> move 'U'
              , D <$> move 'D'
              , L <$> move 'L'
              , R <$> move 'R'] `sepBy` char ','

readInput :: IO (Path, Path)
readInput = do
  (w1, ""):(w2, ""):_ <- readFile "data/Day3-input.txt" >>= return . fmap (last . readP_to_S path) . lines
  return (w1, w2)

step (R m) = m
step (L m) = m
step (U m) = m
step (D m) = m
moveSoFar [] acc     = []
moveSoFar (m:ms) acc = let newAcc = step m + acc in newAcc : moveSoFar ms newAcc

data Seg = H (Int, Int) Int | V Int (Int, Int) deriving (Show)
type AbsPath = [Seg]

moveToSeg :: Seg -> Move -> Seg
moveToSeg (H (beg, end) pos) (L m) = H (end, end - m) pos
moveToSeg (H (beg, end) pos) (R m) = H (end, end + m) pos
moveToSeg (H (beg, end) pos) (U m) = V end (pos, pos + m)
moveToSeg (H (beg, end) pos) (D m) = V end (pos, pos - m)
moveToSeg (V pos (beg, end)) (L m) = H (pos, pos - m) end
moveToSeg (V pos (beg, end)) (R m) = H (pos, pos + m) end
moveToSeg (V pos (beg, end)) (U m) = V pos (end, end + m)
moveToSeg (V pos (beg, end)) (D m) = V pos (end, end - m)

toAbsPath path = move (H (0,0) 0) path
  where
    move seg [] = []
    move seg (p:path) = let nextSeg = moveToSeg seg p in nextSeg : move nextSeg path

bet beg pos end = min beg end <= pos && pos <= max beg end

segIntersect (H (beg1, end1) pos1) (V pos2 (beg2, end2))
  | bet beg1 pos2 end1 && bet beg2 pos1 end2 = [(pos2,pos1, abs(pos2 - beg1)+abs(pos1 - beg2))]
  | otherwise = []
segIntersect v@(V _ _) h@(H _ _) = segIntersect h v

segIntersect (H (beg1, end1) pos1) (H (beg2, end2) pos2)
  | pos1 == pos2 = fmap (\p -> (p, pos1, abs(p - beg1)+abs(p - beg2))) $ intersect [(min beg1 end1)..(max beg1 end1)] [(min beg2 end2)..(max beg2 end2)]
  | otherwise = []
segIntersect (V pos1 (beg1, end1)) (V pos2 (beg2, end2))
  | pos1 == pos2 = fmap (\p -> (pos1, p, abs(p - beg1)+abs(p - beg2))) $ intersect [(min beg1 end1)..(max beg1 end1)] [(min beg2 end2)..(max beg2 end2)]
  | otherwise = []

findPathIntersects p1 p2 = let path1 = toAbsPath p1
                               path2 = toAbsPath p2 in
                             concat [ i | seg1 <- path1, seg2 <- path2, let i = segIntersect seg1 seg2, i /= [] ]

minManhattan :: [(Int, Int, Int)] -> (Int, Int, Int)
minManhattan = minimumBy (\(x1,y1,_) (x2,y2,_) -> (x1+y1) `compare` (x2+y2))

day3Part1 = readInput >>= return . minManhattan . tail . uncurry findPathIntersects

findPathIntersectWithMoveCounts p1 p2 = let path1 = toAbsPath p1
                                            path2 = toAbsPath p2
                                            move1 = moveSoFar p1 0
                                            move2 = moveSoFar p2 0 in
                                          [ (move1 !! (i-1), move2 !! (j-1), s) | i <- [1..length path1 - 1], j <- [1..length path2 - 1], let si = segIntersect (path1!!i) (path2!!j), si /= [], s <- si ]

minMoves :: [(Int, Int, (Int, Int, Int))] -> (Int, Int, (Int, Int, Int))
minMoves = minimumBy (\(i1, j1, (_, _, r1)) (i2, j2, (_, _, r2)) -> (i1+j1+r1) `compare` (i2+j2+r2))

day3Part2 = do
  (l1, l2, (x, y, l3)) <- readInput >>= return . minMoves . tail . uncurry findPathIntersectWithMoveCounts
  return (l1+l2+l3)
