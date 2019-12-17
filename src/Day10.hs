module Day10 where

import Data.List
import Data.Function
import Text.Printf


data Asteroid = Ast { x :: Int, y :: Int } deriving (Show, Eq, Ord)

readAoS :: IO [String]
readAoS = lines <$> readFile "data/Day10-input.txt"

toAsteroids :: [String] -> [Asteroid]
toAsteroids aos = [ ast | x <- [0..length (head aos)-1],
                          y <- [0..length aos-1],
                          let ast = Ast x y, aos !! y !! x == '#' ]

roundToStr :: Int -> Double -> String
roundToStr n f = printf (printf "%%0.%df" n) f -- hacky way to make floating point eql

vec Ast{x=x1,y=y1} Ast{x=x2,y=y2} = (x2-x1, y2-y1)

normal :: (Int, Int) -> (String, String)
normal vec@(x,y) =
  let n = sqrt (fromIntegral (x*x+y*y)) in
    (roundToStr (10::Int) (fromIntegral x/n :: Double), roundToStr (10::Int) (fromIntegral y/n ::Double))

sight = do
   asts <- toAsteroids <$> readAoS
   return $ calcDetects asts []
  where
   calcDetects [] _     = []
   calcDetects (a:as) as' = (a, numOfUniq $ map (normal . vec a) (as++as')) : calcDetects as (a:as')
   numOfUniq = length . group . sort

day10Part1 = maximumBy numOfDetects <$> sight
  where numOfDetects = compare `on` snd

station   = Ast 11 11
asteroids = delete station . toAsteroids <$> readAoS

cartToPolar (x, y) = (if ro - 90 > 0 then ro - 90 else 360 + ro - 90, r2)
  where ro = 360 - (atan2 (fromIntegral y) (fromIntegral x) + quad) * 180 / pi
        r2  = x * x + y * y
        quad = if y > 0 then 0 else 2*pi

asts = withPolarOfLBeamFrom station <$> asteroids
  where
    withPolarOfLBeamFrom s = map (\a -> (a, cartToPolar $ vec s a))

groupBySameRo = groupBy ((==) `on` (fst . snd)) . sortBy (compare `on` (fst . snd))
sortByR = sortBy (compare `on` (snd . snd))

numOfUniqByRo   = length . groupBySameRo <$> asts
numOfUniqByNorm = length . group . sort  <$> (map (normal . vec station) <$> asteroids)

test = (==) <$> numOfUniqByNorm <*>  numOfUniqByRo

astsG = map sortByR . groupBySameRo <$> asts

astsGS = map adjustRo  <$> astsG
  where
    adjustRo = zipWith (\deg (ast, (ro, r)) -> (ast, (ro - fromIntegral deg, r))) [deg | i <-[0..], let deg = i*360]

astsGSF = sortBy roDesc . concat <$> astsGS
  where roDesc = flip compare `on` (fst . snd)

day10Part2 = do
  (Ast{x=x,y=y}, _) <- (!! 199) <$> astsGSF
  return $ x*100+y
