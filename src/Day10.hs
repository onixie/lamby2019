module Day10 where

import Data.List
import Data.Function
import Text.Printf


data Asteroid = Ast { x :: Int, y :: Int } deriving (Show, Eq, Ord)

readAOS :: IO [String]
readAOS = lines <$> readFile "data/Day10-input.txt"

toAsteroids :: [String] -> [Asteroid]
toAsteroids aos = [ ast | x <- [0..length (head aos)-1], y <- [0..length aos-1], let ast = Ast x y, aos !! y !! x == '#' ]

roundToStr :: Int -> Double -> String
roundToStr n f = printf (printf "%%0.%df" n) f -- hacky way to make floating point eql

vec ast1@Ast{x=x1,y=y1} ast2@Ast{x=x2,y=y2} = (x2-x1, y2-y1)

normal :: (Int, Int) -> (String, String)
normal vec@(x,y) = let n = sqrt (fromIntegral (x*x+y*y)) in (roundToStr (10::Int) (fromIntegral x/n :: Double), roundToStr (10::Int) (fromIntegral y/n ::Double))

sight = do
   asts <- toAsteroids <$> readAOS
   return $ comp asts []
  where
   comp [] bs     = []
   comp (a:as) bs = (a, (numOfUniq $ map (normal . vec a) (as++bs))) : comp as (a:bs)
   numOfUniq = length . map head . group . sort

day10Part1 = sight >>= return . maximumBy (compare `on` snd)

station   = Ast 11 11
asteroids = delete station . toAsteroids <$> readAOS

cartToPolar (x, y) = (if ro - 90 > 0 then ro - 90 else 360 + ro - 90, r2)
  where ro = 360 - (atan2 (fromIntegral y) (fromIntegral x) + quad) * 180 / pi
        r2  = x * x + y * y
        quad = if y > 0 then 0 else 2*pi

ast = asteroids >>= return . map (\a -> (a, cartToPolar $ vec station a))

numOfUniqByRo   = ast >>= return . length . map head . groupBy ((==) `on` (fst . snd)) . sortBy (compare `on` (fst . snd))
numOfUniqByNorm = asteroids >>= return . map (normal . vec station) >>= return . length .map head . group . sort

test = (==) <$> numOfUniqByNorm <*>  numOfUniqByRo

gast = ast >>= return . map (sortBy (compare `on` (snd . snd)) ) . groupBy ((==) `on` (fst . snd)) . sortBy (compare `on` (fst . snd))

sgast = gast >>= return . map (zipWith (\deg (ast,(ro,r)) -> (ast,(ro - fromIntegral deg,r))) [deg | i <-[0..], let deg = i*360])

fsgast = sgast >>= return . reverse . sortBy (compare `on` (fst . snd)) . concat
