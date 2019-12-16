module Day10 where

import Data.List
import Text.Printf
data Asteroid = Ast { x :: Int, y :: Int } deriving (Show, Eq, Ord)

readAOS :: IO [String]
readAOS = lines <$> readFile "data/Day10-input.txt"

asteroids :: [String] -> [Asteroid]
asteroids aos = [ ast | x <- [0..length (head aos)-1], y <- [0..length aos-1], let ast = Ast x y, aos !! y !! x == '#' ]

roundToStr :: Int -> Double -> String
roundToStr n f = printf (printf "%%0.%df" n) f -- hacky way to make floating point eql

vec ast1@Ast{x=x1,y=y1} ast2@Ast{x=x2,y=y2} = (x1 - x2, y1 - y2)

normal :: (Int, Int) -> (String, String)
normal vec@(x,y) = let n = sqrt (fromIntegral (x*x+y*y)) in (roundToStr (10::Int) (fromIntegral x/n :: Double), roundToStr (10::Int) (fromIntegral y/n ::Double))

sight = do
   aos <- asteroids <$> readAOS
   return $ comp aos []
  where
   comp [] bs     = []
   comp (a:as) bs = (numOfUniq $ map (normal . vec a) (as++bs)) : comp as (a:bs)
   numOfUniq = length . map head . group . sort
