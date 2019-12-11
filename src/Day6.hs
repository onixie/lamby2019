{-# LANGUAGE TupleSections #-}
module Day6 where

import Text.ParserCombinators.ReadP
import Data.Tree
import Control.Monad.IO.Class

orbit  = char ')'
object = munch (\x -> x/=')' && x/='\n')

uomap = ((,) <$> (object <* orbit) <*> object) `sepBy` lf
  where lf = char '\n'

readMap = last . readP_to_S uomap <$> readFile "data/Day6-sample.txt"

orbitedBy (obj, lv) = return . ((obj, lv), ) . map ((,lv+1) . snd) . filter ((obj==) . fst)

buildFrom root uomap = unfoldTreeM (flip orbitedBy uomap) (root,0)

visual = readMap >>= buildFrom "COM" . fst >>= putStrLn . drawTree . fmap show

day6Part1 = readMap >>= buildFrom "COM" . fst >>= return . sum . fmap snd . flatten
