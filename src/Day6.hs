{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Day6 where

import Text.ParserCombinators.ReadP
import Data.Tree
import Control.Monad.IO.Class

orbit  = char ')'
object = munch (\x -> x/=')' && x/='\n')
uomap = ((,) <$> (object <* orbit) <*> object) `sepBy` char '\n'

inner = fst
outer = snd

readMap = fst . last . readP_to_S uomap <$> readFile "data/Day6-input.txt"

type UOTreeBuilder = forall m. MonadIO m => [(String, String)] -> m (Tree (String , Integer))

buildFrom root dir uomap = unfoldTreeM (flip dir uomap) (root,0)

obj1 `to` obj2 = \(obj, lv) -> return . ((obj, lv), ) . map ((,lv+1) . obj2) . filter ((obj==) . obj1)

fromCOM :: UOTreeBuilder
fromCOM = buildFrom "COM" (inner `to` outer)

fromSAN :: UOTreeBuilder
fromSAN = buildFrom "SAN" (outer `to` inner)

fromYOU :: UOTreeBuilder
fromYOU = buildFrom "YOU" (outer `to` inner)

visualize = putStrLn . drawTree . fmap show

drawFromCOM = readMap >>= fromCOM >>= visualize

day6Part1 = readMap >>= fromCOM >>= return . sum . fmap snd . flatten

day6Part2 = do
    uomap <- readMap
    treeFromSAN <- fromSAN uomap >>= return . flatten
    treeFromYOU <- fromYOU uomap >>= return . flatten
    --print (reverse treeFromSAN)
    --print (reverse treeFromYOU)
    return $ findJoin (reverse treeFromSAN) (reverse treeFromYOU)
  where
    findJoin ((x,n):xs) ((y,m):ys)
      | x == y = findJoin xs ys
      | otherwise = n+m
