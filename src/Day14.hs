{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# Language TemplateHaskell #-}

module Day14 where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.Function
import Data.List
import Control.Lens
import Data.Typeable
import Data.Data
import Data.Data.Lens

data Chemical = Chemical { _amount :: Int, _name :: String } deriving (Show, Eq, Data, Typeable)
data Reaction = Reaction { _inputs :: [Chemical], _produce :: Chemical } deriving (Show, Eq)

makeLenses ''Chemical
makeLenses ''Reaction

chemicalP = Chemical <$> (read <$> munch isDigit) <*> (string " " *> munch isUpper)
reactionP = Reaction <$> (chemicalP `sepBy` string ", ") <*> (string " => " *> chemicalP)
reactionsP = reactionP `sepBy` char '\n'

readReactions = fst . last . readP_to_S reactionsP <$> readFile "data/Day14-input.txt"

cause reactions required =
  case find (\r -> r ^. produce . name == required ^. name) reactions of
    Nothing -> [required]
    Just r  -> let produced = r ^. produce
                   (quot, rem) = (required ^. amount) `quotRem` (produced ^. amount) in
      consume quot r ++ left rem required
  where
    consume quot reaction =
      if quot == 0
      then []
      else reaction ^. inputs & mapped . amount %~ (*quot)
    left rem required =
      if rem == 0
      then []
      else [required & amount .~ rem]

mix = map (foldl1 (\r1 r2 -> r1 & amount %~ (+ r2 ^. amount)))
    . groupBy ((==) `on` _name)
    . sortBy (compare `on` _name)

requiredBy c reactions =
  case find (\r -> r ^. produce . name == c ^. name) reactions of
    Nothing -> []
    Just r  -> uniq $ r ^.. inputs . each . name ++ (concat $ map (flip requiredBy reactions) (r ^. inputs))
  where
    uniq = map head . group . sort

distill required reactions = mix $ chain (a ^.. each . produce) reactions ++ b
  where
    distilled = distilling required []
    distilling (c:cs) cs' =
      if (c ^. name) `notElem` (concat $ map (flip requiredBy reactions) (cs++cs'))
      then (c ^. name):distilling cs (c:cs')
      else distilling cs (c:cs')
    distilling [] cs' = []
    a = reactions ^.. folded.filtered (\r -> r ^. produce . name `elem` distilled)
    b = required ^.. folded.filtered (\c -> c^.name `notElem` distilled)

chain listOfRequired reactions =
  let results = mix . concat $ listOfRequired & mapped %~ cause reactions in
    if listOfRequired == results then results else chain results reactions

superChain [Chemical n "ORE"] reactions = n
superChain listOfRequired reactions =
  let r = flip distill reactions $ chain listOfRequired reactions in
    superChain r reactions

day14Part1 = superChain [Chemical 1 "FUEL"] <$> readReactions

oneTrillion = 1000000000000

day14Part2 n = do
  ore <- superChain [Chemical n "FUEL"] <$> readReactions
  print ore
  return $ ore < oneTrillion
 where
  oneTrillion = 1000000000000

-- λ> day14Part2 4906796
-- 999999889978
