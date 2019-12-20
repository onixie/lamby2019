{-# Language TemplateHaskell #-}

module Day14 where

import Text.ParserCombinators.ReadP
import Data.Char
import Control.Lens

data Chemical = Chemical { _amount :: Int, _name :: String } deriving (Show, Eq)
data Reaction = Reaction { _inputs :: [Chemical], _produce :: Chemical } deriving (Show, Eq)

makeLenses ''Chemical
makeLenses ''Reaction

chemicalP = Chemical <$> (read <$> munch isDigit) <*> (string " " *> munch isUpper)
reactionP = Reaction <$> (chemicalP `sepBy` string ", ") <*> (string " => " *> chemicalP)
reactionsP = reactionP `sepBy` char '\n'

readReactions = fst . last . readP_to_S reactionsP <$> readFile "data/Day14-input.txt"
