{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import Data.Char
import Text.ParserCombinators.ReadP

pa <++> pb = ((++) <$> pa <*> pb)

code :: ReadP Int
code = read <$> (option "" (string "-") <++> munch isDigit)

program :: ReadP [Int]
program = (code `sepBy` char ',') <* optional (char '\n') <* eof

readInput = readFile "data/Day5-input.txt" >>= return . last . readP_to_S program
