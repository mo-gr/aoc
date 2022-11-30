{-# LANGUAGE OverloadedStrings #-}

module Y2018.AOC2 where

import AOC (Solution (PureSolution))
import Text.Parsec.ByteString (Parser)
import Util (Input)
import Data.List (sort, group)
import Data.ByteString.Lens (chars)
import Control.Lens (Fold)
import Control.Lens (toListOf)
import Control.Lens (lined)
import Control.Lens (folded)
import Control.Lens (filtered)
import Control.Lens (lengthOf)
import Control.Lens (productOf)
import Control.Lens (each)

inputParser :: Parser Int
inputParser = undefined

-- 6225
solution1 :: Input -> String
solution1 input = 
  show . productOf each $
   (lengthOf twos (toStrings input), lengthOf threes (toStrings input))

-- revtaubfniyhsgxdoajwkqilp
solution2 :: Input -> String
solution2 input = undefined

solution :: Solution
solution = PureSolution solution1 "6225" solution2 "revtaubfniyhsgxdoajwkqilp"

toStrings :: Input -> [String]
toStrings = toListOf lined . toListOf chars

twoLetters, threeLetters :: String -> Bool
twoLetters = elem 2 . fmap length . group . sort
threeLetters = elem 3 . fmap length . group . sort


twos, threes:: Fold [String] String
twos = folded . filtered twoLetters
threes = folded . filtered threeLetters