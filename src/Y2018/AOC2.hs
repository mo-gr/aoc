{-# LANGUAGE OverloadedStrings #-}

module Y2018.AOC2 where

import AOC (Solution (PureSolution))
import Control.Lens (Fold, each, filtered, folded, lengthOf, lined, productOf, toListOf)
import Control.Monad (guard)
import Data.ByteString.Lens (chars)
import Data.Function (on)
import Data.List (delete, group, maximumBy, sort)
import Text.Parsec.ByteString (Parser)
import Util (Input)

inputParser :: Parser Int
inputParser = undefined

-- 6225
solution1 :: Input -> String
solution1 input =
  show . productOf each $
    (lengthOf twos (toStrings input), lengthOf threes (toStrings input))

-- revtaubfniyhsgxdoajwkqilp
solution2 :: Input -> String
solution2 input =
  maximumBy (compare `on` length)
    . fmap dropDifferent
    . pairs
    $ toStrings input

solution :: Solution
solution = PureSolution solution1 "6225" solution2 "revtaubfniyhsgxdoajwkqilp"

toStrings :: Input -> [String]
toStrings = toListOf lined . toListOf chars

twoLetters, threeLetters :: String -> Bool
twoLetters = elem 2 . fmap length . group . sort
threeLetters = elem 3 . fmap length . group . sort

twos, threes :: Fold [String] String
twos = folded . filtered twoLetters
threes = folded . filtered threeLetters

dropDifferent :: (String, String) -> String
dropDifferent (_, []) = ""
dropDifferent ([], _) = ""
dropDifferent (a : arest, b : brest)
  | a == b = a : dropDifferent (arest, brest)
  | otherwise = dropDifferent (arest, brest)

pairs :: (Ord a) => [a] -> [(a, a)]
pairs as = do
  a <- as
  a' <- delete a as
  guard $ a < a'
  pure (a, a')
