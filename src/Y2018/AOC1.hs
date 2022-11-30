{-# LANGUAGE OverloadedStrings #-}

module Y2018.AOC1 where

import AOC (Solution (PureSolution))
import Control.Lens (each, folded, lined, over, sumOf, toListOf)
import Data.ByteString.Lens (chars)
import qualified Data.Set as S
import Util (Input)

toInt :: String -> Int
toInt ('+' : rest) = read rest
toInt s = read s

solution1 :: Input -> Int
solution1 = sumOf folded . over each toInt . toListOf lined . toListOf chars

-- 561
firstRepeatSum :: (Ord a, Num a) => [a] -> a
firstRepeatSum = go S.empty 0
  where
    go prev rollingSum (a : rest)
      | S.member (a + rollingSum) prev = a + rollingSum
      | otherwise = go (S.insert (a + rollingSum) prev) (a + rollingSum) rest
    go _ _ [] = error "no repeat"

-- 563
solution2 :: Input -> Int
solution2 = firstRepeatSum . cycle . over each toInt . toListOf lined . toListOf chars

solution :: Solution
solution = PureSolution solution1 561 solution2 563
