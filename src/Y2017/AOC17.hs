{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC17 where

import AOC (Solution (PureSolution))
import qualified Data.Sequence as S
import Text.Parsec (newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, times)

inputParser :: Parser Int
inputParser = number <* newline

type Spinlock = (Int, S.Seq Int)

spin :: Int -> Spinlock -> Spinlock
spin _ (0, b) | S.null b = (1, S.fromList [0, 1])
spin step (index, buffer) = (index', buffer')
  where
    !count = length buffer
    !index' = succ $ (index + step) `mod` count
    !buffer' = S.insertAt index' count buffer

-- 2000
solution1 :: Input -> Int
solution1 input =
  let step = parseOrDie inputParser input
      (idx, buff) = times 2017 (spin step) (0, S.empty)
   in buff `S.index` succ idx

-- 10242889
solution2 :: Input -> Int
solution2 input =
  let step = parseOrDie inputParser input
      (_, buff) = times 50000000 (spin step) (0, S.empty)
      zeroIdx = case S.findIndexL (== 0) buff of
        Just idx -> idx
        Nothing -> error "no 0 found"
   in buff `S.index` succ zeroIdx

solution :: Solution
solution = PureSolution solution1 2000 solution2 10242889
