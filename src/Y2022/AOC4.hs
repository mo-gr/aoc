{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC4 where

import AOC (Solution (PureSolution))
import qualified Data.Set as S
import Text.Parsec (char, many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

inputParser :: Parser [(S.Set Int, S.Set Int)]
inputParser = many1 $ do
  s <- number <* char '-'
  e <- number <* char ','
  s' <- number <* char '-'
  e' <- number <* newline
  pure (S.fromList [s .. e], S.fromList [s' .. e'])

contained, overlap :: Ord a => (S.Set a, S.Set a) -> Bool
contained (s, s') = (S.union s s' == s) || (S.union s s' == s')
overlap (s, s') = not . S.null $ S.intersection s s'

-- 651
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> filter contained
    |> length

-- 956
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> filter overlap
    |> length

solution :: Solution
solution = PureSolution solution1 651 solution2 956
