{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC6 where

import AOC (Solution (PureSolution))
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (Down (Down))
import Text.Parsec (letter, many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

type Frequencies = [M.Map Char Int]

toFrequencies :: Frequencies -> String -> Frequencies
toFrequencies freqs word = insert <$> zip freqs word
  where
    insert (m, c) = M.insertWith (+) c 1 m

fromFrequencies :: Ord b => (Int -> b) -> Frequencies -> String
fromFrequencies ord = fmap mostLikely
  where
    mostLikely :: M.Map Char Int -> Char
    mostLikely freq = M.assocs freq |> sortOn (ord . snd) |> head |> fst

inputParser :: Parser [String]
inputParser = many1 $ many1 letter <* newline

-- bjosfbce
solution1 :: Input -> String
solution1 input =
  parseOrDie inputParser input
    |> foldl toFrequencies (repeat M.empty)
    |> fromFrequencies Down

-- veqfxzfx
solution2 :: Input -> String
solution2 input =
  parseOrDie inputParser input
    |> foldl toFrequencies (repeat M.empty)
    |> fromFrequencies id

testData :: Input
testData = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar\n"

solution :: Solution
solution = PureSolution solution1 "bjosfbce" solution2 "veqfxzfx"
