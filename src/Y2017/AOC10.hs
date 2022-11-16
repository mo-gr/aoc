{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC10 where

import AOC (Solution (PureSolution))
import Text.Parsec (char, newline, sepBy1)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Ring = Ring
  { _elems :: [Int],
    _current :: Int,
    _skip :: Int
  }
  deriving (Show)

mkRing :: Int -> Ring
mkRing n = Ring [0 .. n] 0 0

step :: Ring -> Int -> Ring
step (Ring elems current skip) x = Ring elems' ((current + x + skip) `mod` elemLength) (succ skip)
  where
    elemLength = length elems
    subList = cycle elems |> drop current |> take x |> reverse
    elemsRolled = take elemLength (subList <> (cycle elems |> drop (current + x) |> take elemLength))
    elems' = cycle elemsRolled |> drop (elemLength - current) |> take elemLength

checksum :: Ring -> Int
checksum r = _elems r |> take 2 |> product

inputParser :: Parser [Int]
inputParser = (number `sepBy1` char ',') <* newline

-- 23874
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> foldl step (mkRing 255)
    |> checksum

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> undefined

solution :: Solution
solution = PureSolution solution1 23874 solution2 undefined
