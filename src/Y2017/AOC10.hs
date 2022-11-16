{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC10 where

import AOC (Solution (PureSolution))
import Text.Parsec (char, newline, sepBy1)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))
import Data.Word (Word8)
import Data.ByteString.Char8 (unpack)
import Data.Bits (xor)
import Numeric (showHex)
import Data.ByteString.Builder (word8Hex, toLazyByteString)

data Ring = Ring
  { _elems :: [Word8],
    _current :: Int,
    _skip :: Int,
    _length :: Word8
  }
  deriving (Show)

resetElems :: Ring -> Ring
resetElems r = r {_elems = [0 .. (_length r)]}

mkRing :: Word8 -> Ring
mkRing n = Ring [0 .. n] 0 0 n

step :: Ring -> Int -> Ring
step (Ring elems current skip ln) x = Ring elems' ((current + x + skip) `mod` elemLength) (succ skip) ln
  where
    elemLength = length elems
    subList = cycle elems |> drop current |> take x |> reverse
    elemsRolled = take elemLength (subList <> (cycle elems |> drop (current + x) |> take elemLength))
    elems' = cycle elemsRolled |> drop (elemLength - current) |> take elemLength

checksum :: Ring -> Int
checksum r = _elems r |> take 2 |> fmap fromEnum |> product

inputParser :: Parser [Int]
inputParser = (number `sepBy1` char ',') <* newline

-- 23874
solution1 :: Input -> String
solution1 input =
  parseOrDie inputParser input
    |> rounds 1 (mkRing 255)
    |> checksum
    |> show

-- not df54109670266eb8273d17a5564bf3cb
solution2 :: Input -> String
solution2 input =
  toAscii input
    |> addSuffix
    |> rounds 64 (mkRing 255)
    |> _elems
    |> sparseToDense
    |> toHex

solution :: Solution
solution = PureSolution solution1 "23874" solution2 undefined

testData :: Input
testData = "AoC 2017"

toAscii :: Input -> [Int]
toAscii = fmap fromEnum . unpack

addSuffix :: [Int] -> [Int]
addSuffix xs = xs <> [17, 31, 73, 47, 23]

toWord8 :: [Int] -> [Word8]
toWord8 = fmap toEnum

sparseToDense :: [Word8] -> [Word8]
sparseToDense [] = []
sparseToDense xs = (take 16 xs |> foldl1 xor) : sparseToDense (drop 16 xs)

toHex :: [Word8] -> String
toHex = concatMap (pad . (`showHex` ""))

pad :: String -> String
pad [x] = '0':[x]
pad x = x

rounds :: Int -> Ring -> [Int] -> Ring
rounds 0 r _ = r
rounds n r input = rounds (pred n) (foldl step (resetElems r) input) input

