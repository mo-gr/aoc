{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Y2017.AOC14 where

import AOC (Solution (PureSolution))
import Control.Monad (guard)
import Data.Bits (popCount, testBit)
import qualified Data.Set as S
import Data.Word (Word8)
import Text.Parsec (letter, many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))
import Y2017.AOC10 (knotHash)

type Pos = (Int, Int)

inputParser :: Parser String
inputParser = many1 letter <* newline

countBits :: [Word8] -> Int
countBits ws = sum (fmap popCount ws)

toAscii :: String -> [Int]
toAscii = fmap fromEnum

solve1 :: String -> Int
solve1 input = sum (fmap countBits hashed)
  where
    toHash = fmap toAscii [input <> "-" <> show i | i <- [(0 :: Int) .. 127]]
    hashed = fmap knotHash toHash

-- 8106
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> filter (/= '\n') -- drop newline
    |> solve1

toBitField :: [[Word8]] -> [Pos]
toBitField ls = zip [0 ..] ls |> concatMap (\(y, l) -> fmap (,y) (allSetBits l))

findNeighbours :: S.Set Pos -> Pos -> S.Set Pos
findNeighbours field root = go S.empty [root]
  where
    go :: S.Set Pos -> [Pos] -> S.Set Pos
    go visited [] = visited
    go visited ps =
      let newNeighbours = do
            (x, y) <- ps
            neighbour <- [(succ x, y), (pred x, y), (x, succ y), (x, pred y)]
            guard $ neighbour `S.notMember` visited
            guard $ neighbour `S.member` field
            pure neighbour
       in go (S.union visited (S.fromList newNeighbours)) newNeighbours

countRegions :: [Pos] -> Int
countRegions pos = go 0 S.empty (S.fromList pos) pos
  where
    go :: Int -> S.Set Pos -> S.Set Pos -> [Pos] -> Int
    go gs _ _ [] = gs
    go gs inGroups bitField (p : rest)
      | S.member p inGroups = go gs inGroups bitField rest
      | otherwise =
        if S.disjoint neighbours inGroups
          then go (succ gs) (S.union inGroups neighbours) bitField rest
          else go gs (S.union inGroups neighbours) bitField rest
      where
        neighbours = findNeighbours bitField p

allSetBits :: [Word8] -> [Int]
allSetBits ws = zip [0 ..] ws |> concatMap (\(i, w) -> fmap ((8 * i) +) (toSetBits w))

toSetBits :: Word8 -> [Int]
toSetBits w = filter (\b -> testBit w (7 - b)) [0 .. 7]

solve2 :: String -> Int
solve2 input = toBitField hashed |> countRegions
  where
    toHash = fmap toAscii [input <> "-" <> show i | i <- [(0 :: Int) .. 127]]
    hashed = fmap knotHash toHash

-- 1164
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve2

solution :: Solution
solution = PureSolution solution1 8106 solution2 1164
