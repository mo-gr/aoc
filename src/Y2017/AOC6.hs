{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC6 where

import AOC (Solution (PureSolution))
import Data.List (elemIndex, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Parsec (char, sepBy1)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Address = Int

type Memory = M.Map Address Int

inputParser :: Parser [Int]
inputParser = number `sepBy1` char '\t'

redistribute :: Int -> Address -> Memory -> Memory
redistribute 0 _ m = m
redistribute n a m = M.adjust succ a m |> redistribute (pred n) (next m a)

balance :: Memory -> Memory
balance m =
  M.toList m
    |> sortOn fst
    |> reverse
    |> sortOn snd
    |> last
    |> \(addr, val) -> redistribute val (next m addr) (M.insert addr 0 m)

next :: Memory -> Address -> Address
next m a | (M.findMax m |> fst) == a = M.findMin m |> fst
next _ a = succ a

toMemory :: [Int] -> Memory
toMemory vals = M.fromList (zip [0 ..] vals)

runTilDuplicate :: Ord a => Int -> S.Set a -> (a -> a) -> a -> Int
runTilDuplicate n prev f a =
  let a' = f a
   in if S.member a' prev
        then n
        else runTilDuplicate (succ n) (S.insert a' prev) f a'

runTilDuplicateCountCycle :: Eq a => [a] -> (a -> a) -> a -> Int
runTilDuplicateCountCycle [] f a = runTilDuplicateCountCycle [a] f a
runTilDuplicateCountCycle prev f a =
  let a' = f a
      aidx = elemIndex a' . reverse $ prev
   in case aidx of
        Just idx -> length prev - idx
        Nothing -> runTilDuplicateCountCycle (a' : prev) f a'

-- 4074
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> toMemory
    |> runTilDuplicate 1 S.empty balance

-- 2793
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> toMemory
    |> runTilDuplicateCountCycle [] balance

solution :: Solution
solution = PureSolution solution1 4074 solution2 2793
