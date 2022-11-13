{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC5 where

import AOC (Solution (PureSolution))
import qualified Data.Map.Strict as M
import Text.Parsec (many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, parseOrDie, (|>))

inputParser :: Parser [Int]
inputParser = many1 (negativeNumber <* newline)

type Memory = M.Map Int Int

type CPU = (Int, Memory)

testData :: [Int]
testData = [0, 3, 0, 1, -3]

load :: [Int] -> Memory
load ns = M.fromList (zip [0 ..] ns)

eval1 :: CPU -> CPU
eval1 (pc, memory) = case M.lookup pc memory of
  Just val -> (pc + val, M.insert pc (succ val) memory)
  Nothing -> (pc, memory)

eval2 :: CPU -> CPU
eval2 (pc, memory) = case M.lookup pc memory of
  Just val | val >= 3 -> (pc + val, M.insert pc (pred val) memory)
  Just val -> (pc + val, M.insert pc (succ val) memory)
  Nothing -> (pc, memory)

solve :: (CPU -> CPU) -> [Int] -> Int
solve solver program = go (0, load program) 0
  where
    go cpu@(pc, memory) n = case M.lookup pc memory of
      Nothing -> n
      Just _ -> go (solver cpu) (succ n)

-- 355965
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> solve eval1

-- 26948068
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve eval2

solution :: Solution
solution = PureSolution solution1 355965 solution2 26948068
