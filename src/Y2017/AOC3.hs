{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC3 where

import AOC (Solution (PureSolution))
import Control.Monad.State.Lazy (State, evalState, forM, get, put)
import qualified Data.Map.Strict as M
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Point = (Int, Int)

data Direction = R | U | L | D deriving (Show, Eq)

manhattan :: Point -> Point -> Int
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

circle :: Int -> [Direction]
circle 0 = []
circle n = R : replicate (2 * n -1) U <> replicate (2 * n) L <> replicate (2 * n) D <> replicate (2 * n) R

step :: Direction -> Point -> Point
step R (x, y) = (succ x, y)
step L (x, y) = (pred x, y)
step U (x, y) = (x, succ y)
step D (x, y) = (x, pred y)

snail :: [Direction]
snail = go 0
  where
    go n = circle n <> go (succ n)

addresses :: [Point]
addresses = scanl s (0, 0) snail
  where
    s = flip step

inputParser :: Parser Int
inputParser = number

solve1 :: Int -> Int
solve1 t = addresses !! pred t |> manhattan (0, 0)

-- 371
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> solve1

type Memory = M.Map Point Int

type WithMemory = State Memory

neighbours :: Point -> [Point]
neighbours p = tail $ scanl (flip step) p $ circle 1

checksum :: Point -> WithMemory Int
checksum (0, 0) = pure 1
checksum n = do
  mem <- get
  let check = neighbours n |> fmap (getOr0 mem) |> sum
  put (M.insert n check mem)
  pure check

getOr0 :: Memory -> Point -> Int
getOr0 mem p = case M.lookup p mem of
  Just n -> n
  _ -> 0

initialState :: Memory
initialState = M.singleton (0, 0) 1

solve2 :: Int -> Int
solve2 target = evalState solve initialState
  where
    solve = do
      checksums <- forM addresses checksum
      pure . head . dropWhile (< target) $ checksums

-- 369601
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve2

solution :: Solution
solution = PureSolution solution1 371 solution2 369601
