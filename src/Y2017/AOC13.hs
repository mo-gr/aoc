{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC13 where

import AOC (Solution (PureSolution))
import qualified Data.Map.Strict as M
import Text.Parsec (many1, newline, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Level = Int

type Depth = Int

inputParser :: Parser [(Level, Depth)]
inputParser = many1 $ do
  l <- number
  _ <- string ": "
  d <- number
  _ <- newline
  pure (l, d)

makeLayer :: Depth -> [Depth]
makeLayer 0 = repeat 0
makeLayer n = ([0 .. pred n] <> reverse [1 .. pred (pred n)]) |> cycle

tick :: M.Map Level [Depth] -> M.Map Level [Depth]
tick = M.map tail

countCaught :: M.Map Level [Depth] -> [Int]
countCaught m = snd caught
  where
    levels :: Int
    levels = M.findMax m |> fst
    caught = foldl step (m, []) [0 .. levels]
    step :: (M.Map Level [Depth], [Int]) -> Level -> (M.Map Level [Depth], [Int])
    step (state, ccount) lvl = case M.lookup lvl state of
      Nothing -> (tick state, ccount)
      Just scanner ->
        if head scanner == 0
          then (tick state, lvl : ccount)
          else (tick state, ccount)

severity :: M.Map Level Depth -> [Int] -> Int
severity l cs = sum $ fmap (\c -> c * M.findWithDefault 0 c l) cs

-- 1632
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> M.fromList
    |> \levels ->
      M.map makeLayer levels
        |> countCaught
        |> severity levels

mkFilter :: (Level, Depth) -> (Int -> Bool)
mkFilter (l, d) i = (i + l) `rem` (2 * pred d) /= 0

solve2 :: [(Level, Depth)] -> Int
solve2 ls = filter combinedFilter [0 ..] |> head
  where
    combinedFilter = allP (fmap mkFilter ls)
    allP :: [a -> Bool] -> a -> Bool
    allP [] _ = True
    allP (p : rest) a = p a && allP rest a

-- 3834136
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve2

solution :: Solution
solution = PureSolution solution1 1632 solution2 3834136
