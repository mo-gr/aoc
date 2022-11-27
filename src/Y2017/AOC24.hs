{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC24 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List (delete)
import Text.Parsec (char, many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Adapter = (Int, Int)

inputParser :: Parser [Adapter]
inputParser = many1 $ do
  lhs <- number
  rhs <- char '/' *> number <* newline
  pure (lhs, rhs)

strength :: [Adapter] -> Int
strength [] = 0
strength ((l, r) : rest) = l + r + strength rest

findMatch :: Int -> [Adapter] -> [(Adapter, [Adapter])]
findMatch port adapter = do
  (lp, rp) <- adapter
  guard $ (lp == port) || (rp == port)
  pure $ if lp == port then ((lp, rp), delete (lp, rp) adapter) else ((rp, lp), delete (lp, rp) adapter)

buildBridge :: Int -> [Adapter] -> [[Adapter]]
buildBridge port adapter = do
  ((lh, rh), rest) <- findMatch port adapter
  b <- buildBridge rh rest <|> [[]]
  [(lh, rh) : b]

-- 2006
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> buildBridge 0
    |> fmap strength
    |> maximum

findLongAndStrong :: [[Adapter]] -> Int
findLongAndStrong bridges = maximum . fmap strength $ onlyLong
  where
    longest :: Int
    longest = maximum . fmap length $ bridges
    onlyLong :: [[Adapter]]
    onlyLong = filter ((== longest) . length) bridges

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> buildBridge 0
    |> findLongAndStrong

solution :: Solution
solution = PureSolution solution1 2006 solution2 undefined
