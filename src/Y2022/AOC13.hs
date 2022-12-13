{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC13 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.List (elemIndex, intercalate, sortBy)
import Data.Maybe (fromJust)
import Text.Parsec (char, newline, sepBy)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Packet = Signal Int | SignalList [Packet]
  deriving (Eq)

instance Show Packet where
  show (Signal s) = show s
  show (SignalList ls) = "[" <> intercalate "," (fmap show ls) <> "]"

signalParser :: Parser Packet
signalParser = Signal <$> number

listParser :: Parser Packet
listParser = do
  _ <- char '['
  ps <- packetParser `sepBy` char ','
  _ <- char ']'
  pure (SignalList ps)

packetParser :: Parser Packet
packetParser = signalParser <|> listParser

inputParser :: Parser [(Packet, Packet)]
inputParser = flip sepBy newline $ do
  pl <- packetParser <* newline
  pr <- packetParser <* newline
  pure (pl, pr)

inOrder :: Packet -> Packet -> Maybe Bool
inOrder (Signal s) (Signal s') | s < s' = Just True
inOrder (Signal s) (Signal s') | s > s' = Just False
inOrder (Signal _) (Signal _) = Nothing
inOrder (SignalList []) (SignalList (_ : _)) = Just True
inOrder (SignalList (_ : _)) (SignalList []) = Just False
inOrder (SignalList []) (SignalList []) = Nothing
inOrder (SignalList (s : rst)) (SignalList (s' : rst')) = inOrder s s' <|> inOrder (SignalList rst) (SignalList rst')
inOrder s@(Signal _) sl@(SignalList _) = inOrder (SignalList [s]) sl
inOrder sl@(SignalList _) s@(Signal _) = inOrder sl (SignalList [s])

sumOfOrderedIndices :: [(Packet, Packet)] -> Int
sumOfOrderedIndices ps =
  zip [1 ..] ps
    |> filter (fromJust . uncurry inOrder . snd)
    |> fmap fst
    |> sum

-- 5393
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> sumOfOrderedIndices

solve2 :: [(Packet, Packet)] -> Int
solve2 ps = divider1Idx * divider2Idx
  where
    divider1 = SignalList [SignalList [Signal 2]]
    divider2 = SignalList [SignalList [Signal 6]]
    psWithDivider = (divider1, divider2) : ps
    ordered :: [Packet]
    ordered = concatMap (\(a, b) -> [a, b]) psWithDivider |> sortBy signalOrder
    divider1Idx = case elemIndex divider1 ordered of
      Just i -> succ i
      Nothing -> error "Can't find divider 1"
    divider2Idx = case elemIndex divider2 ordered of
      Just i -> succ i
      Nothing -> error "Can't find divider 2"
    signalOrder :: Packet -> Packet -> Ordering
    signalOrder p1 p2 = case inOrder p1 p2 of
      Just True -> LT
      Just False -> GT
      Nothing -> error "undecidable packets"

-- 26712
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve2

solution :: Solution
solution = PureSolution solution1 5393 solution2 26712
