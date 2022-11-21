{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC16 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Array (elems, listArray, (!), (//))
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import qualified Data.Set as S
import Text.Parsec (char, letter, newline, sepBy1)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>), times)

type Program = Char

data Dance
  = Spin Int
  | Exchange Int Int
  | Partner Program Program
  deriving (Show, Eq)

inputParser :: Parser [Dance]
inputParser = (danceP `sepBy1` char ',') <* newline
  where
    danceP = spinP <|> exchangeP <|> partnerP
    spinP = (char 's' *> number) <&> Spin
    exchangeP = do
      a <- char 'x' *> number
      b <- char '/' *> number
      pure $ Exchange a b
    partnerP = do
      a <- char 'p' *> letter
      b <- char '/' *> letter
      pure $ Partner a b

dancers :: [Program]
dancers = ['a' .. 'p']

dance :: [Program] -> Dance -> [Program]
dance ps (Spin n) = take (length ps) $ drop (length ps - n) $ cycle ps
dance ps (Exchange a b) = swapIdx a b ps
dance ps (Partner a b) = swapEq a b ps

swapEq :: Eq a => a -> a -> [a] -> [a]
swapEq _ _ [] = []
swapEq a b (a' : rest) | a == a' = b : swapEq a b rest
swapEq a b (b' : rest) | b == b' = a : swapEq a b rest
swapEq a b (c : rest) = c : swapEq a b rest

swapIdx :: Int -> Int -> [a] -> [a]
swapIdx a b ps =
  listArray (0, pred (length ps)) ps
    |> \arr ->
      arr // [(a, arr ! b), (b, arr ! a)]
        |> elems

findLoop :: Ord a => (a -> a) -> a -> Int
findLoop f = go S.empty
  where
    go prev a =
      let a' = f a
       in if S.member a' prev
            then length prev
            else go (S.insert a' prev) a'

-- dcmlhejnifpokgba
solution1 :: Input -> String
solution1 input =
  parseOrDie inputParser input
    |> foldl dance dancers

-- ifocbejpdnklamhg
solution2 :: Input -> String
solution2 input =
  let danceMoves = parseOrDie inputParser input
      danceRound start = foldl' dance start danceMoves
      loopCount = findLoop danceRound dancers
      uniqueRounds = 1000000000 `rem` loopCount
   in times uniqueRounds danceRound dancers

solution :: Solution
solution = PureSolution solution1 "dcmlhejnifpokgba" solution2 "ifocbejpdnklamhg"
