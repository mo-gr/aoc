module Y2015.AOC19 where

import AOC (Solution (PureSolution))
import Control.Applicative (empty, (<|>))
import Control.Monad.Logic (Logic, observe)
import Data.List (isPrefixOf, nub, sort)
import Data.Maybe (mapMaybe)
import Text.Parsec (letter, many1, newline, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

inputParser :: Parser (String, [Replacement])
inputParser = do
  rs <- many1 replacement
  _ <- newline
  inp <- many1 letter <* newline
  pure (inp, rs)
  where
    replacement = do
      from <- many1 letter
      _ <- string " => "
      to <- many1 letter <* newline
      pure (from, to)

type Replacement = (String, String)

explode :: String -> [Replacement] -> [String]
explode inp rs = nub . sort $ recur inp
  where
    recur "" = []
    recur str@(h : t) = fmap (h :) (recur t) <> mapMaybe (replaceIn str) rs
    replaceIn s (from, to) | from `isPrefixOf` s = Just (to <> drop (length from) s)
    replaceIn _ _ = Nothing

choose :: [a] -> Logic a
choose = foldr ((<|>) . pure) empty

fabricate :: String -> String -> Int -> [Replacement] -> Logic Int
fabricate origin goal currentCost _rs | origin == goal = pure currentCost
fabricate origin goal currentCost rs = do
  evolution <- choose $ explode origin rs
  fabricate evolution goal (succ currentCost) rs

rreverse :: (String, String) -> (String, String)
rreverse (a, b) = (reverse b, reverse a)

-- 518
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> uncurry explode
    |> length

-- 200
solution2 :: Input -> Int
solution2 input =
  let (medicine, rules) = parseOrDie inputParser input
   in fmap rreverse rules
        |> fabricate (reverse medicine) "e" 0
        |> observe

solution :: Solution
solution = PureSolution solution1 518 solution2 200
