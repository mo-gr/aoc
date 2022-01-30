{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC7 where

import AOC (Solution (PureSolution))
import Control.Applicative (liftA2)
import Data.List (isInfixOf)
import Data.Monoid (Any (Any), getAny)
import Text.Parsec (between, char, letter, many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

data IPv7 = IPv7
  { supernets :: [String],
    hypernets :: [String]
  }
  deriving (Show)

hasAnagram :: String -> Bool
hasAnagram [] = False
hasAnagram (a : b : c : d : _) | a /= b && b == c && a == d = True
hasAnagram (_a : rst) = hasAnagram rst

hasABBA :: IPv7 -> Bool
hasABBA = liftA2 (&&) (any hasAnagram . supernets) (not . any hasAnagram . hypernets)

hasAbaBab :: IPv7 -> Bool
hasAbaBab addr =
  any babInHypernets $ foldMap abasAsBabs (supernets addr)
  where
    babInHypernets bab = getAny $ foldMap (Any . isInfixOf bab) (hypernets addr)

abasAsBabs :: String -> [String]
abasAsBabs [] = []
abasAsBabs (a : b : c : rst) | a == c && a /= b = [b, a, b] : abasAsBabs (b : c : rst)
abasAsBabs (_ : rst) = abasAsBabs rst

inputParser :: Parser [IPv7]
inputParser = many1 $ do
  n <- many1 letter
  hns <- many1 $ do
    h <- between (char '[') (char ']') (many1 letter)
    n' <- many1 letter
    pure (h, n')
  _ <- newline
  pure $ IPv7 (n : (snd <$> hns)) (fst <$> hns)

-- 110
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> filter hasABBA
    |> length

-- 242
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> filter hasAbaBab
    |> length

solution :: Solution
solution = PureSolution solution1 110 solution2 242
