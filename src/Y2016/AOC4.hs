{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC4 where

import AOC (Solution (PureSolution))
import Data.Function (on)
import Data.List (group, intersperse, sort, sortBy)
import Text.Parsec (char, letter, many1, sepEndBy1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Room = Room
  { name :: String,
    sector :: Int,
    checksum :: String
  } deriving (Show)

decrypt :: Room -> Room
decrypt r = r {name = name r |> fmap (shift (sector r) )|> filter (/= '-')}

shift :: Int -> Char -> Char
shift 0 c = c
shift _ '-' = '-'
shift n 'z' = shift (pred n) 'a'
shift n c  = shift (pred n) (succ c)

valid :: Room -> Bool
valid r =
  name r
    |> filter (/= '-')
    |> sort
    |> group
    |> sortBy compareOnLengthAndOrder
    |> fmap head
    |> take 5
    |> (== checksum r)

compareOnLengthAndOrder :: String -> String -> Ordering
compareOnLengthAndOrder a b = case (compare `on` length) b a of
  LT -> LT
  GT -> GT
  EQ -> (compare `on` head) a b

inputParser :: Parser [Room]
inputParser = many1 $ do
  n <- intersperse '-' . mconcat <$> many1 letter `sepEndBy1` char '-'
  s <- number
  c <- char '[' *> many1 letter <* char ']' <* newline
  pure $ Room n s c

-- 245102
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> filter valid
    |> fmap sector
    |> sum

-- 324
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> filter valid
    |> fmap decrypt
    |> filter ((== "northpoleobjectstorage").name)
    |> sector.head

solution :: Solution
solution = PureSolution solution1 245102 solution2 324
