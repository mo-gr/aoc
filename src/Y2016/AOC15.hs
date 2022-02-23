{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC15 where

import AOC (Solution (PureSolution))
import Text.Parsec (many1, newline, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Disc = Disc
  { positions :: !Int,
    current :: !Int
  }

isOpen :: Disc -> Bool
isOpen d = current d == 0

rotate :: Disc -> Disc
rotate d = d {current = (current d + 1) `mod` positions d}

dropThrough :: Int -> [Disc] -> Bool
dropThrough _time [] = True
dropThrough (-1) (d : rst) = isOpen d && dropThrough (-1) (rotate <$> rst)
dropThrough n ds = dropThrough (pred n) (rotate <$> ds)

inputParser :: Parser [Disc]
inputParser = many1 $ do
  pos <- string "Disc #" *> number *> string " has " *> number
  cur <- string " positions; at time=0, it is at position " *> number
  _ <- string "." *> newline
  pure $ Disc pos cur

dropThroughSmart :: Int -> [Disc] -> Bool
dropThroughSmart time discs = all (\(slots, rest) -> time `mod` slots == rest) moduloRest
  where
    moduloRest :: [(Int, Int)]
    moduloRest = f <$> zip [1 ..] discs
      where
        f (idx, disc) = (positions disc, abs (positions disc - current disc - idx) `mod` positions disc)

-- 148737
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> \discs ->
      filter (`dropThroughSmart` discs) [1 ..]
        |> head

-- 2353212
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> \discs ->
      filter (`dropThroughSmart` (discs <> [extraDisc])) [1 ..]
        |> head

extraDisc :: Disc
extraDisc = Disc 11 0

solution :: Solution
solution = PureSolution solution1 148737 solution2 2353212
