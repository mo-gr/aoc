module Y2020.AOC24 where

import AOC (Solution (PureSolution))
import Data.Functor (($>))
import Data.List (foldl', group, sort)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Text.Parsec (endOfLine, many1, string, try, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

newtype Coord = Coord (Int, Int) deriving (Show, Eq, Ord)

data Pattern = Pattern {white :: S.Set Coord, black :: S.Set Coord} deriving (Show)

type Instruction = [Coord]

coordParser :: Parser Coord
coordParser =
  Coord
    <$> ( string "e" $> (2, 0)
            <|> string "w" $> (-2, 0)
            <|> try (string "ne") $> (1, -1)
            <|> try (string "sw") $> (-1, 1)
            <|> string "se" $> (1, 1)
            <|> string "nw" $> (-1, -1)
        )

instructionParser :: Parser Instruction
instructionParser = many1 coordParser <* endOfLine

inputParser :: Parser [Instruction]
inputParser = many1 instructionParser

instance Semigroup Coord where
  (<>) (Coord (x, y)) (Coord (x', y')) = Coord (x + x', y + y')

instance Monoid Coord where
  mempty = Coord (0, 0)

neighbours :: Coord -> S.Set Coord
neighbours (Coord (x, y)) = S.fromList $ Coord <$> [(x -1, y -1), (x + 1, y -1), (x + 2, y), (x + 1, y + 1), (x -1, y + 1), (x -2, y)]

tickBlack :: S.Set Coord -> Coord -> Maybe Coord
tickBlack blacks tile =
  let neighbouringTiles = neighbours tile
   in case length $ S.intersection neighbouringTiles blacks of
        0 -> Nothing
        1 -> Just tile
        2 -> Just tile
        _ -> Nothing

tickWhite :: S.Set Coord -> Coord -> Maybe Coord
tickWhite blacks tile =
  let neighbouringTiles = neighbours tile
   in case length $ S.intersection neighbouringTiles blacks of
        2 -> Just tile
        _ -> Nothing

evolve :: S.Set Coord -> S.Set Coord
evolve blacks =
  let relevantFields = S.foldl S.union S.empty $ S.map neighbours blacks
      relevantWhites = relevantFields S.\\ blacks
      blacks' = catMaybesS $ S.map (tickBlack blacks) blacks
      newBlacks = catMaybesS $ S.map (tickWhite blacks) relevantWhites
   in S.union blacks' newBlacks

catMaybesS :: Ord a => S.Set (Maybe a) -> S.Set a
catMaybesS s = S.fromList $ catMaybes $ S.toList s

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x = foldl' (\a _i -> f a) x [1 .. n]

-- 317
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> fmap mconcat
    |> sort
    |> group
    |> fmap length
    |> filter odd
    |> length

-- 3804
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> fmap mconcat
    |> sort
    |> group
    |> filter (odd . length)
    |> fmap head
    |> S.fromList
    |> nTimes 100 evolve
    |> length

solution :: Solution
solution = PureSolution solution1 317 solution2 3804
