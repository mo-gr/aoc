{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC24 where

import AOC (Solution (PureSolution))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))
import qualified Data.Set as S
import Text.Parsec (many1, char, newline, statePos, getParserState, sourceColumn, sourceLine, digit)
import Control.Applicative ((<|>))

type Point = (Int, Int)

data World = World Point [Point] (S.Set Point) deriving (Show)

instance Semigroup World where
  (World p ps s) <> (World p' ps' s') = World (if p > p' then p else p') (ps <> ps') (S.union s s')
instance Monoid World where
  mempty = World (0,0) [] S.empty

inputParser :: Parser World
inputParser =  mconcat <$> do
  many1 (wallP <|> floorP <|> goalP <|> (newline *> mempty))
  where
    wallP = char '#' *> mempty
    floorP = do
      sp <- statePos <$> getParserState
      _ <- char '.'
      pure $ World (0,0) [] (S.singleton (sourceColumn sp, sourceLine sp))
    goalP = do
      sp <- statePos <$> getParserState
      d <- digit
      pure $ case d of
        '0' -> World (sourceColumn sp, sourceLine sp) [] $ S.singleton (sourceColumn sp, sourceLine sp)
        _ -> World (0,0) [(sourceColumn sp, sourceLine sp)] $ S.singleton (sourceColumn sp, sourceLine sp)

solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> undefined

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> undefined

solution :: Solution
solution = PureSolution solution1 undefined solution2 undefined

testData :: Input
testData = "###########\n#0.1.....2#\n#.#######.#\n#4.......3#\n###########\n"