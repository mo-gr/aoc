module Y2015.AOC1 where

import AOC (Solution (PureSolution))
import Text.Parsec (many1, string, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

data Command = Up | Down

commandParser :: Parser Command
commandParser = (string "(" >> return Up) <|> (string ")" >> return Down)

inputParser :: Parser [Command]
inputParser = many1 commandParser

execute :: [Command] -> Int
execute = foldl f 0
  where
    f acc Up = acc + 1
    f acc Down = acc - 1

data FoundBasement = NotYet | FoundIt

executeTilBasement :: [Command] -> Int
executeTilBasement cs = snd' $ foldl f (0, 0, NotYet) cs
  where
    f :: (Int, Int, FoundBasement) -> Command -> (Int, Int, FoundBasement)
    f (acc, pos, FoundIt) _ = (acc, pos, FoundIt)
    f (acc, i, _) Up = (acc + 1, i + 1, NotYet)
    f (acc, i, _) Down | acc - 1 < 0 = (acc - 1, i + 1, FoundIt)
    f (acc, i, _) Down = (acc - 1, i + 1, NotYet)
    snd' (_, b, _) = b

-- 280
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> execute

-- 1797
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> executeTilBasement

solution :: Solution
solution = PureSolution solution1 280 solution2 1797
