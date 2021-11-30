module Y2015.AOC1 where

import Data.Either (fromRight)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (many1, string, (<|>))
import Text.Parsec.ByteString (Parser, parseFromFile)

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
solution1 :: IO Int
solution1 = do
  commands <- fromRight [] <$> parseFromFile inputParser "AOC1.input"
  return $ execute commands

-- 1797
solution2 :: IO Int
solution2 = do
  commands <- fromRight [] <$> parseFromFile inputParser "AOC1.input"
  return $ executeTilBasement commands

verify :: Test
verify =
  TestList
    [ TestCase (solution1 >>= assertEqual "solution 1" 280),
      TestCase (solution2 >>= assertEqual "solution 2" 1797)
    ]
