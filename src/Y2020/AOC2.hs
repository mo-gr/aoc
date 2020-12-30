{-# LANGUAGE RecordWildCards #-}

module Y2020.AOC2 where

import           Data.Either            (fromRight)
import           Text.Parsec            (letter, many1, skipMany, space, string)
import           Text.Parsec.ByteString (Parser, parseFromFile)
import           Util                   (number)

data Policy = Policy { character::Char, minCount::Int, maxCount::Int } deriving (Show)

type Password = String

policyParser :: Parser Policy
policyParser = do
  minC <- number
  _ <- string "-"
  maxC <- number
  _ <- string " "
  char <- letter
  return $ Policy {character = char, minCount = minC, maxCount = maxC}

passwordParser :: Parser Password
passwordParser = many1 letter

lineParser :: Parser (Policy, Password)
lineParser = do
  policy <- policyParser
  _ <- string ":"  >> space
  password <- passwordParser
  skipMany space
  return (policy, password)

inputParser :: Parser [(Policy, Password)]
inputParser = many1 lineParser

exampleInput :: [(Policy, String)]
exampleInput = [
  (Policy {character = 'a', minCount = 1, maxCount = 3},"abcde"),
  (Policy {character = 'b', minCount = 1, maxCount = 3},"cdefg"),
  (Policy {character = 'c', minCount = 2, maxCount = 9},"ccccccccc")
  ]

evalPolicy :: (Policy, String) -> Bool
evalPolicy (Policy{..}, password) = let occurances = length $ filter (== character) password in
  occurances >= minCount && occurances <= maxCount

dec :: Int -> Int
dec n = n -1

evalPolicyToboggan :: (Policy, String) -> Bool
evalPolicyToboggan (Policy{..}, password) = let first = password !! dec minCount
                                                second = password !! dec maxCount in
  (first == character && second /= character) || (first /= character && second == character)

-- 625
solution1 :: IO Int
solution1 = do
  policies <- fromRight [] <$> parseFromFile inputParser "AOC2.input"
  return $ length $ filter evalPolicy policies

-- 391
solution2 :: IO Int
solution2 = do
  policies <- fromRight [] <$> parseFromFile inputParser "AOC2.input"
  return $ length $ filter evalPolicyToboggan policies
