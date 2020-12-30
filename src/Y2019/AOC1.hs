{-# LANGUAGE OverloadedStrings #-}

module Y2019.AOC1 (solution1, solution2) where

import           Text.Parsec            (digit, many1, skipMany, space)
import           Text.Parsec.ByteString (Parser, parseFromFile)

number :: Parser Integer
number = read <$> many1 digit

parseOp :: Parser [Integer]
parseOp = many1 (number <* skipMany space)

calculateFuel :: Integer -> Integer
calculateFuel = max 0 . subtract 2 . floor . (/ (3.0::Double)) . fromIntegral

totalFuel :: Integer -> Integer
totalFuel x | x <= 0 = 0
totalFuel x = let fuel = calculateFuel x in fuel + totalFuel fuel

-- 3252208
solution1 :: IO Integer
solution1 = do
  ops <- parseFromFile parseOp "AOC1.input"
  case ops of
    Right o -> return . sum $ calculateFuel <$> o
    Left  e -> error $ show e

--4875451
solution2 :: IO Integer
solution2 = do
  ops <- parseFromFile parseOp "AOC1.input"
  case ops of
    Right o -> return . sum $ totalFuel <$> o
    Left  e -> error $ show e
