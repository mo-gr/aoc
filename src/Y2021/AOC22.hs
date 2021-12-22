{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC22 where

import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>), negativeNumber)
import Data.Functor (($>))
import Text.Parsec (string, (<|>), newline, many1, try)
import qualified Data.Set as S

type Point = (Int, Int, Int)

type Range = ([Int], [Int], [Int])

data Step = Enable Range | Disable Range deriving (Eq, Show)

stepParser :: Parser Step
stepParser = do
  stepType <- try (string "on " $> Enable) <|> (string "off " $> Disable)
  xRangeStart <- string "x=" *> negativeNumber
  xRangeEnd <- string ".." *> negativeNumber <* string ","
  yRangeStart <- string "y=" *> negativeNumber
  yRangeEnd <- string ".." *> negativeNumber <* string ","
  zRangeStart <- string "z=" *> negativeNumber
  zRangeEnd <- string ".." *> negativeNumber <* newline
  --pure $ stepType ([(max xRangeStart (-50)) .. (min xRangeEnd 50)], [(max yRangeStart (-50)) .. (min yRangeEnd 50)], [(max zRangeStart (-50)) .. (min zRangeEnd 50)])
  pure $ stepType ([xRangeStart .. xRangeEnd], [yRangeStart .. yRangeEnd], [zRangeStart .. zRangeEnd])

inputParser :: Parser [Step]
inputParser = many1 stepParser

cubesInRange :: Range -> S.Set Point
cubesInRange (xs, ys, zs) = S.fromList $ do
  z <- zs
  y <- ys
  x <- xs
  pure (x,y,z)
--  if (x < -50) || (x > 50) || (y < -50) || (y > 50) || (z < -50) || (z > 50)
--  then [] else pure (x,y,z)

runProcedure :: Step -> S.Set Point -> S.Set Point
runProcedure (Enable r) enabled = S.union enabled $ cubesInRange r
runProcedure (Disable r) enabled = S.difference enabled (cubesInRange r)


-- 588120
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> foldl (flip runProcedure) S.empty
    |> length

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> error "not yet"

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 588120 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" undefined . solution2 =<< input
    ]

testData :: Input
testData = "on x=10..12,y=10..12,z=10..12\non x=11..13,y=11..13,z=11..13\noff x=9..11,y=9..11,z=9..11\non x=10..10,y=10..10,z=10..10\noff x=79458..91210,y=-6760..4282,z=1518..13350\n"