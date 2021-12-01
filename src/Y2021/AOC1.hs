module Y2021.AOC1 where

import qualified Data.ByteString.Char8 as C
import Data.Either (fromRight)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (newline, runP, sepBy1)
import Text.Parsec.ByteString (Parser)
import Util (number)

type Measurement = Int

inputParser :: Parser [Measurement]
inputParser = sepBy1 number newline

countIncreases :: [Measurement] -> Int
countIncreases (m : m' : ms)
  | m < m' = 1 + countIncreases (m' : ms)
  | otherwise = countIncreases (m' : ms)
countIncreases _ = 0

triplets :: Num a => [a] -> [a]
triplets xs = (\(a, b, c) -> a + b + c) <$> zip3 xs (tail xs) (tail $ tail xs)

-- 1162
solution1 :: C.ByteString -> Int
solution1 input = countIncreases $ fromRight [] $ runP inputParser () "" input

-- 1190
solution2 :: C.ByteString -> Int
solution2 input = countIncreases . triplets $ fromRight [] $ runP inputParser () "" input

verify :: IO C.ByteString -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 1162 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 1190 . solution2 =<< input
    ]