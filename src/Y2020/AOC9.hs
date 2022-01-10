module Y2020.AOC9 where

import AOC (Solution (PureSolution))
import Text.Parsec (endOfLine, many1)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

inputParser :: Parser [Int]
inputParser = many1 (number <* endOfLine)

isValidXMAS :: [Int] -> Int -> Bool
isValidXMAS preamble x = any (\(a, b) -> a + b == x) $ allPairs preamble

firstInvalidXMAS :: Int -> [Int] -> Int
firstInvalidXMAS pl xs =
  let preamble = take pl xs
      candidate = xs !! pl
   in if isValidXMAS preamble candidate
        then firstInvalidXMAS pl (tail xs)
        else candidate

allPairs :: [a] -> [(a, a)]
allPairs ls@(_ : xs) = [(x, pair) | x <- ls, pair <- xs]
allPairs _ = []

findRange :: Int -> [Int] -> [Int]
findRange _target [] = []
findRange target xs = case sumUp xs 0 of
  Nothing -> findRange target (tail xs)
  Just x -> x
  where
    sumUp :: [Int] -> Int -> Maybe [Int]
    sumUp [] _s = Nothing
    sumUp (x : xx) s = case compare (x + s) target of
      LT -> (++) [x] <$> sumUp xx (x + s)
      EQ -> Just [x]
      GT -> Nothing

-- 27911108
solution1 :: Input -> Int
solution1 input = parseOrDie inputParser input |> firstInvalidXMAS 25

-- 4023754
solution2 :: Input -> Int
solution2 input =
  let numbers = parseOrDie inputParser input
      target = firstInvalidXMAS 25 numbers
      range = findRange target numbers
   in sum [minimum range, maximum range]

solution :: Solution
solution = PureSolution solution1 27911108 solution2 4023754
