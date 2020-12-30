module AOC9 where

import           Data.Either            (fromRight)
import           Text.Parsec            (digit, endOfLine, many1)
import           Text.Parsec.ByteString (Parser, parseFromFile)

number :: Parser Int
number = read <$> many1 digit

inputParser :: Parser [Int]
inputParser = many1 (number <* endOfLine)

isValidXMAS :: [Int] -> Int -> Bool
isValidXMAS preamble x = any (\(a,b) -> a + b ==x) $ allPairs preamble

firstInvalidXMAS :: Int -> [Int] -> Int
firstInvalidXMAS pl xs = let preamble = take pl xs
                             candidate = head $ drop pl xs in
  if isValidXMAS preamble candidate then firstInvalidXMAS pl (tail xs)
                                    else candidate

allPairs :: [a] -> [(a, a)]
allPairs ls @ (_ : xs) = [(x, pair) | x <- ls, pair <- xs]
allPairs _             = []

findRange :: Int -> [Int] -> [Int]
findRange _target [] = []
findRange target xs = case sumUp xs 0 of
  Nothing -> findRange target (tail xs)
  Just x  -> x
  where
    sumUp :: [Int] -> Int -> Maybe [Int]
    sumUp [] _s = Nothing
    sumUp (x:xx) s = case compare (x+s) target of
                       LT -> ((++) [x]) <$> sumUp xx (x+s)
                       EQ -> Just [x]
                       GT -> Nothing

-- 27911108
solution1 :: IO Int
solution1 = do
   numbers <- fromRight [] <$> parseFromFile inputParser "AOC9.input"
   return $ firstInvalidXMAS 25 numbers

-- 4023754
solution2 :: IO Int
solution2 = do
  numbers <- fromRight [] <$> parseFromFile inputParser "AOC9.input"
  let target = firstInvalidXMAS 25 numbers
  let range = findRange target numbers
  return $ sum [minimum range, maximum range]
