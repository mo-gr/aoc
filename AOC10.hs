module AOC10 where

import Text.Parsec ( endOfLine, digit, many1 )

import Text.Parsec.ByteString (Parser, parseFromFile)
import Data.Either (fromRight)
import Data.List (sort)

type Jolts = Int

number :: Parser Int
number = read <$> many1 digit

inputParser :: Parser [Jolts]
inputParser = many1 (number <* endOfLine)

isCompatible :: Jolts -> Jolts -> Bool
isCompatible outp inp = outp == inp - 1
  || outp == inp - 2
  || outp == inp - 3

output :: Jolts
output = 0

device :: [Jolts] -> Jolts
device = (+3) . maximum

countJoltDifferences :: Int -> [Jolts] -> Int
countJoltDifferences _n [] = 0
countJoltDifferences _n [_x] = 0
countJoltDifferences n (x:y:xs) = if y - x == n
 then 1 + countJoltDifferences n (y:xs)
 else 0 + countJoltDifferences n (y:xs)

dropTil :: Jolts -> [Jolts] -> [Jolts]
dropTil c (x:xs) | x < c = dropTil c xs
dropTil _c xs = xs

sumPreviousTouches :: [Int] -> Int -> Int
sumPreviousTouches [] _count = 1
sumPreviousTouches (x:_x) 1 = x
sumPreviousTouches (x:y:_x) 2 = x + y
sumPreviousTouches (x:y:z:_x) 3 = x + y + z
sumPreviousTouches _touches _count = error "something went wrong: more than three previous jolts"

countValidConfigurations :: [Jolts] -> Int
countValidConfigurations jj = let
  indexes = [0..(length jj - 1)]
  countPreviousCompatible i = length . take 3 . filter (`isCompatible` (jj !! i)) $ take i jj
  addPreviousTouches touches i = sumPreviousTouches touches (countPreviousCompatible i) : touches 
  in
    last . reverse $ foldl addPreviousTouches [] indexes

-- 2450
solution1 :: IO Int
solution1 = do
  adapters <- fromRight [] <$> parseFromFile inputParser "AOC10.input"
  let sortedAdapters = sort adapters
  let oneJolts = countJoltDifferences 1 $ concat [[output], sortedAdapters, [device adapters]]
  let threeJolts = countJoltDifferences 3 $ concat [[output], sortedAdapters, [device adapters]]
  return $ oneJolts * threeJolts

-- 32396521357312
solution2 :: IO Int
solution2 = do
  adapters <- fromRight [] <$> parseFromFile inputParser "AOC10.input"
  let sortedAdapters = output : sort adapters ++ [device adapters]
  return $ countValidConfigurations sortedAdapters
