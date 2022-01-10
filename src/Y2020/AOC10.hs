module Y2020.AOC10 where

import AOC (Solution (PureSolution))
import Data.List (sort)
import Text.Parsec (endOfLine, many1)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie)

type Jolts = Int

inputParser :: Parser [Jolts]
inputParser = many1 (number <* endOfLine)

isCompatible :: Jolts -> Jolts -> Bool
isCompatible outp inp =
  outp == inp - 1
    || outp == inp - 2
    || outp == inp - 3

output :: Jolts
output = 0

device :: [Jolts] -> Jolts
device = (+ 3) . maximum

countJoltDifferences :: Int -> [Jolts] -> Int
countJoltDifferences _n [] = 0
countJoltDifferences _n [_x] = 0
countJoltDifferences n (x : y : xs) =
  if y - x == n
    then 1 + countJoltDifferences n (y : xs)
    else 0 + countJoltDifferences n (y : xs)

dropTil :: Jolts -> [Jolts] -> [Jolts]
dropTil c (x : xs) | x < c = dropTil c xs
dropTil _c xs = xs

sumPreviousTouches :: [Int] -> Int -> Int
sumPreviousTouches [] _count = 1
sumPreviousTouches (x : _x) 1 = x
sumPreviousTouches (x : y : _x) 2 = x + y
sumPreviousTouches (x : y : z : _x) 3 = x + y + z
sumPreviousTouches _touches _count = error "something went wrong: more than three previous jolts"

countValidConfigurations :: [Jolts] -> Int
countValidConfigurations jj =
  let indexes = [0 .. (length jj - 1)]
      countPreviousCompatible i = length . take 3 . filter (`isCompatible` (jj !! i)) $ take i jj
      addPreviousTouches touches i = sumPreviousTouches touches (countPreviousCompatible i) : touches
   in last . reverse $ foldl addPreviousTouches [] indexes

-- 2450
solution1 :: Input -> Int
solution1 input =
  let adapters = parseOrDie inputParser input
      sortedAdapters = sort adapters
      oneJolts = countJoltDifferences 1 $ concat [[output], sortedAdapters, [device adapters]]
      threeJolts = countJoltDifferences 3 $ concat [[output], sortedAdapters, [device adapters]]
   in oneJolts * threeJolts

-- 32396521357312
solution2 :: Input -> Int
solution2 input =
  let adapters = parseOrDie inputParser input
      sortedAdapters = output : sort adapters ++ [device adapters]
   in countValidConfigurations sortedAdapters

solution :: Solution
solution = PureSolution solution1 2450 solution2 32396521357312
