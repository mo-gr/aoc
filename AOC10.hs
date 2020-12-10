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

tired :: [Int] -> Int -> Int
tired [] _ = 1
tired (0:_x) _ = 1
tired (x:_x) 1 = x
tired (x:y:_x) 2 = x + y
tired (x:y:z:_x) 3 = x + y + z
tired _ _ = error "go to bed"

countValidConfigurations :: [Jolts] -> Int
countValidConfigurations jj = let touches = [] in
  last . reverse $ foldl (\t i -> (: t) $ tired t (length . take 3 . filter (`isCompatible` (jj !! i)) $ take i jj)) touches [0..(length jj - 1)]

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
