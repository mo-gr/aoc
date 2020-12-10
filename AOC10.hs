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

countValidConfigurations'' :: [Jolts] -> Int
countValidConfigurations'' js = length $ validConfigurations js

--  const numTouches = new Array(input.length).fill(0);
--	numTouches[0] = 1;
--	for (let i = 0; i < input.length - 1; i++) {
--		for (let j = i + 1; j < input.length; j++) {
--			if (input[j] - input[i] > 3) break;
--			numTouches[j] += numTouches[i];
--		}
--	}
--	return numTouches[numTouches.length - 1];

safeHead :: [Int] -> Int
safeHead [] = 1
safeHead (0:_x) = 1
safeHead x = head x

tired :: [Int] -> Int -> Int
tired [] _ = 1
tired (0:_x) _ = 1
tired (x:_x) 1 = x
tired (x:y:_x) 2 = x + y
tired (x:y:z:_x) 3 = x + y + z
tired _ _ = error "go to bed"

cVC :: [Jolts] -> [Int]
cVC jj = let touches = [] in
  reverse $ foldl (\t i -> (: t) $ tired t (length . take 3 . filter (`isCompatible` (jj !! i)) $ take i jj)) touches [0..(length jj - 1)]

countValidConfigurationsMemo :: [Jolts] -> Int -> Int
countValidConfigurationsMemo jj idx' = (f <$> [0..length jj - 1]) !! idx'
  where f idx | idx >= length jj - 1 = 1
        f idx = let candidates = filter (isCompatible (jj !! idx)) (drop idx jj) in
                 case length candidates of
                   1 -> countValidConfigurationsMemo jj (idx + 1)
                   2 -> countValidConfigurationsMemo jj (idx + 1) + countValidConfigurationsMemo jj (idx + 2)
                   _ -> countValidConfigurationsMemo jj (idx + 1) + countValidConfigurationsMemo jj (idx + 2) + countValidConfigurationsMemo jj (idx + 3)

countValidConfigurations :: [Jolts] -> Int -> Int
countValidConfigurations jj idx | idx >= length jj - 1 = 1
countValidConfigurations jj idx = let candidates = filter (isCompatible (jj !! idx)) (drop idx jj) in
 case length candidates of
   1 -> countValidConfigurations jj (idx + 1)
   2 -> countValidConfigurations jj (idx + 1) + countValidConfigurations jj (idx + 2)
   _ -> countValidConfigurations jj (idx + 1) + countValidConfigurations jj (idx + 2) + countValidConfigurations jj (idx + 3)

countValidConfigurations' :: [Jolts] -> Int
countValidConfigurations' [] = 0
countValidConfigurations' [_j] = 1
countValidConfigurations' (j:jj) = let candidates = filter (isCompatible j) jj in
 case length candidates of
   1 -> countValidConfigurations' jj
   2 -> countValidConfigurations' jj + countValidConfigurations' (tail jj)
   _ -> countValidConfigurations' jj + countValidConfigurations' (tail jj) + countValidConfigurations' (tail $ tail jj)

validConfigurations :: [Jolts] -> [[Jolts]]
validConfigurations [] = [[]]
validConfigurations [x] = [[x]]
validConfigurations (outp:others) = do
    let candidates = filter (isCompatible outp) others
    options <- (`dropTil` others) <$> candidates
    next <- validConfigurations options
    return $ outp : next


example :: [Jolts]
example = [0,1,4,5,6,7,10,11,12,15,16,19,22]

-- 2450
solution1 :: IO Int
solution1 = do
  adapters <- fromRight [] <$> parseFromFile inputParser "AOC10.input"
  let sortedAdapters = sort adapters
  let oneJolts = countJoltDifferences 1 $ concat [[output], sortedAdapters, [device adapters]]
  let threeJolts = countJoltDifferences 3 $ concat [[output], sortedAdapters, [device adapters]]
  return $ oneJolts * threeJolts

solution2 :: IO Int
solution2 = do
  adapters <- fromRight [] <$> parseFromFile inputParser "AOC10.input"
  let sortedAdapters = output : sort adapters ++ [device adapters]
  return $ head . reverse $ cVC sortedAdapters
