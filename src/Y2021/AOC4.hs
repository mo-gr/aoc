module Y2021.AOC4 where

import AOC (Solution (PureSolution))
import Data.List (find, transpose)
import Data.Maybe (isJust)
import qualified Data.Set as S
import Text.Parsec (char, count, many, newline, sepBy1, space, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Number = Int

type BingoField = [Number]

bingoNumbersParser :: Parser [Number]
bingoNumbersParser = sepBy1 number $ char ','

bingoFieldParser :: Parser BingoField
bingoFieldParser = mconcat <$> count 5 (count 5 $ many space *> number)

inputParser :: Parser ([Number], [BingoField])
inputParser = do
  nums <- bingoNumbersParser <* newline <* newline
  fields <- many $ try bingoFieldParser
  pure (nums, fields)

rows :: BingoField -> [[Number]]
rows (x1 : x2 : x3 : x4 : x5 : xs) = [x1, x2, x3, x4, x5] : rows xs
rows [] = []
rows _ = error "something failed"

cols :: BingoField -> [[Number]]
cols = transpose . rows

hasWon :: [Number] -> BingoField -> Bool
hasWon nums field =
  let rows' = rows field
      cols' = cols field
      isComplete :: [Number] -> Bool
      isComplete xs = S.intersection (S.fromList xs) (S.fromList nums) |> length |> (== 5)
   in isJust . find id $ (isComplete <$> rows') ++ (isComplete <$> cols')

playTilWinner :: [Number] -> ([Number], [BingoField]) -> (BingoField, [Number])
playTilWinner _ ([], _) = error "no winner"
playTilWinner ms (n : ns, fields) = case find (hasWon (n : ms)) fields of
  Just winner -> (winner, n : ms)
  _ -> playTilWinner (n : ms) (ns, fields)

playTilLoser :: [Number] -> Maybe (BingoField, [Number]) -> ([Number], [BingoField]) -> (BingoField, [Number])
playTilLoser _ (Just lw) ([], _) = lw
playTilLoser _ Nothing ([], _) = error "no loser"
playTilLoser ms lw (n : ns, fields) = case filter (hasWon (n : ms)) fields of
  [] -> playTilLoser (n : ms) lw (ns, fields)
  winner -> playTilLoser (n : ms) (Just (head winner, n : ms)) (ns, filter (`notElem` winner) fields)

calculateScore :: (BingoField, [Number]) -> Int
calculateScore (field, nums@(n : _)) =
  S.fromList field S.\\ S.fromList nums
    |> sum
    |> (* n)
calculateScore _ = error "something went wrong"

-- 8580
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> playTilWinner []
    |> calculateScore

-- 9576
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> playTilLoser [] Nothing
    |> calculateScore

solution :: Solution
solution = PureSolution solution1 8580 solution2 9576
