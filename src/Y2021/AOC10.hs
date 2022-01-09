module Y2021.AOC10 where

import AOC (Solution (PureSolution))
import Data.List (sort)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (many, newline, oneOf)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

data ParseState = P | S | B | A deriving (Show, Eq)

data ParseResult = Incomplete [ParseState] | Corrupted Char | Valid deriving (Show, Eq)

inputParser :: Parser [String]
inputParser = many (many (oneOf "(){}[]<>") <* newline)

consume :: [ParseState] -> String -> ParseResult
consume [] [] = Valid
consume ps [] = Incomplete ps
consume (ps : pRest) (')' : rest) = if ps == P then consume pRest rest else Corrupted ')'
consume (ps : pRest) (']' : rest) = if ps == S then consume pRest rest else Corrupted ']'
consume (ps : pRest) ('}' : rest) = if ps == B then consume pRest rest else Corrupted '}'
consume (ps : pRest) ('>' : rest) = if ps == A then consume pRest rest else Corrupted '>'
consume pState ('(' : rest) = consume (P : pState) rest
consume pState ('[' : rest) = consume (S : pState) rest
consume pState ('{' : rest) = consume (B : pState) rest
consume pState ('<' : rest) = consume (A : pState) rest
consume p s = error $ "Something went wrong " ++ s ++ " " ++ show p

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137
points _ = error "Something went wrong"

completePoints :: ParseState -> Int
completePoints P = 1
completePoints S = 2
completePoints B = 3
completePoints A = 4

completeScore :: ParseResult -> Int
completeScore (Incomplete state) = foldl pointsAcc 0 state
  where
    pointsAcc :: Int -> ParseState -> Int
    pointsAcc sc p = (sc * 5) + completePoints p
completeScore _ = 0

score :: [ParseResult] -> Int
score pr = sum $ fmap points' pr
  where
    points' (Corrupted c) = points c
    points' _ = 0

mid :: [Int] -> Int
mid xs = xs !! (length xs `div` 2)

-- 358737
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> fmap (consume [])
    |> score

-- 4329504793
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> fmap (consume [])
    |> fmap completeScore
    |> filter (/= 0)
    |> sort
    |> mid

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 358737 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 4329504793 . solution2 =<< input
    ]

solution :: Solution
solution = PureSolution solution1 solution2 verify
