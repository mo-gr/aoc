{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC21 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Control.Monad (guard)
import qualified Data.Map.Strict as M
import Text.Parsec (letter, many, newline, oneOf, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Monkey
  = Num Int
  | Plus String String
  | Mul String String
  | Subtract String String
  | Divide String String

inputParser :: Parser (M.Map String Monkey)
inputParser = fmap M.fromList $
  many $ do
    name <- many letter <* string ": "
    op <-
      (Num <$> number)
        <|> opParser
    _ <- newline
    pure (name, op)
  where
    opParser = do
      lhs <- many letter <* string " "
      op <- oneOf "+-*/"
      rhs <- string " " *> many letter
      pure $ case op of
        '+' -> Plus lhs rhs
        '*' -> Mul lhs rhs
        '-' -> Subtract lhs rhs
        '/' -> Divide lhs rhs
        _ -> error "unexpected op"

eval :: M.Map String Monkey -> Monkey -> Int
eval _ms (Num x) = x
eval ms (Plus x y) = eval ms (ms M.! x) + eval ms (ms M.! y)
eval ms (Mul x y) = eval ms (ms M.! x) * eval ms (ms M.! y)
eval ms (Subtract x y) = eval ms (ms M.! x) - eval ms (ms M.! y)
eval ms (Divide x y) = eval ms (ms M.! x) `div` eval ms (ms M.! y)

solve1 :: M.Map String Monkey -> Int
solve1 ms = eval ms (ms M.! "root")

-- 142707821472432
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> solve1

eval2 :: M.Map String Monkey -> String -> Int -> Int
eval2 _ "humn" human = human
eval2 ms "root" human = case ms M.! "root" of
  Plus l r -> if eval2 ms l human == eval2 ms r human then 1 else 0
  _ -> error "invalid root"
eval2 ms monkeyName human = case ms M.! monkeyName of
  Num x -> x
  Plus x y -> eval2 ms x human + eval2 ms y human
  Mul x y -> eval2 ms x human * eval2 ms y human
  Subtract x y -> eval2 ms x human - eval2 ms y human
  Divide x y -> eval2 ms x human `div` eval2 ms y human

solve2 :: M.Map String Monkey -> Int
solve2 ms = head $ do
  hmn <- [3587647550000 ..] -- trial and error
  guard $ eval2 ms "root" hmn == 1
  pure hmn

-- 3587647562851
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve2

solution :: Solution
solution = PureSolution solution1 142707821472432 solution2 3587647562851
