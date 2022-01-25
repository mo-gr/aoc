{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC2 where

import AOC (Solution (PureSolution))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))
import Text.Parsec (many1, char, newline)
import Data.Functor (($>))
import Control.Applicative ((<|>))

data Dir = U | D | L | R

move9 :: Int -> Dir -> Int
move9 n D | n > 6 = n
move9 n D = n + 3
move9 n U | n < 4  = n
move9 n U = n - 3
move9 1 L = 1
move9 4 L = 4
move9 7 L = 7
move9 n L = n - 1
move9 3 R = 3
move9 6 R = 6
move9 9 R = 9
move9 n R = n + 1

move13 :: Int -> Dir -> Int
move13 1 D = 3
move13 1 _ = 1
move13 5 R = 6
move13 5 _ = 5
move13 9 L = 8
move13 9 _ = 9
move13 0xD U = 0xB
move13 0xD _ = 0xD
move13 4 R = 4
move13 0xC R = 0xC
move13 n R = n + 1
move13 2 L = 2
move13 0xA L = 0xA
move13 n L = n - 1
move13 2 U = 2
move13 3 U = 1
move13 4 U = 4
move13 n U = n - 4
move13 0xA D = 0xA
move13 0xB D = 0xD
move13 0xC D = 0xC
move13 n D = n + 4

solve :: ( Int -> Dir -> Int) -> [[Dir]] -> [Int]
solve move = cleanup . foldl f [5]
  where f ns@(n:_) dirs = foldl move n dirs : ns
        f _ _ = error "something went wrong"
        cleanup = tail . reverse

show13 :: Int -> String
show13 0xA = "A"
show13 0xB = "B"
show13 0xC = "C"
show13 0xD = "D"
show13 n = show n

inputParser :: Parser [[Dir]]
inputParser = many1 $
  many1 ((char 'L' $> L) <|>  (char 'U' $> U) <|> (char 'D' $> D) <|> (char 'R' $> R)) <* newline

-- 36629
solution1 :: Input -> String
solution1 input =
  parseOrDie inputParser input
    |> solve move9
    |> fmap show
    |> mconcat

-- 99C3D
solution2 :: Input -> String
solution2 input =
  parseOrDie inputParser input
    |> solve move13
    |> fmap show13
    |> mconcat

solution :: Solution
solution = PureSolution solution1 "36629" solution2 "99C3D"
