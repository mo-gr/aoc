{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC21 (solution) where

import AOC (Solution (PureSolution))
import Data.List (elemIndex)
import Text.Parsec (many1, newline, string, letter, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))
import Control.Monad (msum)
import Control.Applicative ((<|>))

type Operation = String -> String

swapPos :: Int -> Int -> Operation
swapPos x y inp =
  zipWith
    ( \idx c ->
        if idx == x
          then inp !! y
          else
            if idx == y
              then inp !! x
              else c
    )
    [0 ..]
    inp

swapLetter :: Char -> Char -> Operation
swapLetter _ _ [] = []
swapLetter x y (c : rst)
  | x == c = y : swapLetter x y rst
  | y == c = x : swapLetter x y rst
  | otherwise = c : swapLetter x y rst

-- positive rotates to the right
rotate :: Int -> Operation
rotate 0 inp = inp
rotate n inp
  | n < 0 = rotate (succ n) (tail inp <> [head inp])
  | n > 0 = rotate (pred n) (last inp : init inp)
rotate _ _ = error "unexpected rotation"

rotatePos :: Char -> Operation
rotatePos c inp = case elemIndex c inp of
  Nothing -> inp
  Just idx
    | idx >= 4 -> rotate (1 + idx + 1) inp
    | otherwise -> rotate (1 + idx) inp

reversePos :: Int -> Int -> Operation
reversePos x y inp = take x inp <> (reverse . take (succ y - x) . drop x) inp <> drop (succ y) inp

movePos :: Int -> Int -> Operation
movePos x y inp | x == y = inp
movePos x y inp = recur 0 inp
  where
    recur idx rst
      | idx >= length inp = []
      | idx == x = recur (succ idx) (tail rst)
      | idx == y && x > y = inp !! x : head rst : recur (succ idx) (tail rst)
      | idx == y = head rst : inp !! x : recur (succ idx) (tail rst)
      | otherwise = head rst : recur (succ idx) (tail rst)

inputParser :: Parser [Operation]
inputParser = many1 operation
  where
    operation = msum [try rotRightP, try rotLeftP, try swapP, try reverseP, try moveP, try swapLetterP, rotPosP] <* newline
    rotRightP = rotate <$> (string "rotate right " *> number <* (try (string " steps") <|> string " step"))
    rotLeftP = rotate . negate <$> (string "rotate left " *> number <* (try (string " steps") <|> string " step"))
    swapP = do
      _ <- string "swap position "
      p <- number
      _ <- string " with position "
      swapPos p <$> number
    reverseP = do
      _ <- string "reverse positions "
      p <- number
      _ <- string " through "
      reversePos p <$> number
    moveP = do
      _ <- string "move position "
      p <- number
      _ <-string " to position "
      movePos p <$> number
    swapLetterP = do
      _ <- string "swap letter "
      p <- letter
      _ <- string " with letter "
      swapLetter p <$> letter
    rotPosP = rotatePos <$> (string "rotate based on position of letter " *> letter)

-- gcedfahb
solution1 :: Input -> String
solution1 input =
  parseOrDie inputParser input
    |> foldl (\acc op -> op acc) "abcdefgh" 

solution2 :: Input -> String
solution2 input =
  parseOrDie inputParser input
    |> error "not yet"

testData :: Input
testData = "reverse positions 2 through 5\nrotate based on position of letter d\nreverse positions 1 through 7\nrotate right 1 step\n"

example :: Operation
example inp =
  inp
    |> swapPos 4 0
    |> swapLetter 'd' 'b'
    |> reversePos 0 4
    |> rotate (-1)
    |> movePos 1 4
    |> movePos 3 0
    |> rotatePos 'b'
    |> rotatePos 'd'

solution :: Solution
solution = PureSolution solution1 "gcedfahb" solution2 undefined
