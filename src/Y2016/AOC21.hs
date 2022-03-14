{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC21 (solution) where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Control.Monad (msum)
import Data.List (elemIndex)
import Text.Parsec (letter, many1, newline, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Operation = String -> String

data Op = SwapPos Int Int | Rotate Int | SwapLetter Char Char | RotatePos Char | MovePos Int Int | ReversePos Int Int

applyOp :: Op -> Operation
applyOp (ReversePos n m) = reversePos n m
applyOp (MovePos n m) = movePos n m
applyOp (RotatePos n) = rotatePos n
applyOp (SwapLetter n m) = swapLetter n m
applyOp (Rotate n) = rotate n
applyOp (SwapPos n m) = swapPos n m

reverseOp :: Op -> Operation
reverseOp (MovePos n m) = movePos m n
reverseOp (Rotate n) = rotate (negate n)
reverseOp (SwapLetter n m) = swapLetter n m
reverseOp (ReversePos n m) = reversePos n m
reverseOp (SwapPos n m) = swapPos n m
reverseOp (RotatePos n) = bruteReverseRotate n

bruteReverseRotate :: Char -> Operation
bruteReverseRotate c str = head . filter (\b -> str == rotatePos c b) $ flip rotate str <$> [0 ..]

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

inputParser :: Parser [Op]
inputParser = many1 operation
  where
    operation = msum [try rotRightP, try rotLeftP, try swapP, try reverseP, try moveP, try swapLetterP, rotPosP] <* newline
    rotRightP = Rotate <$> (string "rotate right " *> number <* (try (string " steps") <|> string " step"))
    rotLeftP = Rotate . negate <$> (string "rotate left " *> number <* (try (string " steps") <|> string " step"))
    swapP = do
      _ <- string "swap position "
      p <- number
      _ <- string " with position "
      SwapPos p <$> number
    reverseP = do
      _ <- string "reverse positions "
      p <- number
      _ <- string " through "
      ReversePos p <$> number
    moveP = do
      _ <- string "move position "
      p <- number
      _ <- string " to position "
      MovePos p <$> number
    swapLetterP = do
      _ <- string "swap letter "
      p <- letter
      _ <- string " with letter "
      SwapLetter p <$> letter
    rotPosP = RotatePos <$> (string "rotate based on position of letter " *> letter)

-- gcedfahb
solution1 :: Input -> String
solution1 input =
  parseOrDie inputParser input
    |> foldl (flip applyOp) "abcdefgh"

-- hegbdcfa
solution2 :: Input -> String
solution2 input =
  parseOrDie inputParser input
    |> foldr reverseOp "fbgdceah"

solution :: Solution
solution = PureSolution solution1 "gcedfahb" solution2 "hegbdcfa"
