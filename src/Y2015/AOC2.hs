module Y2015.AOC2 where

import AOC (Solution (PureSolution))
import Data.List (sort)
import Text.Parsec (char, sepBy1)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Box = (Int, Int, Int)

boxParser :: Parser Box
boxParser = do
  l <- number
  _ <- char 'x'
  w <- number
  _ <- char 'x'
  h <- number
  return (l, w, h)

inputParser :: Parser [Box]
inputParser = boxParser `sepBy1` char '\n'

boxSurface :: Box -> Int
boxSurface (l, w, h) = 2 * l * w + 2 * w * h + 2 * h * l

boxSurplus :: Box -> Int
boxSurplus (l, w, h) = minimum [l * w, w * h, h * l]

boxPaper :: Box -> Int
boxPaper box = boxSurface box + boxSurplus box

bow :: Box -> Int
bow (l, w, h) = l * w * h

wrap :: Box -> Int
wrap (l, w, h) = case sort [l, w, h] of
  [a, b, _] -> a + a + b + b
  _ -> error "something went wrong"

ribbon :: Box -> Int
ribbon box = bow box + wrap box

-- 1588178
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> map boxPaper
    |> sum

-- 3783758
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> map ribbon
    |> sum

solution :: Solution
solution = PureSolution solution1 1588178 solution2 3783758
