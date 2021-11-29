module Y2015.AOC2 where

import Text.Parsec.ByteString (Parser, parseFromFile)
import Util (number)
import Text.Parsec (char, sepBy1)
import Data.Either (fromRight)
import Data.List (sort)


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
boxSurface (l,w,h) = 2*l*w + 2*w*h + 2*h*l

boxSurplus :: Box -> Int
boxSurplus (l,w,h) = minimum [l*w, w*h, h*l]

boxPaper :: Box -> Int
boxPaper box = boxSurface box + boxSurplus box

bow :: Box -> Int
bow (l,w,h) = l * w * h

wrap :: Box -> Int
wrap (l,w,h) = let [a,b,_] = sort [l,w,h] in a + a + b + b

ribbon :: Box -> Int
ribbon box = bow box + wrap box

-- 1588178
solution1 :: IO Int
solution1 = do
  boxes <- fromRight [] <$> parseFromFile inputParser "data/2015/AOC2.input"
  return $ sum $ map boxPaper boxes

-- 3783758
solution2 :: IO Int
solution2 = do
  boxes <- fromRight [] <$> parseFromFile inputParser "data/2015/AOC2.input"
  return $ sum $ map ribbon boxes
