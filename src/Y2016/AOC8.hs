{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC8 where

import AOC (Solution (IOSolution))
import Control.Applicative ((<|>))
import Data.Functor (($>), (<&>))
import qualified Data.Set as S
import Text.Parsec (many1, newline, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Cmd' a
  = Rect a a
  | RotRow a a
  | RotCol a a
  deriving (Show)

type Cmd = Cmd' Int

type Coord = (Int, Int)

type Screen = S.Set Coord

emptyScreen :: Screen
emptyScreen = S.empty

pointsInRect :: Int -> Int -> Screen
pointsInRect w h = S.fromList $ do
  y <- [0 .. (pred h)]
  x <- [0 .. (pred w)]
  pure (x, y)

shiftRow :: Int -> Screen -> Screen
shiftRow row = foldr f emptyScreen
  where
    f (x, y) scr' | y == row = S.insert (succPixel x, y) scr'
    f p scr' = S.insert p scr'
    succPixel 49 = 0
    succPixel n = n + 1

shiftCol :: Int -> Screen -> Screen
shiftCol col = foldr f emptyScreen
  where
    f (x, y) scr' | x == col = S.insert (x, succPixel y) scr'
    f p scr' = S.insert p scr'
    succPixel 5 = 0
    succPixel n = n + 1

apply :: Cmd -> Screen -> Screen
apply (Rect w h) scr = S.union scr $ pointsInRect w h
apply (RotRow _ 0) scr = scr
apply (RotRow r n) scr = apply (RotRow r (pred n)) $ shiftRow r scr
apply (RotCol _ 0) scr = scr
apply (RotCol c n) scr = apply (RotCol c (pred n)) $ shiftCol c scr

pretty :: Screen -> IO ()
pretty screen = sequence_ $ do
  y <- [0 .. 5]
  x <- [0 .. 50]
  let prnt = if x == 50 then putStrLn else putStr
  pure $ if S.member (x, y) screen then prnt "#" else prnt "."

inputParser :: Parser [Cmd]
inputParser = many1 $ command <* newline
  where
    command, rect, rotRow, rotCol :: Parser Cmd
    command = try rect <|> try rotRow <|> rotCol
    rect = string "rect " *> number >>= (\x -> (string "x" *> number) <&> Rect x)
    rotRow = string "rotate row y=" *> number >>= (\x -> (string " by " *> number) <&> RotRow x)
    rotCol = string "rotate column x=" *> number >>= (\x -> (string " by " *> number) <&> RotCol x)

-- 110
solution1 :: Input -> IO String
solution1 input =
  parseOrDie inputParser input
    |> reverse
    |> foldr apply emptyScreen
    |> show . length
    |> pure

-- ZJHRKCPLYJ
solution2 :: Input -> IO String
solution2 input =
  ( parseOrDie inputParser input
      |> reverse
      |> foldr apply emptyScreen
      |> pretty
  )
    $> "ZJHRKCPLYJ"

testData :: Input
testData = "rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1\n"

solution :: Solution
solution = IOSolution solution1 "110" solution2 "ZJHRKCPLYJ"
