{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC9 where

import Data.Char (digitToInt)
import Data.List (sort)
import qualified Data.Set as S
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (digit, many, many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

type Measurement = Int

type HeightMap = [[Measurement]]

type Point = (Int, Int)

type Basin = S.Set Point

len :: HeightMap -> Int
len (s : _) = length s
len _ = error "something went wrong"

height :: HeightMap -> Int
height = length

lineParser :: Parser [Measurement]
lineParser = do
  chars <- many1 digit
  pure $ digitToInt <$> chars

inputParser :: Parser HeightMap
inputParser = many1 (lineParser <* many newline)

getAt :: HeightMap -> Point -> Measurement
getAt hm (x, y) | x >= len hm = error $ show (x, y)
getAt hm (x, y) | y >= height hm = error $ show (x, y)
getAt hm (x, y) = (hm !! y) !! x

neighbours :: HeightMap -> Point -> [Point]
neighbours hm (x, y) = filter bounds [(x + 1, y), (x -1, y), (x, y + 1), (x, y -1)]
  where
    bounds (x', _) | x' < 0 || x' >= len hm = False
    bounds (_, y') | y' < 0 || y' >= height hm = False
    bounds _ = True

allPoints :: HeightMap -> [Point]
allPoints hm = do
  x <- [0 .. len hm - 1]
  y <- [0 .. height hm - 1]
  pure (x, y)

allNeighboursAreSmaller :: HeightMap -> Point -> Bool
allNeighboursAreSmaller hm p = let this = getAt hm p in
  neighbours hm p
    |> fmap (getAt hm)
    |> all (this <)
    

countLowPoints :: HeightMap -> Int
countLowPoints hm =
  lowPoints hm
    |> fmap ((+ 1) . getAt hm)
    |> sum

lowPoints :: HeightMap -> [Point]
lowPoints hm =
  allPoints hm
    |> filter (allNeighboursAreSmaller hm)

findBasin :: HeightMap -> Basin -> Point -> Basin
findBasin hm b p =
  neighbours hm p
    |> filter notAlreadyInBasin
    |> filter notCeiling
    |> recurse
  where
    notAlreadyInBasin :: Point -> Bool
    notAlreadyInBasin p' = not $ S.member p' b
    notCeiling :: Point -> Bool
    notCeiling p' = getAt hm p' /= 9
    recurse :: [Point] -> Basin
    recurse [] = b
    recurse ps = let basin = S.union b (S.fromList ps) in foldl (findBasin hm) basin ps

basins :: HeightMap -> [Basin]
basins hm = findBasin hm S.empty <$> lowPoints hm

threeBiggestBasins :: HeightMap -> [Int]
threeBiggestBasins hm = basins hm |> fmap S.toList |> fmap length |> sort |> reverse |> take 3

-- 518
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> countLowPoints

-- 949905
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> threeBiggestBasins
    |> product

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 518 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 949905 . solution2 =<< input
    ]
