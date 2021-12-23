{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC22 where

import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (many1, newline, string, try, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, parseOrDie, (|>))
import Data.Foldable (foldr')

type Point = (Int, Int, Int)

data Cube = Cube
  { enabled :: Bool,
    start :: Point,
    end :: Point
  }
  deriving (Eq, Show, Ord)

cubeParser :: Parser Cube
cubeParser = do
  cubeState <- try (string "on " $> True) <|> (string "off " $> False)
  xCubeStart <- string "x=" *> negativeNumber
  xCubeEnd <- string ".." *> negativeNumber <* string ","
  yCubeStart <- string "y=" *> negativeNumber
  yCubeEnd <- string ".." *> negativeNumber <* string ","
  zCubeStart <- string "z=" *> negativeNumber
  zCubeEnd <- string ".." *> negativeNumber <* newline
  pure $ Cube cubeState (xCubeStart, yCubeStart, zCubeStart) (xCubeEnd, yCubeEnd, zCubeEnd)

inputParser :: Parser [Cube]
inputParser = many1 cubeParser

pairwise :: (Int -> Int -> Int) -> Point -> Point -> Point
pairwise f (a, b, c) (a', b', c') = (f a a', f b b', f c c')

volume :: Cube -> Int
volume c = pairwise dist (start c) (end c) |> pointProduct
  where
    dist x x' = 1 + abs (x' - x)
    pointProduct (x, y, z) = x * y * z

noOverlap :: Cube -> Cube -> Bool
noOverlap c c' = isBefore (end c) (start c') || isBefore (end c') (start c)
  where
    isBefore :: Point -> Point -> Bool
    isBefore (x, y, z) (x', y', z') = x < x' || y < y' || z < z'

intersectionCube :: Cube -> Cube -> Maybe Cube
intersectionCube c c' = if noOverlap c c' then Nothing else
  Just $
    Cube
      { enabled = not $ enabled c',
        start = pairwise max (start c) (start c'),
        end = pairwise min (end c) (end c')
      }

overlapVolume :: Cube -> [Cube] -> Int
overlapVolume c cs = sum $ f <$> zip [1..] cs
  where f (i,cc) = case intersectionCube c cc of
                      Nothing -> 0
                      Just ic -> volume ic - overlapVolume ic (drop i cs)

runReverse :: [Cube] -> Int
runReverse cs = snd $ foldr' process ([], 0) cs
  where process :: Cube -> ([Cube], Int) -> ([Cube], Int)
        process c (acc, vol)| not $ enabled c = (c:acc, vol)
        process c (acc, vol) = (c:acc, vol + volume c - overlapVolume c acc)

countEnabled :: [Cube] -> Int
countEnabled [] = 0
countEnabled (c : cs) | enabled c = volume c + countEnabled cs
countEnabled (c : cs) = negate (volume c) + countEnabled cs

cropTo50 :: Cube -> Maybe Cube
cropTo50 c = intersectionCube (Cube (not $ enabled c) (-50, -50, -50) (50, 50, 50)) (c {enabled = not $ enabled c}) 

-- 588120
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> fmap cropTo50
    |> catMaybes
    |> runReverse

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> runReverse

-- 1134088247046731
verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 588120 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 1134088247046731 . solution2 =<< input
    ]

