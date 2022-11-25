{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC21 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List (transpose)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, newline, string)
import Text.Parsec.Combinator (count, many1)
import Text.Parsec.Prim (try)
import Util (Input, parseOrDie, times, (|>))

data Pixel = On | Off
  deriving (Eq, Ord)

type PixelMap = [[Pixel]]

type RuleBook = M.Map PixelMap PixelMap

instance Show Pixel where
  show On = "#"
  show Off = "."

pretty :: PixelMap -> String
pretty [] = "\n"
pretty (r : rest) = concatMap show r <> "\n" <> pretty rest

start :: PixelMap
start = [[Off, On, Off], [Off, Off, On], [On, On, On]]

inputParser :: Parser RuleBook
inputParser = M.fromList <$> many1 (try rule3P <|> rule2P)
  where
    rule2P, rule3P :: Parser (PixelMap, PixelMap)
    rule3P = do
      p1 <- count 3 pixelP <* char '/'
      p2 <- count 3 pixelP <* char '/'
      p3 <- count 3 pixelP
      _ <- string " => "
      o1 <- count 4 pixelP <* char '/'
      o2 <- count 4 pixelP <* char '/'
      o3 <- count 4 pixelP <* char '/'
      o4 <- count 4 pixelP <* newline
      pure ([p1, p2, p3], [o1, o2, o3, o4])
    rule2P = do
      p1 <- count 2 pixelP <* char '/'
      p2 <- count 2 pixelP
      _ <- string " => "
      o1 <- count 3 pixelP <* char '/'
      o2 <- count 3 pixelP <* char '/'
      o3 <- count 3 pixelP <* newline
      pure ([p1, p2], [o1, o2, o3])
    pixelP = char '#' $> On <|> char '.' $> Off

flipTransform, rotateTransform :: PixelMap -> PixelMap
flipTransform = fmap reverse
rotateTransform [] = []
rotateTransform (b : _) | null b = []
rotateTransform bitmap = reverse (fmap head bitmap) : rotateTransform (fmap tail bitmap)

permutations :: PixelMap -> [PixelMap]
permutations pm =
  [ pm,
    rotateTransform pm,
    rotateTransform (rotateTransform pm),
    rotateTransform (rotateTransform (rotateTransform pm)),
    flipTransform pm,
    rotateTransform (flipTransform pm),
    rotateTransform (rotateTransform (flipTransform pm)),
    rotateTransform (rotateTransform (rotateTransform (flipTransform pm)))
  ]

joinPm :: [[PixelMap]] -> PixelMap
joinPm pm = cutEvery ((length . head) pm * (length . head . head) pm) . concat . concat . concatMap transpose $ pm

cutEvery :: Int -> [a] -> [[a]]
cutEvery _ [] = []
cutEvery n a = take n a : cutEvery n (drop n a)

split :: PixelMap -> [[PixelMap]]
split [] = []
split pm | length (head pm) <= 3 = [[pm]]
split pm@(p1 : p2 : prest)
  | even (length (head pm)) =
    (toPm <$> zip (cutEvery 2 p1) (cutEvery 2 p2)) : split prest
  where
    toPm (p, pp) = [p, pp]
split pm@(p1 : p2 : p3 : prest)
  | length (head pm) `mod` 3 == 0 =
    (toPm <$> zip3 (cutEvery 3 p1) (cutEvery 3 p2) (cutEvery 3 p3)) : split prest
  where
    toPm (p, pp, ppp) = [p, pp, ppp]
split p = error $ "invalid split attempt for \n" <> pretty p

expand :: RuleBook -> PixelMap -> PixelMap
expand ruleBook pixel = joinPm expanded
  where
    subMaps :: [[PixelMap]]
    subMaps = split pixel
    expanded :: [[PixelMap]]
    expanded = (fmap . fmap) replace subMaps
    replace :: PixelMap -> PixelMap
    replace p = case catMaybes $ flip M.lookup ruleBook <$> permutations p of
      [] -> error $ "no replacement for \n" <> pretty p
      rp : _ -> rp

countPixel :: PixelMap -> Int
countPixel = length . filter (== On) . concat

-- 147
solution1 :: Input -> Int
solution1 input = times 5 (expand ruleBook) start |> countPixel
  where
    ruleBook = parseOrDie inputParser input

-- 1936582
solution2 :: Input -> Int
solution2 input = times 18 (expand ruleBook) start |> countPixel
  where
    ruleBook = parseOrDie inputParser input

solution :: Solution
solution = PureSolution solution1 147 solution2 1936582
