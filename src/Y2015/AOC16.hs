module Y2015.AOC16 where

import AOC (Solution (PureSolution))
import Control.Monad (guard)
import qualified Data.Map.Strict as M
import Text.Parsec (letter, many1, newline, sepBy1, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Sue = (Int, M.Map String Int)

inputParser :: Parser [Sue]
inputParser = many1 $ do
  _ <- string "Sue "
  idx <- number <* string ": "
  props <- propertyParser `sepBy1` string ", "
  _ <- newline
  pure (idx, M.fromList props)
  where
    propertyParser = do
      pname <- many1 letter <* string ": "
      pcount <- number
      pure (pname, pcount)

filterRightSue :: [Sue] -> [Sue]
filterRightSue sues = do
  sue <- sues
  guard $ maybe True (== 3) $ M.lookup "children" (snd sue)
  guard $ maybe True (== 7) $ M.lookup "cats" (snd sue)
  guard $ maybe True (== 2) $ M.lookup "samoyeds" (snd sue)
  guard $ maybe True (== 3) $ M.lookup "pomeranians" (snd sue)
  guard $ maybe True (== 0) $ M.lookup "akitas" (snd sue)
  guard $ maybe True (== 0) $ M.lookup "vizslas" (snd sue)
  guard $ maybe True (== 5) $ M.lookup "goldfish" (snd sue)
  guard $ maybe True (== 3) $ M.lookup "trees" (snd sue)
  guard $ maybe True (== 2) $ M.lookup "cars" (snd sue)
  guard $ maybe True (== 1) $ M.lookup "perfumes" (snd sue)
  pure sue

filterRightSue' :: [Sue] -> [Sue]
filterRightSue' sues = do
  sue <- sues
  guard $ maybe True (== 3) $ M.lookup "children" (snd sue)
  guard $ maybe True (> 7) $ M.lookup "cats" (snd sue)
  guard $ maybe True (== 2) $ M.lookup "samoyeds" (snd sue)
  guard $ maybe True (< 3) $ M.lookup "pomeranians" (snd sue)
  guard $ maybe True (== 0) $ M.lookup "akitas" (snd sue)
  guard $ maybe True (== 0) $ M.lookup "vizslas" (snd sue)
  guard $ maybe True (< 5) $ M.lookup "goldfish" (snd sue)
  guard $ maybe True (> 3) $ M.lookup "trees" (snd sue)
  guard $ maybe True (== 2) $ M.lookup "cars" (snd sue)
  guard $ maybe True (== 1) $ M.lookup "perfumes" (snd sue)
  pure sue

-- 103
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> filterRightSue
    |> fst . head

-- 405
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> filterRightSue'
    |> fst . head

solution :: Solution
solution = PureSolution solution1 103 solution2 405
