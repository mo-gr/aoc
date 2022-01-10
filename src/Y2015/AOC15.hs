{-# LANGUAGE NamedFieldPuns #-}

module Y2015.AOC15 where

import AOC (Solution (PureSolution))
import Control.Monad (guard)
import qualified Data.Map.Strict as M
import Text.Parsec (letter, many1, newline, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, parseOrDie, (|>))

type Recipe = M.Map Ingredient Int

data Ingredient = Ingredient
  { capacity :: Int,
    durability :: Int,
    flavor :: Int,
    texture :: Int,
    calories :: Int
  }
  deriving (Show, Eq, Ord)

inputParser :: Parser [Ingredient]
inputParser = many1 $ do
  _name <- many1 letter <* string ": "
  cap <- string "capacity " *> negativeNumber <* string ", "
  dur <- string "durability " *> negativeNumber <* string ", "
  fla <- string "flavor " *> negativeNumber <* string ", "
  tex <- string "texture " *> negativeNumber <* string ", "
  cal <- string "calories " *> negativeNumber <* newline
  pure $ Ingredient cap dur fla tex cal

recipeScore :: Recipe -> Int
recipeScore is = productT4 . negToZeroT4 $ M.foldlWithKey f (0, 0, 0, 0) is
  where
    productT4 (x, y, z, a) = x * y * z * a
    negToZeroT4 (x, y, z, a) = (max 0 x, max 0 y, max 0 z, max 0 a)
    f (cap, dur, fla, tex) Ingredient {capacity, durability, flavor, texture} count =
      (cap + (capacity * count), dur + (durability * count), fla + (flavor * count), tex + (texture * count))

possibleRecipies :: [Ingredient] -> [Recipe]
possibleRecipies is = do
  amount1 <- [0 .. 100]
  amount2 <- [0 .. 100]
  amount3 <- [0 .. 100]
  amount4 <- [0 .. 100]
  guard $ (amount1 + amount2 + amount3 + amount4) == 100
  pure $ M.fromList [(head is, amount1), (is !! 1, amount2), (is !! 2, amount3), (is !! 3, amount4)]

totalCal500 :: Recipe -> Bool
totalCal500 r = 500 == M.foldlWithKey f 0 r
  where
    f acc Ingredient {calories} count = acc + (calories * count)

-- 13882464
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> possibleRecipies
    |> fmap recipeScore
    |> maximum

-- 11171160
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> possibleRecipies
    |> filter totalCal500
    |> fmap recipeScore
    |> maximum

solution :: Solution
solution = PureSolution solution1 13882464 solution2 11171160
