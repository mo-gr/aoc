{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2018.AOC3 where

import AOC (Solution (PureSolution))
import Control.Lens (makeLenses, view)
import Control.Monad (guard)
import Data.List (delete, group, sort)
import qualified Data.Set as S
import Text.Parsec (char, many1, newline, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie)

type Point = (Int, Int)

data Claim = Claim
  { _claimId :: Int,
    _area :: S.Set Point
  }
  deriving (Show, Eq)

makeLenses ''Claim

inputParser :: Parser [Claim]
inputParser = many1 $ do
  cid <- char '#' *> number <* string " @ "
  sx <- number
  sy <- char ',' *> number <* string ": "
  w <- number
  h <- char 'x' *> number <* newline
  pure $
    Claim cid $
      S.fromList $ do
        cx <- (sx +) <$> [0 .. pred w]
        cy <- (sy +) <$> [0 .. pred h]
        pure (cx, cy)

solve1 :: [Claim] -> Int
solve1 cs =
  length
    . fmap head -- take the first of each duplicate
    . filter ((> 1) . length) -- keep only duplicates
    . group
    . sort -- all points sorted
    $ concatMap (S.toList . view area) cs

-- 113716
solution1 :: Input -> Int
solution1 input =
  solve1 $
    parseOrDie inputParser input

solve2 :: [Claim] -> Int
solve2 cs = view claimId $
  head $ do
    c <- cs
    let cs' = delete c cs
    guard $ all (S.disjoint (view area c) . view area) cs'
    pure c

-- 742
solution2 :: Input -> Int
solution2 input =
  solve2 $
    parseOrDie inputParser input

solution :: Solution
solution = PureSolution solution1 113716 solution2 742
