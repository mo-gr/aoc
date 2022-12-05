{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC5 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Text.Parsec (char, digit, letter, many1, newline, sepBy, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type CrateId = Int

type Crates = M.Map CrateId [Char]

type Move = (Int, CrateId, CrateId)

inputParser :: Parser (Crates, [Move])
inputParser = do
  c <- crateP <* newline
  ms <- many1 $ do
    mc <- string "move " *> number
    mf <- string " from " *> number
    mt <- string " to " *> number <* newline
    pure (mc, mf, mt)
  pure (c, ms)

crateP :: Parser Crates
crateP = do
  ls <- lineP `sepBy` newline
  _ <- many1 (char ' ' <|> digit) <* newline
  pure $ toCrate ls
  where
    lineP :: Parser [Maybe Char]
    lineP = (string "   " $> Nothing <|> char '[' *> (Just <$> letter) <* char ']') `sepBy` char ' '
    toCrate :: [[Maybe Char]] -> Crates
    toCrate = foldl insertLine M.empty
    insertLine :: Crates -> [Maybe Char] -> Crates
    insertLine cs l = foldl insertWithIndex cs $ zip [1 ..] l
    insertWithIndex :: Crates -> (Int, Maybe Char) -> Crates
    insertWithIndex cs (_, Nothing) = cs
    insertWithIndex cs (i, Just c) = M.insertWith (flip (<>)) i [c] cs

topCrates :: Crates -> String
topCrates = fmap (head . snd) . M.toList

moveCrates, moveCrates9001 :: Crates -> Move -> Crates
moveCrates crates (cnt, from, to) =
  M.insert from rest crates
    |> M.adjust (reverse grab <>) to
  where
    (grab, rest) = splitAt cnt (crates M.! from)
moveCrates9001 crates (cnt, from, to) =
  M.insert from rest crates
    |> M.adjust (grab <>) to
  where
    (grab, rest) = splitAt cnt (crates M.! from)

-- CFFHVVHNC
solution1 :: Input -> String
solution1 input =
  parseOrDie inputParser input
    |> uncurry (foldl moveCrates)
    |> topCrates

-- FSZWBPTBG
solution2 :: Input -> String
solution2 input =
  parseOrDie inputParser input
    |> uncurry (foldl moveCrates9001)
    |> topCrates

solution :: Solution
solution = PureSolution solution1 "CFFHVVHNC" solution2 "FSZWBPTBG"