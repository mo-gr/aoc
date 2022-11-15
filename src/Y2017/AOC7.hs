{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC7 where

import AOC (Solution (PureSolution))
import Control.Applicative (liftA2, (<|>))
import Data.List (sortOn)
import Data.Tree (Tree, rootLabel, subForest, unfoldTree)
import Text.Parsec (letter, many1, newline, sepBy, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Program = Program
  { name :: String,
    weight :: Int
  }
  deriving (Show, Eq)

type ProgramInput = (Program, [String])

inputParser :: Parser [ProgramInput]
inputParser = many1 $ do
  nme <- many1 letter
  wght <- string " (" *> number <* string ")"
  blnc <- try (string " -> " *> (many1 letter `sepBy` string ", ")) <|> pure []
  _ <- newline
  pure (Program nme wght, blnc)

toProgram :: [ProgramInput] -> Tree Program
toProgram pis = unfoldTree f (findRoot pis)
  where
    f :: ProgramInput -> (Program, [ProgramInput])
    f (p, bs) = (p, filter (byName bs) pis)
    byName names (p, _) = name p `elem` names
    discs :: [String]
    discs = concatMap snd pis
    findRoot :: [ProgramInput] -> ProgramInput
    findRoot = head . filter (\(p, _) -> name p `notElem` discs)

selfWeight :: Tree Program -> Int
selfWeight = weight . rootLabel

totalWeight :: Tree Program -> Int
totalWeight = liftA2 (+) selfWeight (sum . discWeights)

discWeights :: Tree Program -> [Int]
discWeights = fmap totalWeight . subForest

balanceWeight :: Tree Program -> Int
balanceWeight t = discWeights t |> liftA2 (-) maximum minimum

isBalanced :: Tree Program -> Bool
isBalanced tr | null (subForest tr) = True
isBalanced tr = balanceWeight tr == 0

unbalanced :: Tree Program -> [Tree Program]
unbalanced tr
  | balanceWeight tr == 0 = []
  | all isBalanced (subForest tr) = subForest tr
  | otherwise = concatMap unbalanced (subForest tr)

balance :: Tree Program -> Int
balance tr =
  fmap (liftA2 (,) selfWeight totalWeight) (unbalanced tr)
    |> sortOn snd
    |> last
    |> fst
    |> subtract (balanceWeight tr)

-- xegshds
solution1 :: Input -> String
solution1 input =
  parseOrDie inputParser input
    |> toProgram
    |> rootLabel
    |> name

-- "299"
solution2 :: Input -> String
solution2 input =
  parseOrDie inputParser input
    |> toProgram
    |> balance
    |> show

solution :: Solution
solution = PureSolution solution1 "xegshds" solution2 "299"
