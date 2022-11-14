{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC7 where

import AOC (Solution (PureSolution))
import Control.Applicative (liftA2, (<|>))
import Data.List (sortOn)
import Data.Tree
import Text.Parsec (letter, many1, newline, sepBy, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Program = Program
  { name :: String,
    weight :: Int
  }
  deriving (Show, Eq)

type ProgramInput = (String, Int, [String])

inputParser :: Parser [ProgramInput]
inputParser = many1 $ do
  nme <- many1 letter
  wght <- string " (" *> number <* string ")"
  blnc <- try (string " -> " *> (many1 letter `sepBy` string ", ")) <|> pure []
  _ <- newline
  pure (nme, wght, blnc)

toProgram :: [ProgramInput] -> Tree Program
toProgram pis = unfoldTree f (findRoot pis)
  where
    f :: ProgramInput -> (Program, [ProgramInput])
    f (n, w, bs) = (Program n w, filter (byName bs) pis)
    byName names (nme, _, _) = nme `elem` names
    discs :: [String]
    discs = concatMap (\(_, _, bs) -> bs) pis
    findRoot :: [ProgramInput] -> ProgramInput
    findRoot = head . filter (\(n, _, _) -> n `notElem` discs)

treeWeight :: Tree Program -> Int
treeWeight t = rootLabel t |> weight

totalWeight :: Tree Program -> Int
totalWeight tr = (weight . rootLabel $ tr) + (sum . discWeights $ tr)

discWeights :: Tree Program -> [Int]
discWeights tr = case subForest tr of
  [] -> []
  disc -> fmap (\t -> treeWeight t + sum (discWeights t)) disc

balanceWeight :: Tree Program -> Int
balanceWeight t = discWeights t |> liftA2 (-) maximum minimum

isBalanced :: Tree Program -> Bool
isBalanced tr | null (subForest tr) = True
isBalanced tr = balanceWeight tr == 0

unbalanced :: Tree Program -> [Tree Program]
unbalanced tr | balanceWeight tr == 0 = []
unbalanced tr =
  if subForest tr |> fmap isBalanced |> and
    then subForest tr
    else subForest tr |> concatMap unbalanced

balance :: Tree Program -> Int
balance tr =
  let bw = balanceWeight tr
      unb = unbalanced tr
   in case unb of
        [] -> error "tree balanced"
        unbtr ->
          fmap (\t -> (weight . rootLabel $ t, totalWeight t)) unbtr
            |> sortOn snd
            |> last
            |> fst
            |> subtract bw

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

testData :: Input
testData = "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)\n"
