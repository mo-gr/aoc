module Y2021.AOC14 where

import Data.Bifunctor (first)
import Data.List (group, sort)
import qualified Data.Map.Strict as M
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (count, many1, newline, string, upper)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, times, (|>))

type Polymer = String

type Reaction = (String, Char)

type Reactions = M.Map String Char

type PolymerTracker = M.Map String Int

polymerParser :: Parser Polymer
polymerParser = many1 upper <* newline

reactionParser :: Parser Reaction
reactionParser = do
  inp <- count 2 upper
  _ <- string " -> "
  out <- upper <* newline
  pure (inp, out)

inputParser :: Parser (Polymer, [Reaction])
inputParser = do
  p <- polymerParser <* newline
  rs <- many1 reactionParser
  pure (p, rs)

dissect :: PolymerTracker -> Polymer -> PolymerTracker
dissect pt [] = pt
dissect pt [p] = M.insertWith (+) [p] 1 pt
dissect pt (p : p' : ps) = M.insertWith (+) [p, p'] 1 $ dissect pt (p' : ps)

react' :: PolymerTracker -> Reactions -> PolymerTracker
react' pt rs = M.foldrWithKey' f pt pt
  where
    f :: Polymer -> Int -> PolymerTracker -> PolymerTracker
    f (p : p' : _) pCount acc | pCount > 0 = case M.lookup [p, p'] rs of
      Just i ->
        M.adjust (\c -> c - pCount) [p, p'] $
          M.insertWith (+) [i, p'] pCount $
            M.insertWith (+) [p, i] pCount acc
      Nothing -> acc
    f _ _ acc = acc

react :: Polymer -> Reactions -> Polymer
react [] _rs = []
react [p] _rs = [p]
react (p : p' : ps) rs = case M.lookup [p, p'] rs of
  Just i -> p : i : react (p' : ps) rs
  Nothing -> react (p' : ps) rs

reactTimes :: Int -> Polymer -> Reactions -> Polymer
reactTimes n p rs = times n (`react` rs) p

reactTimes' :: Int -> Polymer -> Reactions -> PolymerTracker
reactTimes' n p rs = times n (`react'` rs) (dissect M.empty p)

score :: Polymer -> Int
score p = sort p |> group |> fmap length |> \l -> maximum l - minimum l

score' :: PolymerTracker -> Int
score' p =
  M.toList p
    |> fmap (first head)
    |> M.fromListWith (+)
    |> M.toList
    |> fmap snd
    |> sort
    |> \l -> maximum l - minimum l

-- 2712
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> fmap M.fromList
    |> uncurry (reactTimes 10)
    |> score

-- 8336623059567
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> fmap M.fromList
    |> uncurry (reactTimes' 40)
    |> score'

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 2712 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 8336623059567 . solution2 =<< input
    ]
