{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2022.AOC11 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Control.Lens (each, lengthOf, makeLenses, over, set, traversed, view, _2)
import Data.Functor (($>))
import Data.List (sort)
import qualified Data.Map.Strict as M
import Debug.Trace (traceShowId)
import Text.Parsec (char, newline, sepBy, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, times, (|>))

inputParser :: Parser (M.Map MonkeyId (Monkey Item))
inputParser = fmap M.fromList $
  flip sepBy newline $ do
    mid <- string "Monkey " *> number <* char ':' <* newline
    is <- string "  Starting items: " *> (number `sepBy` string ", ") <* newline
    op <- string "  Operation: new = old " *> opP <* newline
    d <- string "  Test: divisible by " *> number <* newline
    true <- string "    If true: throw to monkey " *> number <* newline
    false <- string "    If false: throw to monkey " *> number <* newline
    pure (mid, Monkey is d op (\a -> if a `rem` d == 0 then true else false) 0)
  where
    opP =
      (string "+ " *> number >>= \n -> pure (+ n))
        <|> try (string "* " *> number >>= \n -> pure (* n))
        <|> (string "* old" $> \n -> n * n)

inputParserComplex :: Parser (M.Map MonkeyId (Monkey ComplexItem))
inputParserComplex = fmap M.fromList $
  flip sepBy newline $ do
    mid <- string "Monkey " *> number <* char ':' <* newline
    is <- string "  Starting items: " *> (number `sepBy` string ", ") <* newline
    op <- string "  Operation: new = old " *> opP <* newline
    d <- string "  Test: divisible by " *> number <* newline
    true <- string "    If true: throw to monkey " *> number <* newline
    false <- string "    If false: throw to monkey " *> number <* newline
    pure (mid, Monkey (fmap (M.singleton 0) is) d op (\a -> if (a M.! d) `rem` d == 0 then true else false) 0)
  where
    opP =
      (string "+ " *> number >>= \n -> pure (M.mapWithKey (\k v -> n + v `mod` k)))
        <|> try (string "* " *> number >>= \n -> pure (M.mapWithKey (\k v -> n * v `mod` k)))
        <|> (string "* old" $> M.mapWithKey (\k v -> v * v `mod` k))

type Item = Int

type ComplexItem = M.Map Int Int

type MonkeyId = Int

data Monkey a = Monkey
  { _items :: [a],
    _divisor :: Int,
    _op :: a -> a,
    _throw :: a -> MonkeyId,
    _inspectCount :: Int
  }

makeLenses ''Monkey

turn1 :: Monkey Item -> (Monkey Item, [(MonkeyId, Item)])
turn1 m = (m', view items m |> fmap process)
  where
    process :: Item -> (MonkeyId, Item)
    process i =
      view op m i
        |> (`quot` 3)
        |> \i' -> (view throw m i', i')
    m' =
      m |> set items []
        |> over inspectCount (+ lengthOf (items . each) m)

monkeyRound :: (Monkey a -> (Monkey a, [(MonkeyId, a)])) -> M.Map MonkeyId (Monkey a) -> M.Map MonkeyId (Monkey a)
monkeyRound turn ms = foldl processTurn ms (M.toAscList ms)
  where
    processTurn ms' (mid, _) = turn ((M.!) ms' mid) |> \(m', ts) -> foldl ins (M.insert mid m' ms') ts
    ins :: M.Map MonkeyId (Monkey a) -> (MonkeyId, a) -> M.Map MonkeyId (Monkey a)
    ins mp (k, i) = M.adjust (over items (\is -> is <> [i])) k mp

prepopulateDivisorMap :: M.Map MonkeyId (Monkey ComplexItem) -> M.Map MonkeyId (Monkey ComplexItem)
prepopulateDivisorMap mmap = M.map (over (items . traversed) (\i -> populatedItem (i M.! 0))) mmap
  where
    ms = M.elems mmap
    divisors = filter (/= 0) $ view divisor <$> ms
    populatedItem :: Int -> ComplexItem
    populatedItem i = M.fromList $ zip divisors (repeat i)

-- 54036
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> times 20 (monkeyRound turn1)
    |> monkeyBusiness

monkeyBusiness :: M.Map MonkeyId (Monkey a) -> Int
monkeyBusiness m = M.toList m |> fmap (view (_2 . inspectCount)) |> traceShowId |> sort |> reverse |> take 2 |> product

turn2 :: Monkey ComplexItem -> (Monkey ComplexItem, [(MonkeyId, ComplexItem)])
turn2 m = (m', view items m |> fmap process)
  where
    process :: ComplexItem -> (MonkeyId, ComplexItem)
    process i =
      view op m i
        |> \i' -> (view throw m i', i')
    m' =
      m |> set items []
        |> over inspectCount (+ lengthOf (items . each) m)

-- 13237873355
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParserComplex input
    |> prepopulateDivisorMap
    |> times 10000 (monkeyRound turn2)
    |> monkeyBusiness

solution :: Solution
solution = PureSolution solution1 54036 solution2 13237873355

testData :: Input
testData =
  "Monkey 0:\n\
  \  Starting items: 79, 98\n\
  \  Operation: new = old * 19\n\
  \  Test: divisible by 23\n\
  \    If true: throw to monkey 2\n\
  \    If false: throw to monkey 3\n\
  \\n\
  \Monkey 1:\n\
  \  Starting items: 54, 65, 75, 74\n\
  \  Operation: new = old + 6\n\
  \  Test: divisible by 19\n\
  \    If true: throw to monkey 2\n\
  \    If false: throw to monkey 0\n\
  \\n\
  \Monkey 2:\n\
  \  Starting items: 79, 60, 97\n\
  \  Operation: new = old * old\n\
  \  Test: divisible by 13\n\
  \    If true: throw to monkey 1\n\
  \    If false: throw to monkey 3\n\
  \\n\
  \Monkey 3:\n\
  \  Starting items: 74\n\
  \  Operation: new = old + 3\n\
  \  Test: divisible by 17\n\
  \    If true: throw to monkey 0\n\
  \    If false: throw to monkey 1\n"
