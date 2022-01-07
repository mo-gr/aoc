module Y2015.AOC13 where

import Data.Functor ((<&>))
import Data.List (nub, permutations, sort)
import qualified Data.Map.Strict as M
import Data.Tuple (swap)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (letter, many1, newline, string, try, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Name = String

type Happiness = Int

type HappyTable = M.Map (Name, Name) Happiness

inputParser :: Parser HappyTable
inputParser = do
  happies <- many1 $ do
    name <- many1 letter
    happiness <- try (string " would gain " *> number) <|> (string " would lose " *> number <&> negate)
    _ <- string " happiness units by sitting next to "
    name' <- many1 letter
    _ <- string "." *> newline
    pure ((name, name'), happiness)
  pure $ M.fromList happies

maxHappy :: HappyTable -> Int
maxHappy ht =
  let guests = M.keys ht |> fmap fst
   in maximum $ do
        seatings <- permutations . nub . sort $ guests
        let pairings = zip seatings (tail seatings <> [head seatings])
        let cost = (pairings <> fmap swap pairings) |> fmap (ht M.!)
        pure $ sum cost

insertMe :: HappyTable -> HappyTable
insertMe ht =
  let guests = M.keys ht |> fmap fst |> sort |> nub
   in M.union ht $ M.fromList $ guests >>= (\g -> [((g, "Me"), 0), (("Me", g), 0)])

-- 664
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> maxHappy

-- 640
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> insertMe
    |> maxHappy

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 664 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 640 . solution2 =<< input
    ]
