module Y2021.AOC25 where

import AOC (Solution (PureSolution))
import Data.Bifunctor (first, second)
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (char, many1, newline, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

data Field = Empty | EastCumber | SouthCumber deriving (Eq)

instance Show Field where
  show Empty = "."
  show EastCumber = ">"
  show SouthCumber = "v"

type Point = (Int, Int)

type Floor = M.Map Point Field

fieldParser :: Parser Field
fieldParser =
  (char '.' $> Empty)
    <|> (char '>' $> EastCumber)
    <|> (char 'v' $> SouthCumber)

inputParser :: Parser Floor
inputParser = do
  listOfFields <- many1 (many1 fieldParser <* newline)
  pure $ M.fromList (kvPairs listOfFields)
  where
    kvPairs :: [[Field]] -> [(Point, Field)]
    kvPairs os = do
      x <- [0 .. length (head os) - 1]
      y <- [0 .. length os - 1]
      pure ((x, y), (os !! y) !! x)

east :: Floor -> Point -> Point
east f p = if first (1 +) p `M.member` f then first (1 +) p else first (const 0) p

south :: Floor -> Point -> Point
south f p = if second (1 +) p `M.member` f then second (1 +) p else second (const 0) p

canMove :: Floor -> Point -> Bool
canMove f p = case f M.! p of
  Empty -> False
  SouthCumber -> f M.! south f p == Empty
  EastCumber -> f M.! east f p == Empty

step :: Floor -> Floor
step f = M.foldlWithKey southMove afterEast afterEast
  where
    eastMove :: Floor -> Point -> Field -> Floor
    eastMove acc p EastCumber | canMove f p = M.insert (east f p) EastCumber $ M.insert p Empty acc
    eastMove acc _ _ = acc
    afterEast = M.foldlWithKey eastMove f f
    southMove :: Floor -> Point -> Field -> Floor
    southMove acc p SouthCumber | canMove afterEast p = M.insert (south afterEast p) SouthCumber $ M.insert p Empty acc
    southMove acc _ _ = acc

runUntilStable :: Floor -> Int
runUntilStable = recur 1
  where
    recur :: Int -> Floor -> Int
    recur n f = let f' = step f in if f' == f then n else recur (n + 1) f'

-- 523
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> runUntilStable

-- does not exist
solution2 :: Input -> Int
solution2 _input = 0

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 523 . solution1 =<< input
    --TestCase $ assertEqual "solution 2" undefined . solution2 =<< input
    ]

solution :: Solution
solution = PureSolution solution1 523 solution2 0
