module Y2015.AOC11 where

import Control.Monad (guard)
import Data.List (group)
import Debug.Trace
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (lower, many1)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

inputParser :: Parser String
inputParser = many1 lower

increment :: String -> String
increment s = reverse $ f $ reverse s
  where
    f (a : rst) = case succ a of
      '{' -> 'a' : f rst
      a' -> a' : rst
    f [] = []

twoPairs, hasStraight3, noConfusing :: String -> Bool
twoPairs s =
  group s
    |> fmap length
    |> filter (>= 2)
    |> length
    |> (>= 2)
hasStraight3 [] = False
hasStraight3 (a : b : c : _) | b == succ a && c == succ b = True
hasStraight3 (_ : rst) = hasStraight3 rst
noConfusing [] = True
noConfusing ('i' : _) = False
noConfusing ('o' : _) = False
noConfusing ('l' : _) = False
noConfusing (_ : rst) = noConfusing rst

nextPassword :: String -> String
nextPassword p = head $ do
  p' <- tail $ iterate increment p
  guard $ noConfusing p'
  guard $ hasStraight3 p'
  guard $ twoPairs p'
  pure p'

-- hxbxxyzz
-- 8134462547934010698
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> nextPassword
    |> traceId
    |> fmap fromEnum
    |> toOneInt

-- hxcaabcc
-- 104120999797989999
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> nextPassword
    |> nextPassword
    |> traceId
    |> fmap fromEnum
    |> toOneInt

toOneInt :: [Int] -> Int
toOneInt is = fmap show is |> mconcat |> read

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 8134462547934010698 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 104120999797989999 . solution2 =<< input
    ]
