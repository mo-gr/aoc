module Y2021.AOC12 where

import AOC (Solution (PureSolution))
import Data.Functor (($>), (<&>))
import Data.List (group, sort)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (lower, many1, newline, string, try, upper, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

data Cave = Start | End | Small String | Big String deriving (Show, Eq, Ord)

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall _ = False

type Connection = (Cave, Cave)

type Path = [Cave]

type Filter = Path -> Cave -> Bool

caveParser :: Parser Cave
caveParser =
  try (string "start" $> Start)
    <|> try (string "end" $> End)
    <|> (many1 upper <&> Big)
    <|> (many1 lower <&> Small)

inputParser :: Parser [Connection]
inputParser = many1 $ do
  c1 <- caveParser <* string "-"
  c2 <- caveParser <* newline
  pure (c1, c2)

candidates :: [Connection] -> Cave -> [Cave]
candidates cs cave = mconcat $ fmap cf cs
  where
    cf (c, c') | c == cave = [c']
    cf (c, c') | c' == cave = [c]
    cf _ = []

allPaths :: Filter -> Path -> [Connection] -> [Path]
allPaths _ p _ | length p > 30 = error $ "too deep: " ++ show p
allPaths _ (End : p) _ = [End : p]
allPaths fInvalid path@(c : _) cs = mconcat $ do
  candidate <- filter (fInvalid path) $ candidates cs c
  pure $ allPaths fInvalid (candidate : path) cs
allPaths _ _ _ = error "something went wrong"

invalid :: Path -> Cave -> Bool
invalid _ (Big _) = True
invalid path c = c `notElem` path

hasBeenToAnySmallTwice :: Path -> Bool
hasBeenToAnySmallTwice p =
  filter isSmall p
    |> sort
    |> group
    |> fmap length
    |> any (> 1)

invalid' :: Path -> Cave -> Bool
invalid' _ (Big _) = True
invalid' path c@(Small _) | hasBeenToAnySmallTwice path = c `notElem` path
invalid' _ (Small _) = True
invalid' path c = c `notElem` path

-- 5254
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> allPaths invalid [Start]
    |> length

-- 149385
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> allPaths invalid' [Start]
    |> length

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 5254 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 149385 . solution2 =<< input
    ]

solution :: Solution
solution = PureSolution solution1 5254 solution2 149385
