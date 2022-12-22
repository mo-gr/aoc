{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2022.AOC19 where

import AOC (Solution (PureSolution))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>), number)
import Text.Parsec (many, string, newline)
import Control.Lens (makeLenses)

data Blueprint = Blueprint {
  blueprintId :: Int
  , oreCost :: Int
  , clayCost :: Int
  , obsidianCost :: (Int, Int)
  , geodeCost :: (Int, Int)
}

data State = State {
  oreRobots :: Int
  , clayRobots :: Int
  , obsidianRobots :: Int
  , geodeRobots :: Int
  , ore :: Int
  , clay :: Int
  , geodes :: Int
}

makeLenses ''Blueprint
makeLenses ''State

inputParser :: Parser [Blueprint]
inputParser = many $ do
  bpid <- string "Blueprint " *> number <* string ": "
  oc <- string "Each ore robot costs " *> number <* string " ore. "
  cc <- string "Each clay robot costs " *> number <* string " ore. "
  obo <- string "Each obsidian robot costs " *> number <* string " ore "
  obc <- string "and " *> number <* string " clay. "
  goc <- string "Each geode robot costs " *> number <* string " ore "
  gobc <- string "and " *> number <* string " obsidian." <* newline
  pure $ Blueprint bpid oc cc (obo, obc) (goc, gobc)

solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> length

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> undefined

solution :: Solution
solution = PureSolution solution1 undefined solution2 undefined

testData :: Input
testData = "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\n\
           \Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.\n"