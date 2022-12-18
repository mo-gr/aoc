{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2022.AOC16 where

import AOC (Solution (PureSolution))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>), number)
import Text.Parsec (many, string, newline, sepBy, upper, try)
import qualified Data.Map.Strict as M
import Control.Lens (makeLenses, view, set)
import Control.Applicative ((<|>))
import Debug.Trace

data Valve = Valve {
  _valveName :: String,
  _flowRate :: Int,
  _tunnels :: [String],
  _isOpen :: Bool
} deriving (Show)

makeLenses ''Valve

data Step = Open | Move
  deriving Show

zipByName :: [Valve] -> [(String, Valve)]
zipByName vs = zip (fmap (view valveName) vs) vs

inputParser :: Parser (M.Map String Valve)
inputParser = fmap (M.fromList . zipByName) $ many $ do
  vn <- string "Valve " *> many upper
  fr <- string " has flow rate=" *> number
  ts <- (try (string "; tunnels lead to valves ") <|> string "; tunnel leads to valve ") *> (many upper `sepBy` string ", ") <* newline
  pure $ Valve vn fr ts False

solve1 :: [String] -> Int -> M.Map String Valve -> [Int]
solve1 _ mins _ | mins <= 2 = [0]
solve1 _ _ valves | M.elems valves |> all (\v -> view isOpen v || view flowRate v == 0) = [0]
--solve1 p _ _ | length p >=8 && take 8 p |> mconcat |> notElem '^' = []
solve1 p mins valves = let currentName = take 2 $ head p in do
  let current = valves M.! currentName
  step <- if view isOpen current || view flowRate current == 0 then [Move] else [Open, Move]
  case step of
    Open -> ((view flowRate current * pred mins) +) <$> solve1 ((currentName <> "^") :p) (pred mins) (M.adjust (set isOpen True) currentName valves)
    Move -> do
             next <- view tunnels current
             if next `notElem` take 2 p
              then solve1 (next:p) (pred mins) valves
              else [0]


solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> solve1 ["AA"] 30
    |> maximum

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> undefined

solution :: Solution
solution = PureSolution solution1 undefined solution2 undefined

testData :: Input
testData = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n\
            \Valve BB has flow rate=13; tunnels lead to valves CC, AA\n\
            \Valve CC has flow rate=2; tunnels lead to valves DD, BB\n\
            \Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n\
            \Valve EE has flow rate=3; tunnels lead to valves FF, DD\n\
            \Valve FF has flow rate=0; tunnels lead to valves EE, GG\n\
            \Valve GG has flow rate=0; tunnels lead to valves FF, HH\n\
            \Valve HH has flow rate=22; tunnel leads to valve GG\n\
            \Valve II has flow rate=0; tunnels lead to valves AA, JJ\n\
            \Valve JJ has flow rate=21; tunnel leads to valve II\n"