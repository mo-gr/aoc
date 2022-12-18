{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2022.AOC16 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Control.Lens (makeLenses, set, view)
import Control.Monad (guard)
import Data.List (subsequences)
import qualified Data.Map.Strict as M
import Text.Parsec (many, newline, sepBy, string, try, upper)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Valve = Valve
  { _valveName :: String,
    _flowRate :: Int,
    _tunnels :: [String],
    _compactTunnels :: [(String, Int)],
    _isOpen :: Bool
  }
  deriving (Show)

makeLenses ''Valve

data Step = Open | Move
  deriving (Show)

zipByName :: [Valve] -> [(String, Valve)]
zipByName vs = zip (fmap (view valveName) vs) vs

inputParser :: Parser (M.Map String Valve)
inputParser = fmap (M.fromList . zipByName) $
  many $ do
    vn <- string "Valve " *> many upper
    fr <- string " has flow rate=" *> number
    ts <- (try (string "; tunnels lead to valves ") <|> string "; tunnel leads to valve ") *> (many upper `sepBy` string ", ") <* newline
    pure $ Valve vn fr ts [] False

compactPaths :: M.Map String Valve -> M.Map String Valve
compactPaths valves = M.map (\v -> set compactTunnels (reachable [] (view tunnels v)) v) valves
  where
    reachable :: [(String, Int)] -> [String] -> [(String, Int)]
    reachable r [] = r
    reachable r (v : vrest) | v `elem` (fst <$> r) = reachable r vrest
    reachable r (v : vrest) | (M.!) valves v |> view flowRate |> (> 0) = reachable ((v, 1) : r) vrest
    reachable r (v : vrest) = (fmap . fmap) succ $ reachable r ((M.!) valves v |> view tunnels) <> reachable r vrest

bfs :: M.Map String Valve -> String -> [String] -> Int
bfs valves target steps
  | target `elem` steps = 0
  | otherwise = succ $
    bfs valves target $ do
      s <- steps
      view tunnels (valves M.! s)

shortestPaths :: M.Map String Valve -> M.Map (String, String) Int
shortestPaths valves = M.fromList $ do
  v <- "AA" : M.elems valves |> filter ((> 0) . view flowRate) |> filter (not . view isOpen) |> fmap (view valveName)
  v' <- M.elems valves |> filter ((> 0) . view flowRate) |> filter (not . view isOpen) |> fmap (view valveName)
  guard $ v /= v'
  pure ((v, v'), bfs valves v' [v])

solve1 :: [String] -> Int -> M.Map (String, String) Int -> M.Map String Valve -> [Int]
solve1 _ mins _ _ | mins < 0 = []
solve1 _ _ _ valves | M.elems valves |> all (\v -> view isOpen v || view flowRate v == 0) = [0]
solve1 p mins paths valves =
  let currentName = take 2 $ head p
   in do
        let current = valves M.! currentName
        step <- if view isOpen current || view flowRate current == 0 then [Move] else [Open]
        case step of
          Open -> ((view flowRate current * pred mins) +) <$> solve1 ((currentName <> "^") : p) (pred mins) paths (M.adjust (set isOpen True) currentName valves)
          Move -> do
            next <- M.elems valves |> filter ((> 0) . view flowRate) |> filter (not . view isOpen) |> fmap (view valveName)
            guard $ next /= currentName
            let cost = paths M.! (currentName, next)
            if cost < mins
              then solve1 (next : p) (mins - cost) paths valves
              else [0]

solve2 :: M.Map String Valve -> Int
solve2 valves = maximum $ do
  let paths = shortestPaths valves
  let targets = M.elems valves |> filter ((> 0) . view flowRate) |> fmap (view valveName)
  (sp1, sp2) <- splits targets
  let flow1 = maximum $ solve1 ["AA"] 26 paths (M.fromList (fmap (\v -> (v, valves M.! v)) ("AA" : sp1)))
  let flow2 = maximum $ solve1 ["AA"] 26 paths (M.fromList (fmap (\v -> (v, valves M.! v)) ("AA" : sp2)))
  pure (flow1 + flow2)

splits :: Eq a => [a] -> [([a], [a])]
splits as = do
  s <- subsequences as
  pure (s, filter (`notElem` s) as)

-- 1767
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> \v ->
      solve1 ["AA"] 30 (shortestPaths v) v
        |> maximum

-- 2528
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve2

solution :: Solution
solution = PureSolution solution1 1767 solution2 2528

testData :: Input
testData =
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n\
  \Valve BB has flow rate=13; tunnels lead to valves CC, AA\n\
  \Valve CC has flow rate=2; tunnels lead to valves DD, BB\n\
  \Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n\
  \Valve EE has flow rate=3; tunnels lead to valves FF, DD\n\
  \Valve FF has flow rate=0; tunnels lead to valves EE, GG\n\
  \Valve GG has flow rate=0; tunnels lead to valves FF, HH\n\
  \Valve HH has flow rate=22; tunnel leads to valve GG\n\
  \Valve II has flow rate=0; tunnels lead to valves AA, JJ\n\
  \Valve JJ has flow rate=21; tunnel leads to valve II\n"
