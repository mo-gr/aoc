{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC12 where

import AOC (Solution (PureSolution))
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Text.Parsec (many1, newline, sepBy1, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Pid = Int

type Connections = (Pid, [Pid])

type Pipes = M.Map Pid [Pid]

inputParser :: Parser [Connections]
inputParser = many1 $ do
  self <- number
  _ <- string " <-> "
  others <- number `sepBy1` string ", "
  _ <- newline
  pure (self, others)

visit :: S.Set Pid -> Pipes -> S.Set Pid
visit seen pipes =
  let connected = S.toList seen |> mapMaybe (`M.lookup` pipes) |> mconcat
      seen' = S.union seen (S.fromList connected)
   in if seen == seen' then seen else visit seen' pipes

countGroups :: Pipes -> Int
countGroups pipes = groupCount
  where
    maxPid :: Int
    maxPid = M.findMax pipes |> fst
    allGroups :: [S.Set Pid]
    allGroups = fmap (\p -> visit (S.singleton p) pipes) [0 .. maxPid]
    groupCount = S.fromList allGroups |> length

-- 145
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> M.fromList
    |> visit (S.singleton 0)
    |> length

-- 207
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> M.fromList
    |> countGroups

solution :: Solution
solution = PureSolution solution1 145 solution2 207
