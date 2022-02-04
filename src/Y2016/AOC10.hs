{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC10 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.List (sort)
import qualified Data.Map.Strict as M
import Text.Parsec (many1, newline, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data AgentId
  = BotId {unAgentId :: Int}
  | InputId {unAgentId :: Int}
  | OutputId {unAgentId :: Int}
  deriving (Eq, Ord)

data Agent' a
  = Bot AgentId AgentId AgentId [a]
  | Input AgentId a
  | Output AgentId a

instance Show a => Show (Agent' a) where
  show (Input _ v) = "Source of " <> show v
  show (Bot _ l h v) = "Bot (" <> show (unAgentId l) <> "/" <> show (unAgentId h) <> ") " <> show v
  show (Output _ a) = show a <> " output"

instance Show AgentId where
  show = show . unAgentId

type Agent = Agent' Int

type World = M.Map AgentId Agent

inputParser :: Parser [Agent]
inputParser = many1 $ (botP <|> inputP) <* newline
  where
    botP, inputP :: Parser Agent
    botP = do
      botId <- fmap BotId $ string "bot " *> number
      low <- string " gives low to " *> (string "bot " *> fmap BotId number <|> string "output " *> fmap OutputId number)
      high <- string " and high to " *> (string "bot " *> fmap BotId number <|> string "output " *> fmap OutputId number)
      pure $ Bot botId low high []
    inputP = do
      inputValue <- string "value " *> number
      targetBot <- fmap BotId $ string " goes to bot " *> number
      pure $ Input targetBot inputValue

toMapEntry :: Agent -> (AgentId, Agent)
toMapEntry a@(Bot i _ _ _) = (i, a)
toMapEntry a@(Input _ v) = (InputId v, a)
toMapEntry a@(Output i _) = (i, a)

canHold :: Agent -> Int -> Bool
canHold (Bot _ _ _ []) _ = True
canHold (Bot _ _ _ [a]) a' | a /= a' = True
canHold (Output _ _) _ = True
canHold _ _ = False

giveTo :: Agent -> Int -> Agent
giveTo (Bot i l h v) a = Bot i l h $ sort (a : v)
giveTo agent _a = agent

findBot :: Int -> Int -> Int -> World -> AgentId
findBot 1000 _ _ w = error $ "no solution " <> show w
findBot n v w world = case filter vwBot $ M.toList world of
  [(botId, _)] -> botId
  _ -> findBot (succ n) v w $ step world
  where
    vwBot (_, Bot _ _ _ [a, b]) | a == v && b == w = True
    vwBot _ = False

findOutputProduct :: Int -> World -> Int
findOutputProduct 100 w = error $ "no solution " <> show w
findOutputProduct n world = case (M.lookup (OutputId 0) world, M.lookup (OutputId 1) world, M.lookup (OutputId 2) world) of
  (Just (Output _ a), Just (Output _ b), Just (Output _ c)) -> a * b * c
  _ -> findOutputProduct (succ n) $ step world

step :: World -> World
step world = M.foldl f world world
  where
    f acc (Input b v) = case M.lookup b acc of
      Just bot | canHold bot v -> M.insert b (giveTo bot v) acc
      _ -> acc
    f acc (Bot b l h [v, w]) = case (M.lookup l acc, M.lookup h acc) of
      (Just lowBot, Just highBot)
        | canHold lowBot v && canHold highBot w ->
          M.insert l (giveTo lowBot v) $
            M.insert h (giveTo highBot w) $
              M.insert b (Bot b l h []) acc
      (Nothing, _) -> M.insert l (Output l v) acc
      (_, Nothing) -> M.insert h (Output h v) acc
      _ -> acc
    f acc _ = acc

-- 147
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> M.fromList . fmap toMapEntry
    |> findBot 0 17 61
    |> unAgentId

-- 55637
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> M.fromList . fmap toMapEntry
    |> findOutputProduct 0

solution :: Solution
solution = PureSolution solution1 147 solution2 55637
