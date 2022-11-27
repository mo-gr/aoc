{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC25 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Set as S
import Text.Parsec (char, letter, newline, sepBy1, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Write = Write1 | Write0
  deriving (Show, Eq, Ord)

data Move = MoveL | MoveR
  deriving (Show, Eq, Ord)

type Op = (Write, Move, StateId)

type StateId = Char

data State = State
  { stateId :: StateId,
    op0 :: Op,
    op1 :: Op
  }
  deriving (Show, Eq, Ord)

preambleP :: Parser (StateId, Int)
preambleP = do
  startState <- string "Begin in state " *> letter <* char '.' <* newline
  steps <- string "Perform a diagnostic checksum after " *> number <* string " steps." <* newline
  pure (startState, steps)

stateP :: Parser State
stateP = do
  sid <- string "In state " *> letter <* char ':' <* newline
  _ <- string "  If the current value is 0:" <* newline
  w0 <- (try (string "    - Write the value 1." $> Write1) <|> (string "    - Write the value 0." $> Write0)) <* newline
  m0 <- (try (string "    - Move one slot to the right." $> MoveR) <|> (string "    - Move one slot to the left." $> MoveL)) <* newline
  n0 <- string "    - Continue with state " *> letter <* char '.' <* newline
  _ <- string "  If the current value is 1:" <* newline
  w1 <- (try (string "    - Write the value 1." $> Write1) <|> (string "    - Write the value 0." $> Write0)) <* newline
  m1 <- (try (string "    - Move one slot to the right." $> MoveR) <|> (string "    - Move one slot to the left." $> MoveL)) <* newline
  n1 <- string "    - Continue with state " *> letter <* char '.' <* newline
  pure $ State sid (w0, m0, n0) (w1, m1, n1)

inputParser :: Parser (StateId, Int, [State])
inputParser = do
  (start, count) <- preambleP <* newline
  ss <- stateP `sepBy1` newline
  pure (start, count, ss)

data Touring = Touring
  { states :: [State],
    ones :: S.Set Int,
    state :: StateId,
    remaining :: Int,
    cursor :: Int
  }
  deriving (Show)

mkTouring :: (StateId, Int, [State]) -> Touring
mkTouring (st, steps, allStates) = Touring allStates S.empty st steps 0

lookupState :: [State] -> Char -> State
lookupState ss s = head $ filter ((== s) . stateId) ss

write :: Write -> Touring -> Touring
write Write0 t@Touring {ones, cursor} = t {ones = S.delete cursor ones}
write Write1 t@Touring {ones, cursor} = t {ones = S.insert cursor ones}

move :: Move -> Touring -> Touring
move MoveL t = t {cursor = pred (cursor t)}
move MoveR t = t {cursor = succ (cursor t)}

lookupOp :: Touring -> Op
lookupOp Touring {ones, cursor, states, state}
  | S.member cursor ones = lookupState states state |> op1
  | otherwise = lookupState states state |> op0

eval :: Touring -> Touring
eval t | remaining t == 0 = t
eval t =
  let (wr, mo, s') = lookupOp t
   in t {state = s', remaining = pred (remaining t)} |> write wr |> move mo |> eval

-- 2526
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> mkTouring
    |> eval
    |> ones
    |> length

solution2 :: Input -> Int
solution2 _input = 0

solution :: Solution
solution = PureSolution solution1 2526 solution2 0
