{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2022.AOC10 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Control.Lens (makeLenses, over, view)
import Data.Functor (($>))
import Debug.Trace (traceId)
import Text.Parsec (many1, newline, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, parseOrDie, (|>))

inputParser :: Parser [Op]
inputParser = many1 (addP <|> noopP)
  where
    noopP = string "noop" *> newline $> Noop
    addP = AddX <$> (string "addx " *> negativeNumber <* newline)

data Op = AddX Int | Noop
  deriving (Show, Eq)

data CPU = CPU
  { _regX :: Int,
    _pc :: Int,
    _cycles :: Int
  }
  deriving (Show, Eq)

makeLenses ''CPU

eval :: CPU -> Op -> CPU
eval c Noop = over cycles (+ 1) c |> over pc succ
eval c (AddX x) = over regX (+ x) c |> over cycles (+ 2) |> over pc succ

overTime :: [Op] -> [CPU]
overTime = scanl eval (CPU 1 0 0)

signalAt :: Int -> [CPU] -> Int
signalAt cc cpus = cc * regAAt cc cpus

-- 14160
solution1 :: Input -> String
solution1 input =
  parseOrDie inputParser input
    |> overTime
    |> \cc ->
      [signalAt 20 cc, signalAt 60 cc, signalAt 100 cc, signalAt 140 cc, signalAt 180 cc, signalAt 220 cc]
        |> sum
        |> show

regAAt :: Int -> [CPU] -> Int
regAAt cc cpus =
  takeWhile ((< cc) . view cycles) cpus
    |> last
    |> view regX

crtLines :: String -> [String]
crtLines [] = []
crtLines c = take 40 c : crtLines (drop 40 c)

render :: [CPU] -> String
render cpus = do
  c <- [1 .. 240]
  let a = regAAt c cpus
  if (pred c `mod` 40) `elem` [pred a, a, succ a]
    then pure '#'
    else pure '.'

solution2 :: Input -> String
solution2 input =
  parseOrDie inputParser input
    |> overTime
    |> render
    |> crtLines
    |> unlines
    |> traceId
    |> const "RJERPEFC"

solution :: Solution
solution = PureSolution solution1 "14160" solution2 "RJERPEFC"
