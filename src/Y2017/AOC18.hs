{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC18 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Safe (atMay)
import Text.Parsec (char, many1, newline, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, parseOrDie, (|>))

data CPU a = CPU
  { regA :: a,
    regB :: a,
    regF :: a,
    regP :: a,
    regI :: a,
    pc :: Int,
    freqSend :: [a],
    freqReceive :: [a]
  }
  deriving (Show, Eq)

data Operand a
  = Value a
  | RegA
  | RegB
  | RegF
  | RegP
  | RegI
  deriving (Show, Eq)

data Op a
  = Sound (Operand a)
  | Set (Operand a) (Operand a)
  | Add (Operand a) (Operand a)
  | Mul (Operand a) (Operand a)
  | Mod (Operand a) (Operand a)
  | Recover (Operand a)
  | Jgz (Operand a) (Operand a)
  deriving (Show, Eq)

mkCpu :: CPU Int
mkCpu = CPU 0 0 0 0 0 0 [] []

inputParser :: Parser [Op Int]
inputParser =
  many1 $
    do
      try (Sound <$> (string "snd " *> operandP <* newline))
      <|> (Recover <$> (string "rcv " *> operandP <* newline))
      <|> (Set <$> (string "set " *> operandP) <*> (char ' ' *> operandP <* newline))
      <|> (Add <$> (string "add " *> operandP) <*> (char ' ' *> operandP <* newline))
      <|> try (Mul <$> (string "mul " *> operandP) <*> (char ' ' *> operandP <* newline))
      <|> (Mod <$> (string "mod " *> operandP) <*> (char ' ' *> operandP <* newline))
      <|> (Jgz <$> (string "jgz " *> operandP) <*> (char ' ' *> operandP <* newline))

operandP :: Parser (Operand Int)
operandP =
  (Value <$> negativeNumber)
    <|> (char 'a' $> RegA)
    <|> (char 'b' $> RegB)
    <|> (char 'f' $> RegF)
    <|> (char 'p' $> RegP)
    <|> (char 'i' $> RegI)

get :: CPU a -> Operand a -> a
get _ (Value v) = v
get c RegA = regA c
get c RegB = regB c
get c RegF = regF c
get c RegP = regP c
get c RegI = regI c

put :: CPU a -> a -> Operand a -> CPU a
put _ _ (Value _) = error "writing to constant"
put c v RegA = c {regA = v}
put c v RegB = c {regB = v}
put c v RegF = c {regF = v}
put c v RegP = c {regP = v}
put c v RegI = c {regI = v}

nextPc :: CPU a -> CPU a
nextPc c = c {pc = succ (pc c)}

eval :: CPU Int -> Op Int -> CPU Int
eval c (Sound op) = c {freqSend = get c op : freqSend c} |> nextPc
eval c (Recover op) = case freqReceive c of
  [] -> c
  (f : rest) -> (put c f op) {freqReceive = rest} |> nextPc
eval c (Set to from) = put c (get c from) to |> nextPc
eval c (Add to from) = put c (get c from + get c to) to |> nextPc
eval c (Mul to from) = put c (get c from * get c to) to |> nextPc
eval c (Mod to from) = put c (get c to `mod` get c from) to |> nextPc
eval c (Jgz x y) =
  if get c x > 0
    then c {pc = pc c + get c y}
    else c |> nextPc

isRecover :: Op a -> Bool
isRecover (Recover _) = True
isRecover _ = False

nextOp :: CPU a -> [Op a] -> Maybe (Op a)
nextOp c p = p `atMay` pc c

runTilReq :: CPU Int -> [Op Int] -> Int
runTilReq c program | isRecover (fromJust $ nextOp c program) = head $ freqSend c
runTilReq c program = runTilReq (eval c (fromJust $ nextOp c program)) program

-- 3423
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> runTilReq mkCpu

data Duet a = Duet
  { cpu0 :: CPU a,
    cpu1 :: CPU a,
    sendCount :: Int
  }
  deriving (Show, Eq)

mkDuet :: Duet Int
mkDuet =
  Duet
    { cpu0 = mkCpu {regP = 0},
      cpu1 = mkCpu {regP = 1},
      sendCount = 0
    }

runDuet :: Duet Int -> [Op Int] -> Int
runDuet d program =
  let cp0 = (cpu0 d) {freqSend = [], freqReceive = freqReceive (cpu0 d) <> freqSend (cpu1 d)}
      cp1 = (cpu1 d) {freqSend = [], freqReceive = freqReceive (cpu1 d) <> freqSend (cpu0 d)}
      op0 = nextOp cp0 program
      op1 = nextOp cp1 program
   in case (op0, op1) of
        (Just (Recover _), Just (Recover _))
          | null (freqReceive cp0)
              && null (freqReceive cp1) ->
            sendCount d
        (Nothing, Nothing) -> sendCount d
        (Just opp0, Nothing) -> runDuet d {cpu0 = eval cp0 opp0} program
        (Nothing, Just opp1) -> runDuet d {cpu1 = eval cp1 opp1} program
        (Just opp0, Just opp1@(Sound _)) -> runDuet d {cpu0 = eval cp0 opp0, cpu1 = eval cp1 opp1, sendCount = succ (sendCount d)} program
        (Just opp0, Just opp1) -> runDuet d {cpu0 = eval cp0 opp0, cpu1 = eval cp1 opp1} program

-- 7493
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> runDuet mkDuet

solution :: Solution
solution = PureSolution solution1 3423 solution2 7493
