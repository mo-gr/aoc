{-# LANGUAGE OverloadedStrings #-}

module Y2019.AOC2 (solution1, solution2) where

import           Control.Monad.State.Strict (State, execState, gets, put)
import           Data.Array                 (Array, listArray, (!), (//))
import           Data.Maybe                 (isJust)
import           Text.Parsec                (digit, many1, sepBy, string)
import           Text.Parsec.ByteString     (Parser, parseFromFile)

number :: Parser Int
number = read <$> many1 digit

parseOp :: Parser [Int]
parseOp = number `sepBy` string ","

convert :: [Int] -> Array Int Int
convert xs = listArray (0, length xs - 1) xs

type Value = Int
type Address = Int
data Machine = Machine { memory :: Array Address Value, opCode :: Address }
type MachineState = State Machine

buildMachine :: [Value] -> Machine
buildMachine input = Machine { memory = convert input, opCode = 0 }

setup :: Value -> Value -> Machine -> Machine
setup verb noun m =
  Machine { memory = memory m // [(1, verb), (2, noun)], opCode = 0 }

-- 4576384
solution1 :: IO Value
solution1 = do
  ops <- parseFromFile parseOp "AOC2.input"
  let input = setup 12 2 . buildMachine <$> ops
  case input of
    Right m -> return . (! 0) . memory $ execState runUntilHalt m
    Left  e -> error $ show e

desired :: Int
desired = 19690720

inputs :: [(Int, Int)]
inputs = (,) <$> [0 .. 99] <*> [0 .. 99]

solves :: Machine -> (Int, Int) -> Maybe (Int, Int)
solves m (verb, noun) =
  let p'  = setup verb noun m
      out = (! 0) . memory $ execState runUntilHalt p'
  in  if desired == out then Just (verb, noun) else Nothing

brute :: Machine -> Maybe (Int, Int)
brute p = head . filter isJust $ (solves p <$> inputs)

format :: (Int, Int) -> Int
format (x, y) = x * 100 + y

-- 5398
solution2 :: IO (Maybe Int)
solution2 = do
  ops <- parseFromFile parseOp "AOC2.input"
  case ops of
    Right o -> return . fmap format . brute $ buildMachine o
    Left  e -> error $ show e

runUntilHalt :: MachineState ()
runUntilHalt = do
  opAddr    <- gets opCode
  operation <- loadMemory opAddr
  case operation of
    1  -> opAdd >> runUntilHalt
    2  -> opMul >> runUntilHalt
    99 -> return ()
    x  -> error $ "unknown opcode: " ++ show x

opAdd :: MachineState ()
opAdd = mathOp (+)
opMul :: MachineState ()
opMul = mathOp (*)

mathOp :: (Value -> Value -> Value) -> MachineState ()
mathOp op = do
  o  <- gets opCode
  a1 <- loadIndirect (o + 1)
  a2 <- loadIndirect (o + 2)
  storeIndirect (o + 3) (a1 `op` a2)
  m' <- gets memory
  put $ Machine { memory = m', opCode = o + 4 }

loadMemory :: Address -> MachineState Value
loadMemory x = do
  m <- gets memory
  return $ m ! x

loadIndirect :: Address -> MachineState Value
loadIndirect x = do
  m <- gets memory
  let ref = m ! x
  return $ m ! ref

storeIndirect :: Address -> Value -> MachineState ()
storeIndirect x v = do
  m      <- gets memory
  o      <- gets opCode
  target <- loadMemory x
  let m' = m // [(target, v)]
  put $ Machine { memory = m', opCode = o }
