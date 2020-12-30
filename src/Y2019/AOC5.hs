{-# LANGUAGE OverloadedStrings #-}

module Y2019.AOC5 where

import           Control.Monad.State.Strict (State, execState, gets, modify)
import           Data.Array                 (Array, listArray, (!), (//))
import qualified Hedgehog                   as H
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range
import           Text.Parsec                (digit, many1, parse, sepBy, string,
                                             (<|>))
import           Text.Parsec.ByteString     (Parser, parseFromFile)

number :: Parser Int
number = read <$> many1 digit

negativeNumber :: Parser Int
negativeNumber = negate . read <$> (string "-" *> many1 digit)

parseOp :: Parser [Int]
parseOp = (number <|> negativeNumber) `sepBy` string ","

convert :: [Int] -> Array Int Int
convert xs = listArray (0, length xs - 1) xs

type Value = Int
type Address = Int
data Machine = Machine { memory :: Array Address Value, opCode :: Address, input :: Value, output :: Value } deriving (Show, Eq)
type MachineState = State Machine

data ParameterMode = Position | Immediate

buildMachine :: [Value] -> Machine
buildMachine input' =
  Machine { memory = convert input', opCode = 0, input = 0, output = 0 }

setup :: Value -> Value -> Machine -> Machine
setup input' output' m = m { input = input', output = output' }

-- 14155342
solution1 :: IO Value
solution1 = do
  ops <- parseFromFile parseOp "AOC5.input"
  let input' = setup 1 0 . buildMachine <$> ops
  case input' of
    Right m -> return . output $ execState runUntilHalt m
    Left  e -> error $ show e

-- 8684145
solution2 :: IO Value
solution2 = do
  ops <- parseFromFile parseOp "AOC5.input"
  let input' = setup 5 0 . buildMachine <$> ops
  case input' of
    Right m -> return . output $ execState runUntilHalt m
    Left  e -> error $ show e


tick :: MachineState ()
tick = do
  opAddr    <- gets opCode
  operation <- load Immediate opAddr
  case operation of
    1    -> opAdd Position Position
    101  -> opAdd Immediate Position
    1001 -> opAdd Position Immediate
    1101 -> opAdd Immediate Immediate
    2    -> opMul Position Position
    102  -> opMul Immediate Position
    1002 -> opMul Position Immediate
    1102 -> opMul Immediate Immediate
    3    -> readInput Position
    103  -> readInput Immediate
    4    -> writeOutput Position
    104  -> writeOutput Immediate
    5    -> jmpIfTrue Position Position
    105  -> jmpIfTrue Immediate Position
    1005 -> jmpIfTrue Position Immediate
    1105 -> jmpIfTrue Immediate Immediate
    6    -> jmpIfFalse Position Position
    106  -> jmpIfFalse Immediate Position
    1006 -> jmpIfFalse Position Immediate
    1106 -> jmpIfFalse Immediate Immediate
    7    -> opLT Position Position
    107  -> opLT Immediate Position
    1007 -> opLT Position Immediate
    1107 -> opLT Immediate Immediate
    8    -> opEq Position Position
    108  -> opEq Immediate Position
    1008 -> opEq Position Immediate
    1108 -> opEq Immediate Immediate
    99   -> return ()
    x    -> error $ "unknown opcode: " ++ show x

runUntilHalt :: MachineState ()
runUntilHalt = do
  opAddr    <- gets opCode
  operation <- load Immediate opAddr
  case operation of
    99 -> return ()
    _  -> tick >> runUntilHalt

readInput :: ParameterMode -> MachineState ()
readInput p = do
  o      <- gets opCode
  input' <- gets input
  store p (o + 1) input'
  modify (\s -> s { opCode = o + 2 })

writeOutput :: ParameterMode -> MachineState ()
writeOutput p = do
  o   <- gets opCode
  val <- load p (o + 1)
  modify (\s -> s { opCode = o + 2, output = val })

jmpIfTrue :: ParameterMode -> ParameterMode -> MachineState ()
jmpIfTrue = jmpCond (/= 0)
jmpIfFalse :: ParameterMode -> ParameterMode -> MachineState ()
jmpIfFalse = jmpCond (== 0)

jmpCond :: (Value -> Bool) -> ParameterMode -> ParameterMode -> MachineState ()
jmpCond cond p1 p2 = do
  o    <- gets opCode
  arg  <- load p1 (o + 1)
  dest <- load p2 (o + 2)
  modify (\s -> s { opCode = if cond arg then dest else o + 3 })

opAdd :: ParameterMode -> ParameterMode -> MachineState ()
opAdd = mathOp (+)
opMul :: ParameterMode -> ParameterMode -> MachineState ()
opMul = mathOp (*)
opEq :: ParameterMode -> ParameterMode -> MachineState ()
opEq = mathOp (\a b -> if a == b then 1 else 0)
opLT :: ParameterMode -> ParameterMode -> MachineState ()
opLT = mathOp (\a b -> if a < b then 1 else 0)


mathOp
  :: (Value -> Value -> Value)
  -> ParameterMode
  -> ParameterMode
  -> MachineState ()
mathOp op p1 p2 = do
  o  <- gets opCode
  a1 <- load p1 (o + 1)
  a2 <- load p2 (o + 2)
  store Position (o + 3) (a1 `op` a2)
  m' <- gets memory
  modify (\s -> s { memory = m', opCode = o + 4 })

loadDirect :: Address -> MachineState Value
loadDirect x = do
  m <- gets memory
  return $ m ! x

loadIndirect :: Address -> MachineState Value
loadIndirect x = do
  m <- gets memory
  let ref = m ! x
  return $ m ! ref

store' :: Address -> Value -> MachineState ()
store' targetAddr v = do
  m      <- gets memory
  target <- load Immediate targetAddr
  let m' = m // [(target, v)]
  modify (\s -> s { memory = m' })

store :: ParameterMode -> Address -> Value -> MachineState ()
store Immediate = error "there is no store immediate"
store Position  = store'

load :: ParameterMode -> Address -> MachineState Value
load Immediate = loadDirect
load Position  = loadIndirect

-- TESTS

prop_parser :: H.Property
prop_parser =
  H.withTests 1 $ H.property $ case parse parseOp "test" "101,-1,0,0,99" of
    Right x -> H.assert $ x == [101, -1, 0, 0, 99]
    Left  e -> H.footnote (show e) >> H.failure

prop_example_mul :: H.Property
prop_example_mul = H.withTests 1 $ H.property $ do
  let m  = buildMachine [1002, 4, 3, 4, 33]
  let m' = execState tick m
  m' H.=== (buildMachine [1002, 4, 3, 4, 99]) { opCode = 4 }

prop_example_add :: H.Property
prop_example_add = H.withTests 1 $ H.property $ do
  let m  = buildMachine [1101, 100, -1, 4, 0]
  let m' = execState tick m
  m' H.=== (buildMachine [1101, 100, -1, 4, 99]) { opCode = 4 }

prop_example_read :: H.Property
prop_example_read = H.withTests 1 $ H.property $ do
  let m  = (buildMachine [3, 1, 99]) { input = 42 }
  let m' = execState tick m
  m' H.=== (buildMachine [3, 42, 99]) { opCode = 2, input = 42 }

prop_in_out :: H.Property
prop_in_out = H.property $ do
  in' <- H.forAll $ Gen.int (Range.linear 0 100)
  let m  = (buildMachine [3, 0, 4, 0, 99]) { input = in' }
  let m' = execState runUntilHalt m
  output m' H.=== in'

prop_example :: H.Property
prop_example = H.property $ do
  let m  = buildMachine [1, 1, 1, 4, 99, 5, 6, 0, 99]
  let m' = execState runUntilHalt m
  m' H.=== (buildMachine [30, 1, 1, 4, 2, 5, 6, 0, 99]) { opCode = 8 }

prop_input_example :: H.Property
prop_input_example = H.property $ do
  let m  = (buildMachine [3, 0, 1, 0, 6, 6, 1100]) { input = 1 }
  let m' = execState (tick >> tick) m
  m' H.=== (buildMachine [1, 0, 1, 0, 6, 6, 1101]) { opCode = 6, input = 1 }

prop_example2 :: H.Property
prop_example2 = H.property $ do
  let m = buildMachine
        [ 3
        , 21
        , 1008
        , 21
        , 8
        , 20
        , 1005
        , 20
        , 22
        , 107
        , 8
        , 21
        , 20
        , 1006
        , 20
        , 31
        , 1106
        , 0
        , 36
        , 98
        , 0
        , 0
        , 1002
        , 21
        , 125
        , 20
        , 4
        , 20
        , 1105
        , 1
        , 46
        , 104
        , 999
        , 1105
        , 1
        , 46
        , 1101
        , 1000
        , 1
        , 20
        , 4
        , 20
        , 1105
        , 1
        , 46
        , 98
        , 99
        ]
  let m' = execState runUntilHalt m
  output m' H.=== 999

_tests :: IO Bool
_tests = H.checkParallel $ H.Group
  "AOC5"
  [ ("prop_parser"       , prop_parser)
  , ("prop_example_mul"  , prop_example_mul)
  , ("prop_example_add"  , prop_example_add)
  , ("prop_example_read" , prop_example_read)
  , ("prop_in_out"       , prop_in_out)
  , ("prop_example"      , prop_example)
  , ("prop_example2"     , prop_example2)
  , ("prop_input_example", prop_input_example)
  ]
