{-# LANGUAGE OverloadedStrings #-}

module Y2019.AOC7
  ( solution1
  , solution2
  )
where

import           Control.Monad.State.Strict (State, execState, get, gets,
                                             modify)
import           Data.Array                 (Array, listArray, (!), (//))
import           Data.List                  (permutations)
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
data Machine = Machine {
    memory :: Array Address Value,
    opCode :: Address, input :: [Value],
    output :: Value,
    runState :: RunState
    } deriving (Show, Eq)
type MachineState = State Machine
data RunState = Halt | WaitingForInput | WritingOutput | Running deriving (Show, Eq)

type FeedbackMachineState = State [Machine]

data ParameterMode = Position | Immediate

buildMachine :: [Value] -> Machine
buildMachine input' = Machine { memory   = convert input'
                              , opCode   = 0
                              , input    = []
                              , output   = 0
                              , runState = Running
                              }

connect :: Value -> Value -> Machine -> Machine
connect input' input'' m = m { input = [input', input''] }

perms :: [[Value]]
perms = permutations [0, 1, 2, 3, 4]
feedbackPerms :: [[Value]]
feedbackPerms = permutations [5, 6, 7, 8, 9]

runConnected :: Machine -> [Value] -> IO Value
runConnected m [s0, s1, s2, s3, s4] = do
  let m1 = connect s0 0 m
  o1 <- return . output $ execState runUntilHalt m1
  let m2 = connect s1 o1 m
  o2 <- return . output $ execState runUntilHalt m2
  let m3 = connect s2 o2 m
  o3 <- return . output $ execState runUntilHalt m3
  let m4 = connect s3 o3 m
  o4 <- return . output $ execState runUntilHalt m4
  let m5 = connect s4 o4 m
  return . output $ execState runUntilHalt m5
runConnected _ _ = error "unexpected configuration"

liveConnected :: Machine -> [Value] -> IO Value
liveConnected m [s0, s1, s2, s3, s4] = return . output . last $ execState
  runThrusterUntilHalt
  [ m { input = [s0, 0] }
  , m { input = [s1] }
  , m { input = [s2] }
  , m { input = [s3] }
  , m { input = [s4] }
  ]
liveConnected _ _ = error "unexpected configuration"

-- 298586
solution1 :: IO Value
solution1 = do
  ops <- parseFromFile parseOp "AOC7.input"
  let machine = buildMachine <$> ops
  case machine of
    Left  e -> error . show $ e
    Right m -> maximum <$> sequence (runConnected m <$> perms)

runThrusterUntilHalt :: FeedbackMachineState ()
runThrusterUntilHalt = do
  runStates <- gets $ fmap runState
  case runStates of
    [_, _, _, _, Halt] -> return ()
    [WritingOutput, _, _, _, _] ->
      modify
          (\machines -> case machines of
            (m0 : m1 : ms) ->
              m0 { runState = Running }
                : m1 { runState = Running, input = input m1 ++ [output m0] }
                : ms
            _ -> error "unexpected state"
          )
        >> runThrusterUntilHalt
    [_, WritingOutput, _, _, _] ->
      modify
          (\machines -> case machines of
            (m0 : m1 : m2 : ms) ->
              m0
                : m1 { runState = Running }
                : m2 { runState = Running, input = input m2 ++ [output m1] }
                : ms
            _ -> error "unexpected state"
          )
        >> runThrusterUntilHalt
    [_, _, WritingOutput, _, _] ->
      modify
          (\machines -> case machines of
            (m0 : m1 : m2 : m3 : ms) ->
              m0
                : m1
                : m2 { runState = Running }
                : m3 { runState = Running, input = input m3 ++ [output m2] }
                : ms
            _ -> error "unexpected state"
          )
        >> runThrusterUntilHalt
    [_, _, _, WritingOutput, _] ->
      modify
          (\machines -> case machines of
            (m0 : m1 : m2 : m3 : m4 : ms) ->
              m0
                : m1
                : m2
                : m3 { runState = Running }
                : m4 { runState = Running, input = input m4 ++ [output m3] }
                : ms
            _ -> error "unexpected state"
          )
        >> runThrusterUntilHalt
    [_, _, _, _, WritingOutput] ->
      modify
          (\machines -> case machines of
            (m0 : m1 : m2 : m3 : m4 : ms) ->
              m0 { runState = Running, input = input m0 ++ [output m4] }
                : m1
                : m2
                : m3
                : m4 { runState = Running }
                : ms
            _ -> error "unexpected state"
          )
        >> runThrusterUntilHalt
    _ -> tickAll >> runThrusterUntilHalt

tickAll :: FeedbackMachineState ()
tickAll = do
  machines <- get
  modify
    $   const
    $   (\m -> if runState m == Running then execState tick m else m)
    <$> machines

-- 9246095
solution2 :: IO Value
solution2 = do
  ops <- parseFromFile parseOp "AOC7.input"
  let machine = buildMachine <$> ops
  case machine of
    Left  e -> error . show $ e
    Right m -> maximum <$> sequence (liveConnected m <$> feedbackPerms)


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
    99   -> halt
    x    -> error $ "unknown opcode: " ++ show x

runUntilHalt :: MachineState ()
runUntilHalt = do
  runState' <- gets runState
  case runState' of
    Halt            -> return ()
    WaitingForInput -> error "waiting for input"
    _               -> tick >> runUntilHalt

halt :: MachineState ()
halt = modify (\s -> s { runState = Halt })

readInput :: ParameterMode -> MachineState ()
readInput p = do
  o      <- gets opCode
  input' <- gets input
  case input' of
    []       -> modify (\s -> s { runState = WaitingForInput })
    (x : xs) -> do
      store p (o + 1) x
      modify (\s -> s { opCode = o + 2, input = xs })

writeOutput :: ParameterMode -> MachineState ()
writeOutput p = do
  o   <- gets opCode
  val <- load p (o + 1)
  modify (\s -> s { opCode = o + 2, output = val, runState = WritingOutput })

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
  let m  = (buildMachine [3, 1, 99]) { input = [42] }
  let m' = execState tick m
  m' H.=== (buildMachine [3, 42, 99]) { opCode = 2, input = [] }

prop_in_out :: H.Property
prop_in_out = H.property $ do
  in' <- H.forAll $ Gen.int (Range.linear 0 100)
  let m  = (buildMachine [3, 0, 4, 0, 99]) { input = [in'] }
  let m' = execState runUntilHalt m
  output m' H.=== in'

prop_example :: H.Property
prop_example = H.property $ do
  let m  = buildMachine [1, 1, 1, 4, 99, 5, 6, 0, 99]
  let m' = execState runUntilHalt m
  m' H.=== (buildMachine [30, 1, 1, 4, 2, 5, 6, 0, 99]) { opCode   = 8
                                                        , runState = Halt
                                                        }

prop_input_example :: H.Property
prop_input_example = H.property $ do
  let m  = (buildMachine [3, 0, 1, 0, 6, 6, 1100]) { input = [1] }
  let m' = execState (tick >> tick) m
  m' H.=== (buildMachine [1, 0, 1, 0, 6, 6, 1101]) { opCode = 6, input = [] }

prop_example2 :: H.Property
prop_example2 = H.property $ do
  let m = (buildMachine
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
          ) { input = [0]
            }
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
