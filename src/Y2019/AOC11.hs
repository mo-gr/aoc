{-# LANGUAGE OverloadedStrings #-}

module Y2019.AOC11
    ( solution1
    , solution2
    )
where

import           Control.Monad.State.Strict (State, execState, gets, modify,
                                             modify')
import qualified Data.Map.Strict            as Map
import           Data.Set                   (fromList)
import qualified Hedgehog                   as H
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range
import           Text.Parsec                (digit, many1, parse, sepBy, string,
                                             (<|>))
import           Text.Parsec.ByteString     (Parser, parseFromFile)
--import           Debug.Trace                    ( trace )

number :: Parser Int
number = read <$> many1 digit
negativeNumber :: Parser Int
negativeNumber = negate . read <$> (string "-" *> many1 digit)

parseOp :: Parser [Int]
parseOp = (number <|> negativeNumber) `sepBy` string ","

convert :: [Int] -> Map.Map Int Int
convert xs = Map.fromList $ zip [0 ..] xs

type Value = Int
type Address = Int
data Machine = Machine {
    memory   :: Map.Map Address Value,
    opCode   :: Address,
    input    :: [Value],
    output   :: [Value],
    relBase  :: Address,
    runState :: RunState,
    robot    :: Robot
    } deriving (Show, Eq)
type MachineState = State Machine
data RunState = Halt | WaitingForInput | Running deriving (Show, Eq)

data ParameterMode = Position | Immediate | Relative deriving (Show, Eq)

data Robot = Robot { direction :: RobotDirection, path :: [(Point, Paint)] } deriving (Show, Eq)
data RobotDirection = UP | DOWN | LEFT | RIGHT deriving (Show, Eq)
type Point = (Int, Int)
data Paint = Black | White deriving (Show, Eq)

nextDirection :: RobotDirection -> Value -> RobotDirection
nextDirection UP    0 = LEFT
nextDirection UP    1 = RIGHT
nextDirection DOWN  0 = RIGHT
nextDirection DOWN  1 = LEFT
nextDirection LEFT  0 = DOWN
nextDirection LEFT  1 = UP
nextDirection RIGHT 0 = UP
nextDirection RIGHT 1 = DOWN
nextDirection _     _ = error "invalid robot command"

paintPanel :: Value -> Robot -> Robot
paintPanel 0 r@Robot { path = ((p, _) : ps) } = r { path = (p, Black) : ps }
paintPanel 1 r@Robot { path = ((p, _) : ps) } = r { path = (p, White) : ps }
paintPanel _ _                                = error "invalid robot paint"

turnRobot :: Value -> Robot -> Robot
turnRobot v r@Robot { direction = dir } =
    let nextDir = nextDirection dir v in r { direction = nextDir }

moveRobot :: Robot -> Robot
moveRobot r@Robot { direction = direction', path = p@(((x, y), _) : _) }
    | direction' == UP = r { path = ((x, y + 1), currentColor p (x, y + 1)) : p
                           }
moveRobot r@Robot { direction = direction', path = p@(((x, y), _) : _) }
    | direction' == DOWN = r
        { path = ((x, y - 1), currentColor p (x, y - 1)) : p
        }
moveRobot r@Robot { direction = direction', path = p@(((x, y), _) : _) }
    | direction' == LEFT = r
        { path = ((x - 1, y), currentColor p (x - 1, y)) : p
        }
moveRobot r@Robot { direction = direction', path = p@(((x, y), _) : _) }
    | direction' == RIGHT = r
        { path = ((x + 1, y), currentColor p (x + 1, y)) : p
        }
moveRobot r = error $ "invalid robot move: " ++ show r

currentColor :: [(Point, Paint)] -> Point -> Paint
currentColor [] _            = Black
currentColor ((p', c) : _) p | p == p' = c
currentColor (_ : ps) p      = currentColor ps p

buildMachine :: [Value] -> Machine
buildMachine input' = Machine { memory   = convert input'
                              , opCode   = 0
                              , relBase  = 0
                              , input    = []
                              , output   = []
                              , runState = Running
                              , robot    = Robot UP [((0, 0), Black)]
                              }

uniqueTiles :: Robot -> Int
uniqueTiles Robot { path = p } = length . fromList $ fst <$> p

printPath :: [(Point, Paint)] -> IO ()
printPath p = let minX = minimum $ fst.fst <$> p
                  maxX = maximum $ fst.fst <$> p
                  minY = minimum $ snd.fst <$> p
                  maxY = maximum $ snd.fst <$> p
                  charToPrint :: [(Point, Paint)] -> Point -> String
                  charToPrint [] _               = " "
                  charToPrint ((p', White): _) q | q == p' = "#"
                  charToPrint (_p:ps) q          = charToPrint ps q
                  printLine :: [Int] -> Int -> IO ()
                  printLine [] _ = putStrLn ""
                  printLine (x:xs) y = putStr (charToPrint p (x,y)) >> printLine xs y
              in sequence_ $ printLine [minX..maxX] <$> reverse [minY..maxY]

-- 2539
solution1 :: IO ()
solution1 = do
    ops <- parseFromFile parseOp "AOC11.input"
    let machine = buildMachine <$> ops
    case machine of
        Left  e -> error (show e)
        Right m -> print . uniqueTiles . robot $ execState runUntilHalt m

-- ZLEBKJRA
solution2 :: IO ()
solution2 = do
    ops <- parseFromFile parseOp "AOC11.input"
    let machine = buildMachine <$> ops
    case machine of
        Left  e -> error (show e)
        Right m -> printPath . path . robot $ execState runUntilHalt (m {robot=Robot UP [((0, 0), White)]})


tick :: MachineState ()
tick = do
    opAddr    <- gets opCode
    operation <- load Immediate opAddr
    case operation `mod` 100 of
        1  -> opAdd `uncurrry` decodeParameterMode3 operation
        2  -> opMul `uncurrry` decodeParameterMode3 operation
        3  -> readInput (decodeParameterMode1 operation)
        4  -> writeOutput (decodeParameterMode1 operation)
        5  -> jmpIfTrue `uncurry` decodeParameterMode2 operation
        6  -> jmpIfFalse `uncurry` decodeParameterMode2 operation
        7  -> opLT `uncurrry` decodeParameterMode3 operation
        8  -> opEq `uncurrry` decodeParameterMode3 operation
        9  -> adjustRelBase (decodeParameterMode1 operation)
        99 -> halt
        x  -> error $ "unknown opcode: " ++ show x

uncurrry :: (a -> b -> c -> d) -> (a, (b, c)) -> d
uncurrry f (a, (b, c)) = f a b c

nthDigit :: Int -> Value -> Int
nthDigit 0 x = x `mod` 10
nthDigit n x = (x `div` (10 ^ n)) `mod` 10

toParameterMode :: Int -> ParameterMode
toParameterMode 0 = Position
toParameterMode 1 = Immediate
toParameterMode 2 = Relative
toParameterMode _ = error "unknown parameter mode"

decodeParameterMode3 :: Value -> (ParameterMode, (ParameterMode, ParameterMode))
decodeParameterMode3 x =
    ( toParameterMode . nthDigit 2 $ x
    , (toParameterMode . nthDigit 3 $ x, toParameterMode . nthDigit 4 $ x)
    )

decodeParameterMode2 :: Value -> (ParameterMode, ParameterMode)
decodeParameterMode2 x =
    (toParameterMode . nthDigit 2 $ x, toParameterMode . nthDigit 3 $ x)

decodeParameterMode1 :: Value -> ParameterMode
decodeParameterMode1 = toParameterMode . nthDigit 2

currentColorFromRobo :: Robot -> Value
currentColorFromRobo robo = case currentColor (path robo) (fst . head . path $ robo) of
    Black -> 0
    White -> 1

runRobot :: MachineState ()
runRobot = do
    robotControl <- gets output
    robo <- gets robot
    case robotControl of
        (color : control : _) -> do
            let robo' = moveRobot . turnRobot control . paintPanel color $ robo
            modify
                (\s -> s { robot    = robo'
                         , input    = [currentColorFromRobo robo']
                         , runState = Running
                         , output   = []
                         }
                )
        _ -> modify (\s -> s { input = [currentColorFromRobo robo], runState = Running })

runUntilHalt :: MachineState ()
runUntilHalt = do
    runState' <- gets runState
    case runState' of
        Halt            -> return ()
        WaitingForInput -> runRobot >> runUntilHalt
        Running         -> do
            tick
            runUntilHalt

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

adjustRelBase :: ParameterMode -> MachineState ()
adjustRelBase p = do
    o        <- gets opCode
    val      <- load p (o + 1)
    relBase' <- gets relBase
    modify' (\s -> s { opCode = o + 2, relBase = val + relBase' })

writeOutput :: ParameterMode -> MachineState ()
writeOutput p = do
    o       <- gets opCode
    val     <- load p (o + 1)
    output' <- gets output
    modify' (\s -> s { opCode = o + 2, output = output' ++ [val] })

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

opAdd :: ParameterMode -> ParameterMode -> ParameterMode -> MachineState ()
opAdd = mathOp (+)
opMul :: ParameterMode -> ParameterMode -> ParameterMode -> MachineState ()
opMul = mathOp (*)
opEq :: ParameterMode -> ParameterMode -> ParameterMode -> MachineState ()
opEq = mathOp (\a b -> if a == b then 1 else 0)
opLT :: ParameterMode -> ParameterMode -> ParameterMode -> MachineState ()
opLT = mathOp (\a b -> if a < b then 1 else 0)


mathOp
    :: (Value -> Value -> Value)
    -> ParameterMode
    -> ParameterMode
    -> ParameterMode
    -> MachineState ()
mathOp op p1 p2 p3 = do
    o  <- gets opCode
    a1 <- load p1 (o + 1)
    a2 <- load p2 (o + 2)
    store p3 (o + 3) (a1 `op` a2)
    m' <- gets memory
    modify (\s -> s { memory = m', opCode = o + 4 })

loadDirect :: Address -> MachineState Value
loadDirect x = do
    m <- gets memory
    return $ Map.findWithDefault 0 x m

loadIndirect :: Address -> MachineState Value
loadIndirect x = do
    m <- gets memory
    let ref = Map.findWithDefault 0 x m
    return $ Map.findWithDefault 0 ref m

loadRelative :: Address -> MachineState Value
loadRelative x = do
    m   <- gets memory
    rel <- gets relBase
    let ref = Map.findWithDefault 0 x m
    return $ Map.findWithDefault 0 (rel + ref) m

store' :: Address -> Value -> MachineState ()
store' targetAddr v = do
    m      <- gets memory
    target <- load Immediate targetAddr
    let m' = Map.insert target v m
    modify (\s -> s { memory = m' })

storeRelative :: Address -> Value -> MachineState ()
storeRelative targetAddr v = do
    m        <- gets memory
    relBase' <- gets relBase
    target   <- load Immediate targetAddr
    let m' = Map.insert (target + relBase') v m
    modify (\s -> s { memory = m' })

store :: ParameterMode -> Address -> Value -> MachineState ()
store Immediate = error "there is no store immediate"
store Relative  = storeRelative
store Position  = store'

load :: ParameterMode -> Address -> MachineState Value
load Immediate = loadDirect
load Position  = loadIndirect
load Relative  = loadRelative

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
    output m' H.=== [in']

prop_example :: H.Property
prop_example = H.withTests 1 $ H.property $ do
    let m  = buildMachine [1, 1, 1, 4, 99, 5, 6, 0, 99]
    let m' = execState runUntilHalt m
    m' H.=== (buildMachine [30, 1, 1, 4, 2, 5, 6, 0, 99]) { opCode   = 8
                                                          , runState = Halt
                                                          }

prop_input_example :: H.Property
prop_input_example = H.withTests 1 $ H.property $ do
    let m  = (buildMachine [3, 0, 1, 0, 6, 6, 1100]) { input = [1] }
    let m' = execState (tick >> tick) m
    m' H.=== (buildMachine [1, 0, 1, 0, 6, 6, 1101]) { opCode = 6, input = [] }

prop_example2 :: H.Property
prop_example2 = H.withTests 1 $ H.property $ do
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
    output m' H.=== [999]

prop_quine :: H.Property
prop_quine = H.withTests 1 $ H.property $ do
    let quine =
            [ 109
            , 1
            , 204
            , -1
            , 1001
            , 100
            , 1
            , 100
            , 1008
            , 100
            , 16
            , 101
            , 1006
            , 101
            , 0
            , 99
            ]
    let m  = buildMachine quine
    let m' = execState runUntilHalt m
    output m' H.=== quine

prop_large_num :: H.Property
prop_large_num = H.withTests 1 $ H.property $ do
    let m  = buildMachine [104, 1125899906842624, 99]
    let m' = execState runUntilHalt m
    output m' H.=== [1125899906842624]

prop_long_num :: H.Property
prop_long_num = H.withTests 1 $ H.property $ do
    let m = buildMachine [1102, 34915192, 34915192, 7, 4, 7, 99, 0]
    let m' = execState runUntilHalt m
    output m' H.=== [1219070632396864]

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
    , ("prop_quine"        , prop_quine)
    , ("prop_large_num"    , prop_large_num)
    , ("prop_long_num"     , prop_long_num)
    ]

