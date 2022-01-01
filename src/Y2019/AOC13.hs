{-# LANGUAGE OverloadedStrings #-}

module Y2019.AOC13
    ( solution1
    , solution2
    )
where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (when, void)
import           Control.Monad.State.Strict (StateT, execStateT, gets, modify,
                                             modify')
import           Control.Monad.Trans        (lift)
import qualified Data.Map.Strict            as Map
import           System.IO
import           Text.Parsec                (digit, many1, sepBy, string, (<|>))
import           Text.Parsec.ByteString     (Parser, parseFromFile)
--import           Debug.Trace                    ( trace )
import qualified System.Console.ANSI        as ANSI

execState :: StateT s IO a -> s -> IO s
execState = execStateT

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
    ballX    :: Value,
    paddleX  :: Value
    } deriving (Show, Eq)
type MachineState = StateT Machine IO
data RunState = Halt | WaitingForInput | Running deriving (Show, Eq)

data ParameterMode = Position | Immediate | Relative deriving (Show, Eq)

buildMachine :: [Value] -> Machine
buildMachine input' = Machine { memory   = convert input'
                              , opCode   = 0
                              , relBase  = 0
                              , input    = []
                              , output   = []
                              , runState = Running
                              , ballX    = 0
                              , paddleX  = 0
                              }

countBlocks :: [Value] -> Int
countBlocks []                     = 0
countBlocks (_x : _y : 2     : vs) = 1 + countBlocks vs
countBlocks (_x : _y : _tile : vs) = countBlocks vs
countBlocks _                      = error "invalid output"

-- 427
solution1 :: IO Int
solution1 = do
    ops <- parseFromFile parseOp "AOC13.input"
    let machine = buildMachine <$> ops
    case machine of
        Left  e -> error (show e)
        Right m -> countBlocks . output <$> execState runUntilHalt m

solution2 :: IO ()
solution2 = do
    ops <- parseFromFile parseOp "AOC13.input"
    let machine = buildMachine <$> ops

    case machine of
        Left  e -> error (show e)
        Right m -> do
            let m' = m { memory = Map.insert 0 2 $ memory m, input = [0] }
            void (execState runUntilHaltIO m')

getTilePrint :: Int -> String
getTilePrint 0 = " "
getTilePrint 1 = "#"
getTilePrint 2 = "X"
getTilePrint 3 = "-"
getTilePrint 4 = "."
getTilePrint x = error $ "invalid output: " ++ show x

printScreen :: [Value] -> IO ()
printScreen p =
    let printOutput :: [Int] -> IO ()
        printOutput []                = return ()
        printOutput (-1 : _y : z : xs) = do
            ANSI.setCursorPosition 28 2
            print z
            printOutput xs
        printOutput (x : y : z : xs) = do
            ANSI.setCursorPosition y x
            putStr $ getTilePrint z
            printOutput xs
        printOutput _ = return ()
    in  do
            printOutput p
            hFlush stdout


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

runUntilHalt :: MachineState ()
runUntilHalt = do
    runState' <- gets runState
    case runState' of
        Halt            -> return ()
        WaitingForInput -> error "waiting for input"
        Running         -> do
            tick
            runUntilHalt

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
        char <- getChar
        more <- hReady stdin
        (if more then getKey' else return) (char : chars)

_readKey :: IO (Maybe Value)
_readKey = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    ready <- hReady stdin
    key   <- if ready then getKey else return ""
    return $ case key of
        "\ESC[C" -> Just 1
        "\ESC[D" -> Just (-1)
        _        -> Nothing

runUntilHaltIO :: MachineState ()
runUntilHaltIO = do
    runState' <- gets runState
    case runState' of
        Halt            -> return ()
        WaitingForInput -> error "waiting for input"
        Running         -> do
            output' <- gets output
            when (length output' >= 3) $ do
                lift $ printScreen (take 3 output')
                modify (\s -> s { output = drop 3 output' })
                when (length output' < 10) $ lift $ threadDelay 500
            case output' of
                (x : _y : 4 : _) -> modify (\s -> s { ballX = x })
                (x : _y : 3 : _) -> modify (\s -> s { paddleX = x })
                _                -> return ()
            bX <- gets ballX
            pX <- gets paddleX
            when (bX > pX) $ modify (\s -> s { input = [1] })
            when (bX < pX) $ modify (\s -> s { input = [-1] })
            when (bX == pX) $ modify (\s -> s { input = [0] })
            -- k <- lift _readKey
            -- case k of
            --     Just k' -> modify (\s -> s {input=[k']})
            --     Nothing -> return ()
            tick
            runUntilHaltIO

halt :: MachineState ()
halt = modify (\s -> s { runState = Halt })

readInput :: ParameterMode -> MachineState ()
readInput p = do
    o      <- gets opCode
    input' <- gets input
    case input' of
        []       -> modify (\s -> s { runState = WaitingForInput })
        (x : _) -> do
            store p (o + 1) x
            modify (\s -> s { opCode = o + 2, input = [] })

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
