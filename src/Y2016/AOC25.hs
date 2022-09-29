{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC25 where

import AOC (Solution (PureSolution))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>), negativeNumber)
import Text.Parsec (many1, newline, char, try, string)
import Data.Foldable (asum)
import Data.Functor (($>), (<&>))
import Control.Applicative ((<|>))
import Debug.Trace (traceShow)

data Assembunny a = Assembunny
  { regA :: a,
    regB :: a,
    regC :: a,
    regD :: a,
    pc :: Int,
    program :: [Instruction a]
  }

data Operand a = RegA | RegB | RegC | RegD | Value a
  deriving (Show)

instance Show a => Show (Assembunny a) where
  show b = show (pc b) <> ":" <> show (program b)

writePc :: Int -> Assembunny Int -> Assembunny Int
writePc v b = b {pc = v}

writeOp :: Operand Int -> Int -> Assembunny Int -> Assembunny Int
writeOp RegA v b = b {regA = v}
writeOp RegB v b = b {regB = v}
writeOp RegC v b = b {regC = v}
writeOp RegD v b = b {regD = v}
writeOp Value {} _ _ = error "attempting invalid write"

readOp :: Operand Int -> Assembunny Int -> Int
readOp RegA = regA
readOp RegB = regB
readOp RegC = regC
readOp RegD = regD
readOp (Value v) = const v

data Instruction a
  = Cpy (Operand a) (Operand a)
  | Inc (Operand a)
  | Dec (Operand a)
  | Jnz (Operand a) (Operand a)
  | Out (Operand a)
  | Noop
  deriving (Show)

mkBunny :: [Instruction Int] -> Assembunny Int
mkBunny = Assembunny 0 0 0 0 0

inputParser :: Parser [Instruction Int]
inputParser = many1 (instructionP <* newline)
  where
    instructionP = asum [copyP, incP, decP, jnzP, outP]
    operandP = (char 'a' $> RegA) <|> (char 'b' $> RegB) <|> (char 'c' $> RegC) <|> (char 'd' $> RegD) <|> (negativeNumber <&> Value)
    copyP = try $ do
      _ <- string "cpy "
      a <- operandP
      _ <- string " "
      Cpy a <$> operandP
    incP = do
      _ <- string "inc "
      Inc <$> operandP
    decP = do
      _ <- string "dec "
      Dec <$> operandP
    jnzP = do
      _ <- string "jnz "
      conditionReg <- operandP
      _ <- string " "
      Jnz conditionReg <$> operandP
    outP = do
      _ <- string "out "
      Out <$> operandP

safeIndex :: Int -> [a] -> Maybe a
safeIndex a as
  | a < 0 = Nothing
  | a >= length as = Nothing
  | otherwise = Just $ as !! a

runTilHalt :: Int -> Assembunny Int -> Assembunny Int
runTilHalt 0 _ = error "no result"
runTilHalt limit bunny = case drop (pc bunny) (program bunny) of
  [] -> bunny
  instr : _ -> eval instr bunny |> runTilHalt (pred limit)

eval :: Instruction Int -> Assembunny Int -> Assembunny Int
eval (Cpy f to) b = writeOp to (readOp f b) b |> nextPc
eval (Inc r) b = readOp r b |> succ |> flip (writeOp r) b |> nextPc
eval (Dec r) b = readOp r b |> pred |> flip (writeOp r) b |> nextPc
eval (Jnz cnd j) b = case readOp cnd b of
  0 -> nextPc b
  _ -> writePc (pc b + readOp j b) b
eval Noop b = nextPc b
eval (Out r) b = traceShow (readOp r b) b |> nextPc


nextPc :: Assembunny Int -> Assembunny Int
nextPc b = b {pc = succ $ pc b}

solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> mkBunny
    |> runTilHalt 100000000
    |> regA 

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> undefined

solution :: Solution
solution = PureSolution solution1 undefined solution2 undefined

testData :: Input
testData = "cpy 1 a\ncpy 1 b\ncpy 26 d\njnz c 2\njnz 1 5\ncpy 7 c\ninc d\ndec c\njnz c -2\ncpy a c\ninc a\ndec b\njnz b -2\ncpy c b\ndec d\njnz d -6\ncpy 16 c\ncpy 17 d\ninc a\ndec d\njnz d -2\ndec c\njnz c -5\n"