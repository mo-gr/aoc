{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Y2016.AOC25 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.Functor (($>), (<&>))
import Data.Maybe (catMaybes)
import Text.Parsec (char, many1, newline, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, parseOrDie, (|>))

data Assembunny a = Assembunny
  { regA :: a,
    regB :: a,
    regC :: a,
    regD :: a,
    pc :: Int,
    output :: [Int],
    program :: [Instruction a]
  }

data Operand a = RegA | RegB | RegC | RegD | Value a
  deriving (Show)

instance Show a => Show (Assembunny a) where
  show b = show (pc b) <> ":" <> show (output b)

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
mkBunny = Assembunny 0 0 0 0 0 []

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

hasInvalidOutput :: Assembunny Int -> Bool
hasInvalidOutput Assembunny {output} = not $ isValid output
  where
    isValid [] = True
    isValid [0] = True
    isValid (1 : 0 : rest) = isValid (0 : rest)
    isValid (0 : 1 : rest) = isValid (1 : rest)
    isValid _ = False

runTilHalt :: Int -> Assembunny Int -> Maybe (Assembunny Int)
runTilHalt 0 b = Just b
runTilHalt _ b | hasInvalidOutput b = Nothing
runTilHalt limit bunny = case drop (pc bunny) (program bunny) of
  [] -> Nothing -- programm ended, so it doesn't produce infinite signal
  instr : _ -> eval instr bunny |> runTilHalt (pred limit)

eval :: Instruction Int -> Assembunny Int -> Assembunny Int
eval (Cpy f to) b = writeOp to (readOp f b) b |> nextPc
eval (Inc r) b = readOp r b |> succ |> flip (writeOp r) b |> nextPc
eval (Dec r) b = readOp r b |> pred |> flip (writeOp r) b |> nextPc
eval (Jnz cnd j) b = case readOp cnd b of
  0 -> nextPc b
  _ -> writePc (pc b + readOp j b) b
eval Noop b = nextPc b
eval (Out r) b = b {output = readOp r b : output b} |> nextPc

nextPc :: Assembunny Int -> Assembunny Int
nextPc b = b {pc = succ $ pc b}

-- 192
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> mkBunny
    |> \b ->
      fmap (\startA -> b {regA = startA} |> runTilHalt 10000000 |> fmap (startA,)) [0 .. 1000]
        |> catMaybes
        |> \case
          (x : _) -> fst x
          _ -> -1

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> const 0

solution :: Solution
solution = PureSolution solution1 192 solution2 0
