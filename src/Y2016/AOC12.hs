{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC12 (solution) where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.Functor (($>))
import Text.Parsec (char, many1, newline, oneOf, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, number, parseOrDie, (|>))

data Assembunny a = Assembunny
  { regA :: a,
    regB :: a,
    regC :: a,
    regD :: a,
    pc :: Int,
    program :: [Instruction a]
  }

writeA, writeB, writeC, writeD :: Int -> Assembunny Int -> Assembunny Int
writeA v b = b {regA = v}
writeB v b = b {regB = v}
writeC v b = b {regC = v}
writeD v b = b {regD = v}

type Update a = (Assembunny a -> Assembunny a)

type RegRead a = (Assembunny a -> a)

data Instruction a
  = Cpy (RegRead a) (a -> Update a)
  | Inc (Update a)
  | Dec (Update a)
  | Jnz (RegRead a) (Update a)

instance (Show a) => Show (Instruction a) where
  show (Cpy _ _) = "cpy"
  show (Inc _) = "inc"
  show (Dec _) = "dec"
  show (Jnz _ _) = "jnz"

mkBunny :: [Instruction Int] -> Assembunny Int
mkBunny = Assembunny 0 0 0 0 0

inputParser :: Parser [Instruction Int]
inputParser = many1 (instructionP <* newline)
  where
    instructionP = asum [copyP, incP, decP, jnzP]
    regWriteP =
      (char 'a' $> writeA)
        <|> (char 'b' $> writeB)
        <|> (char 'c' $> writeC)
        <|> (char 'd' $> writeD)
    regReadP =
      (char 'a' $> regA)
        <|> (char 'b' $> regB)
        <|> (char 'c' $> regC)
        <|> (char 'd' $> regD)
    copyP = try $ do
      _ <- string "cpy "
      a <- regReadP <|> (const <$> number)
      _ <- string " "
      Cpy a <$> regWriteP
    incP = do
      _ <- string "inc "
      reg <- oneOf "abcd"
      case reg of
        'a' -> pure $ Inc (regA >>= writeA . succ)
        'b' -> pure $ Inc (regB >>= writeB . succ)
        'c' -> pure $ Inc (regC >>= writeC . succ)
        'd' -> pure $ Inc (regD >>= writeD . succ)
        _ -> error "something went wrong"
    decP = do
      _ <- string "dec "
      reg <- oneOf "abcd"
      case reg of
        'a' -> pure $ Dec (regA >>= writeA . pred)
        'b' -> pure $ Dec (regB >>= writeB . pred)
        'c' -> pure $ Dec (regC >>= writeC . pred)
        'd' -> pure $ Dec (regD >>= writeD . pred)
        _ -> error "something went wrong"
    jnzP = do
      _ <- string "jnz "
      conditionReg <- regReadP <|> (const <$> number)
      _ <- string " "
      offset <- negativeNumber
      pure $ Jnz conditionReg (\b -> b {pc = pc b + offset})

runTilHalt :: Update Int
runTilHalt bunny = case drop (pc bunny) (program bunny) of
  [] -> bunny
  instr : _ -> eval instr bunny |> runTilHalt

eval :: Instruction Int -> Update Int
eval (Cpy rd f) b = let a = rd b in f a b |> nextPc
eval (Inc f) b = f b |> nextPc
eval (Dec f) b = f b |> nextPc
eval (Jnz rd f) b = case rd b of
  0 -> b |> nextPc
  _ -> f b

nextPc :: Update Int
nextPc b = b {pc = succ $ pc b}

-- 318083
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> mkBunny
    |> runTilHalt
    |> regA

-- 9227737
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> mkBunny
    |> writeC 1
    |> runTilHalt
    |> regA

solution :: Solution
solution = PureSolution solution1 318083 solution2 9227737
