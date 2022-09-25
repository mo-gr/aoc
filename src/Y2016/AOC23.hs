{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC23 where

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

instance Show a => Show (Assembunny a) where
  show b = show (pc b) <> ":" <> show (program b)

writeA, writeB, writeC, writeD, writePc :: Int -> Assembunny Int -> Assembunny Int
writeA v b = b {regA = v}
writeB v b = b {regB = v}
writeC v b = b {regC = v}
writeD v b = b {regD = v}
writePc v b = b {pc = v}

type Update a = (a -> Assembunny a -> Assembunny a)

type RegRead a = (Assembunny a -> a)

data Instruction a
  = Cpy (RegRead a) (RegRead a) (Update a)
  | Inc (RegRead a) (Update a)
  | Dec (RegRead a) (Update a)
  | Jnz (RegRead a) (RegRead a) (Update a)
  | Tgl (RegRead a) (Update a)
  | MulAdd (RegRead a) (RegRead a) (Update a)
  | Noop

instance (Show a) => Show (Instruction a) where
  show Cpy {} = "cpy"
  show (Inc _ _) = "inc"
  show (Dec _ _) = "dec"
  show Jnz {} = "jnz"
  show (Tgl _ _) = "tgl"
  show MulAdd {} = "mad"
  show Noop = "nop"

mkBunny :: [Instruction Int] -> Assembunny Int
mkBunny = Assembunny 0 0 0 0 0

inputParser :: Parser [Instruction Int]
inputParser = many1 (instructionP <* newline)
  where
    instructionP = asum [copyP, incP, decP, jnzP, tglP]
    regReadP =
      (char 'a' $> regA)
        <|> (char 'b' $> regB)
        <|> (char 'c' $> regC)
        <|> (char 'd' $> regD)
    copyP = try $ do
      _ <- string "cpy "
      a <- regReadP <|> (const <$> negativeNumber)
      _ <- string " "
      reg <- oneOf "abcd"
      case reg of
        'a' -> pure $ Cpy a regA writeA
        'b' -> pure $ Cpy a regB writeB
        'c' -> pure $ Cpy a regC writeC
        'd' -> pure $ Cpy a regD writeD
        _ -> error "something went wrong"
    incP = do
      _ <- string "inc "
      reg <- oneOf "abcd"
      case reg of
        'a' -> pure $ Inc regA writeA
        'b' -> pure $ Inc regB writeB
        'c' -> pure $ Inc regC writeC
        'd' -> pure $ Inc regD writeD
        _ -> error "something went wrong"
    decP = do
      _ <- string "dec "
      reg <- oneOf "abcd"
      case reg of
        'a' -> pure $ Dec regA writeA
        'b' -> pure $ Dec regB writeB
        'c' -> pure $ Dec regC writeC
        'd' -> pure $ Dec regD writeD
        _ -> error "something went wrong"
    jnzP = do
      _ <- string "jnz "
      conditionReg <- regReadP <|> (const <$> number)
      _ <- string " "
      (negativeNumber >>= \offset -> pure (Jnz conditionReg (const offset) (\_ b -> b)))
        <|> do
          reg <- oneOf "abcd"
          case reg of
            'a' -> pure $ Jnz conditionReg regA writeA
            'b' -> pure $ Jnz conditionReg regB writeB
            'c' -> pure $ Jnz conditionReg regC writeC
            'd' -> pure $ Jnz conditionReg regD writeD
            _ -> error "something went wrong"
    tglP = do
      _ <- string "tgl "
      reg <- oneOf "abcd"
      case reg of
        'a' -> pure $ Tgl regA writeA
        'b' -> pure $ Tgl regB writeB
        'c' -> pure $ Tgl regC writeC
        'd' -> pure $ Tgl regD writeD
        _ -> error "something went wrong"

toggle :: Update Int
toggle target ab = case newInst of
  Nothing -> ab
  Just new -> ab {program = take targetIndex oldProgram <> [new] <> drop (succ targetIndex) oldProgram}
  where
    targetIndex = target + pc ab
    targetInst = safeIndex targetIndex (program ab)
    oldProgram = program ab
    newInst = case targetInst of
      Nothing -> Nothing
      Just (Inc r f) -> Just (Dec r f)
      Just (Cpy r rr _) -> Just (Jnz r rr writePc)
      Just (Jnz r rr t) -> Just (Cpy r rr t)
      Just (Dec r f) -> Just (Inc r f)
      Just (Tgl r t) -> Just (Inc r t)
      Just MulAdd {} -> error "invalid optimisation"
      Just Noop -> error "invalid optimisation"

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
eval (Cpy rd _ f) b = let a = rd b in f a b |> nextPc
eval (Inc r f) b = r b |> succ |> flip f b |> nextPc
eval (Dec r f) b = r b |> pred |> flip f b |> nextPc
eval (Jnz rd j _) b = case rd b of
  0 -> b |> nextPc
  _ -> writePc (pc b + j b) b
eval (Tgl r _) b = toggle (r b) b |> nextPc
eval (MulAdd r rr t) b = (regA b + (r b * rr b)) |> flip t b |> writeD 0 |> writeC 0 |> nextPc
eval Noop b = b |> nextPc

nextPc :: Assembunny Int -> Assembunny Int
nextPc b = b {pc = succ $ pc b}

-- nasty hack to optimize the following asm to regB + regD +> regA (reg c and d to 0)
-- cpy b c inc a dec c jnz c -2 dec d jnz d -5
optimize :: [Instruction Int] -> [Instruction Int]
optimize [] = []
optimize (Cpy {} : Inc {} : Dec {} : Jnz {} : Dec {} : Jnz {} : rest) = MulAdd regB regD writeA : replicate 5 Noop <> optimize rest
optimize (inst : rest) = inst : optimize rest

-- 14346
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> optimize
    |> mkBunny
    |> writeA 7
    |> runTilHalt 100000000
    |> regA

-- 479010906
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> optimize
    |> mkBunny
    |> writeA 12
    |> runTilHalt 100000000
    |> regA

solution :: Solution
solution = PureSolution solution1 14346 solution2 479010906
