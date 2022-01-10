{-# LANGUAGE NamedFieldPuns #-}

module Y2020.AOC8 where

import AOC (Solution (PureSolution))
import Text.Parsec
  ( char,
    count,
    letter,
    many1,
    space,
    (<|>),
  )
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie)

data Instruction
  = NOP {arg :: Int}
  | ACC {arg :: Int}
  | JMP {arg :: Int}
  deriving (Show)

data CPU = CPU
  { accumulator :: Int,
    op :: Int,
    code :: [Instruction]
  }
  deriving (Show)

instructionParser :: Parser Instruction
instructionParser = do
  inst <- count 3 letter <* space
  argument <- (char '+' >> number) <|> (char '-' >> (* (-1)) <$> number)
  return $ case inst of
    "nop" -> NOP argument
    "acc" -> ACC argument
    "jmp" -> JMP argument
    wat -> error $ "unknown instruction: " ++ wat

inputParser :: Parser [Instruction]
inputParser = many1 (instructionParser <* space)

tick :: CPU -> CPU
tick c@CPU {op, code} =
  if op >= length code
    then error ("terminated with: " ++ show (accumulator c))
    else tick' c (code !! op)

tick' :: CPU -> Instruction -> CPU
tick' c@CPU {op} NOP {} = c {op = op + 1}
tick' c@CPU {op} JMP {arg} = c {op = op + arg}
tick' c@CPU {op, accumulator} ACC {arg} = c {op = op + 1, accumulator = accumulator + arg}

makeCPU :: [Instruction] -> CPU
makeCPU code = CPU {accumulator = 0, op = 0, code = code}

runUntilLoop :: [Int] -> CPU -> CPU
runUntilLoop ops c =
  if op c `elem` ops
    then c
    else runUntilLoop (op c : ops) (tick c)

codeVariations :: [Instruction] -> [[Instruction]]
codeVariations code =
  concat $
    ( \i ->
        case code !! i of
          JMP {arg} -> [take i code ++ [NOP arg] ++ drop (i + 1) code]
          NOP {arg} -> [take i code ++ [JMP arg] ++ drop (i + 1) code]
          _ -> [code]
    )
      <$> [0 .. (length code - 1)]

-- 2058
solution1 :: Input -> Int
solution1 input =
  let code = parseOrDie inputParser input
   in accumulator $ runUntilLoop [] (makeCPU code)

-- 1000
solution2 :: Input -> Int
solution2 input =
  let code = parseOrDie inputParser input
   in head (accumulator . runUntilLoop [] <$> (makeCPU <$> codeVariations code))

solution :: Solution
solution = PureSolution solution1 2058 solution2 1000