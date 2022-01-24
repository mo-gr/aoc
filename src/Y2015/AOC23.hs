module Y2015.AOC23 where

import AOC (Solution (PureSolution))
import Data.Functor (($>))
import Text.Parsec (many1, newline, string, try, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, explicitNumber, parseOrDie, (|>))

data Machine = Machine
  { regA :: Word,
    regB :: Word,
    pc :: Int,
    program :: [Instruction]
  }

instance Show Machine where
  show m = show (regA m) <> "/" <> show (regB m) <> "@" <> show (pc m)

type UpdateReg = Machine -> Machine

data Instruction
  = Hlf {effect :: UpdateReg}
  | Tpl {effect :: UpdateReg}
  | Inc {effect :: UpdateReg}
  | Jmp {effect :: UpdateReg}
  | Jie {effect :: UpdateReg}
  | Jio {effect :: UpdateReg}

mkMachine :: [Instruction] -> Machine
mkMachine = Machine 0 0 0

inputParser :: Parser [Instruction]
inputParser =
  many1 $
    do
      ( try (string "hlf a") $> Hlf (\m -> m {regA = regA m `div` 2, pc = succ $ pc m})
          <|> try (string "hlf b") $> Hlf (\m -> m {regB = regB m `div` 2, pc = succ $ pc m})
          <|> try (string "tpl a") $> Tpl (\m -> m {regA = regA m * 3, pc = succ $ pc m})
          <|> try (string "tpl b") $> Tpl (\m -> m {regB = regB m * 3, pc = succ $ pc m})
          <|> try (string "inc a") $> Inc (\m -> m {regA = regA m + 1, pc = succ $ pc m})
          <|> try (string "inc b") $> Inc (\m -> m {regB = regB m + 1, pc = succ $ pc m})
          <|> try (string "jmp " *> explicitNumber >>= \offset -> pure $ Jmp (\m -> m {pc = pc m + offset}))
          <|> try (string "jie a, " *> explicitNumber >>= \offset -> pure $ Jie (\m -> if even (regA m) then m {pc = pc m + offset} else m {pc = succ $ pc m}))
          <|> try (string "jie b, " *> explicitNumber >>= \offset -> pure $ Jie (\m -> if even (regB m) then m {pc = pc m + offset} else m {pc = succ $ pc m}))
          <|> try (string "jio a, " *> explicitNumber >>= \offset -> pure $ Jio (\m -> if 1 == regA m then m {pc = pc m + offset} else m {pc = succ $ pc m}))
          <|> try (string "jio b, " *> explicitNumber >>= \offset -> pure $ Jio (\m -> if 1 == regB m then m {pc = pc m + offset} else m {pc = succ $ pc m}))
        )
        <* newline

safeIndex :: Int -> [a] -> Maybe a
safeIndex a as | a < 0 = Nothing
 | a >= length as = Nothing
 | otherwise = Just $ as !! a

runTilOoB :: Machine -> Word
runTilOoB m = case safeIndex (pc m) (program m) of
  Nothing -> regB m
  Just instruction -> runTilOoB $ effect instruction m

-- 184
solution1 :: Input -> Word
solution1 input =
  parseOrDie inputParser input
    |> mkMachine
    |> runTilOoB

-- 231
solution2 :: Input -> Word
solution2 input =
  parseOrDie inputParser input
    |> mkMachine
    |> \m -> m {regA = 1}
    |> runTilOoB

solution :: Solution
solution = PureSolution solution1 184 solution2 231
