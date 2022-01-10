{-# LANGUAGE NamedFieldPuns #-}

module Y2020.AOC14 where

import AOC (Solution (PureSolution))
import Data.Bits (clearBit, setBit)
import qualified Data.Map.Strict as M
import Text.Parsec
  ( char,
    count,
    endOfLine,
    many1,
    string,
    try,
    (<|>),
  )
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Command
  = UpdateMask {newMask :: Mask}
  | Write {location :: Address, value :: Value}
  deriving (Show)

maskParser :: Parser Command
maskParser = do
  _ <- try $ string "mask = "
  bs <- count 36 (char 'X' <|> char '1' <|> char '0')
  _ <- endOfLine
  let set = fst <$> filter (\(_idx, b) -> b == '1') (zip [0 ..] (reverse bs))
  let clear = fst <$> filter (\(_idx, b) -> b == '0') (zip [0 ..] (reverse bs))
  let fls = fst <$> filter (\(_idx, b) -> b == 'X') (zip [0 ..] (reverse bs))
  return $ UpdateMask $ Mask {setBits = set, unsetBits = clear, floatBits = fls}

writeParser :: Parser Command
writeParser = do
  loc <- string "mem[" *> number <* string "] = "
  val <- number <* endOfLine
  return $ Write loc val

commandParser :: Parser Command
commandParser = maskParser <|> writeParser

inputParser :: Parser [Command]
inputParser = many1 commandParser

data Mask = Mask {setBits :: [BitIndex], unsetBits :: [BitIndex], floatBits :: [BitIndex]}
  deriving (Show)

data Memory = Memory {values :: M.Map Address Value, mask :: Mask}
  deriving (Show)

mkMemory :: Memory
mkMemory = Memory M.empty (Mask [] [] [])

type Value = Int

type Address = Int

type BitIndex = Int

applyMask :: Mask -> Value -> Value
applyMask Mask {setBits, unsetBits} i = foldl clearBit (foldl setBit i setBits) unsetBits

applyMaskV2 :: Mask -> Address -> [Address]
applyMaskV2 Mask {setBits, floatBits} addr =
  let addr' = foldl setBit addr setBits
      go :: Address -> [BitIndex] -> [Address]
      go v [] = [v]
      go v (b : bs) = concat $ flip go bs <$> [setBit v b, clearBit v b]
   in go addr' floatBits

tick :: Memory -> Command -> Memory
tick m UpdateMask {newMask} = m {mask = newMask}
tick m Write {location, value} = m {values = M.insert location (applyMask (mask m) value) (values m)}

tickV2 :: Memory -> Command -> Memory
tickV2 m UpdateMask {newMask} = m {mask = newMask}
tickV2 m@Memory {mask, values} Write {location, value} =
  let addrs = applyMaskV2 mask location
   in m {values = foldl (\vMap loc -> M.insert loc value vMap) values addrs}

memorySum :: Memory -> Value
memorySum Memory {values} = M.foldl (+) 0 values

-- 8332632930672
solution1 :: Input -> Int
solution1 input = parseOrDie inputParser input |> foldl tick mkMemory |> memorySum

-- 4753238784664
solution2 :: Input -> Int
solution2 input = parseOrDie inputParser input |> foldl tickV2 mkMemory |> memorySum

solution :: Solution
solution = PureSolution solution1 8332632930672 solution2 4753238784664
