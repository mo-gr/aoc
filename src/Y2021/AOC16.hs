{-# LANGUAGE NamedFieldPuns #-}

module Y2021.AOC16 where

import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (hexDigit, many1)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

type Bit = Int

data LengthType = Length | Number deriving (Show)

data Packet
  = OperatorPacket
      { version :: Int,
        packetTypeId :: Int,
        lengthType :: LengthType,
        subPackets :: [Packet]
      }
  | LiteralPacket
      { version :: Int,
        packetTypeId :: Int,
        value :: Int
      }
  | PaddingPacket
  deriving (Show)

inputParser :: Parser String
inputParser = many1 hexDigit

hexToBits :: String -> [Bit]
hexToBits s = mconcat $ hexToBit <$> s

hexToBit :: Char -> [Bit]
hexToBit '0' = [0, 0, 0, 0]
hexToBit '1' = [0, 0, 0, 1]
hexToBit '2' = [0, 0, 1, 0]
hexToBit '3' = [0, 0, 1, 1]
hexToBit '4' = [0, 1, 0, 0]
hexToBit '5' = [0, 1, 0, 1]
hexToBit '6' = [0, 1, 1, 0]
hexToBit '7' = [0, 1, 1, 1]
hexToBit '8' = [1, 0, 0, 0]
hexToBit '9' = [1, 0, 0, 1]
hexToBit 'A' = [1, 0, 1, 0]
hexToBit 'B' = [1, 0, 1, 1]
hexToBit 'C' = [1, 1, 0, 0]
hexToBit 'D' = [1, 1, 0, 1]
hexToBit 'E' = [1, 1, 1, 0]
hexToBit 'F' = [1, 1, 1, 1]
hexToBit _ = error "invalid hex"

bitsToNumber :: [Bit] -> Int
bitsToNumber [] = 0
bitsToNumber (1 : bs) = (2 ^ length bs) + bitsToNumber bs
bitsToNumber (_ : bs) = bitsToNumber bs

parseLiteralValue :: [Bit] -> ([Bit], Int)
parseLiteralValue = recur []
  where
    recur nb (1 : bs) = recur (nb ++ take 4 bs) (drop 4 bs)
    recur nb (0 : bs) = (drop 4 bs, bitsToNumber (nb ++ take 4 bs))
    recur _ _ = error "can't parse literal value"

parsePacket :: [Bit] -> ([Bit], Packet)
parsePacket (v1 : v2 : v3 : 1 : 0 : 0 : sub) =
  let (rst, lv) = parseLiteralValue sub
   in ( rst,
        LiteralPacket
          { version = bitsToNumber [v1, v2, v3],
            packetTypeId = 4,
            value = lv
          }
      )
parsePacket (v1 : v2 : v3 : t1 : t2 : t3 : 1 : pack) =
  let (rst, sp) = subPacketsNumber pack
   in ( rst,
        OperatorPacket
          { version = bitsToNumber [v1, v2, v3],
            packetTypeId = bitsToNumber [t1, t2, t3],
            lengthType = Number,
            subPackets = sp
          }
      )
parsePacket (v1 : v2 : v3 : t1 : t2 : t3 : 0 : pack) =
  let (rst, sp) = subPacketsLength pack
   in ( rst,
        OperatorPacket
          { version = bitsToNumber [v1, v2, v3],
            packetTypeId = bitsToNumber [t1, t2, t3],
            lengthType = Length,
            subPackets = sp
          }
      )
parsePacket zeros | all (== 0) zeros = ([], PaddingPacket)
parsePacket bs = error $ "unexpected packet format " ++ show bs

subPacketsLength :: [Bit] -> ([Bit], [Packet])
subPacketsLength bs =
  let numBits = bitsToNumber (take 15 bs)
      subPacketBits = take numBits (drop 15 bs)
      recur :: ([Bit], [Packet]) -> ([Bit], [Packet])
      recur ([], sps) = (drop (15 + numBits) bs, sps)
      recur (rst, ps) =
        let (rst', p) = parsePacket rst
         in recur (rst', p : ps)
   in recur (subPacketBits, [])

subPacketsNumber :: [Bit] -> ([Bit], [Packet])
subPacketsNumber bs =
  let numPacks = bitsToNumber (take 11 bs)
      recur :: Int -> ([Bit], [Packet]) -> ([Bit], [Packet])
      recur 0 sps = sps
      recur n (bs', ps) =
        let (rst, p) = parsePacket bs'
         in recur (n - 1) (rst, p : ps)
   in recur numPacks (drop 11 bs, [])

versionSum :: Packet -> Int
versionSum PaddingPacket = 0
versionSum LiteralPacket {version} = version
versionSum OperatorPacket {version, subPackets} = version + sum (versionSum <$> subPackets)

eval :: Packet -> Int
eval PaddingPacket = 0
eval LiteralPacket {value} = value
eval OperatorPacket {packetTypeId = 0, subPackets} = sum (eval <$> subPackets)
eval OperatorPacket {packetTypeId = 1, subPackets} = product (eval <$> subPackets)
eval OperatorPacket {packetTypeId = 2, subPackets} = minimum (eval <$> subPackets)
eval OperatorPacket {packetTypeId = 3, subPackets} = maximum (eval <$> subPackets)
eval OperatorPacket {packetTypeId = 5, subPackets = [s2, s1]} = if eval s1 > eval s2 then 1 else 0
eval OperatorPacket {packetTypeId = 6, subPackets = [s2, s1]} = if eval s1 < eval s2 then 1 else 0
eval OperatorPacket {packetTypeId = 7, subPackets = [s1, s2]} = if eval s1 == eval s2 then 1 else 0
eval p = error $ "invalid packet: " ++ show p

-- 989
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> hexToBits
    |> parsePacket
    |> snd
    |> versionSum

-- 7936430475134
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> hexToBits
    |> parsePacket
    |> snd
    |> eval

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 989 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 7936430475134 . solution2 =<< input
    ]
