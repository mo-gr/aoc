{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC23 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Set as S
import Safe (atMay)
import Text.Parsec (char, many1, newline, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, parseOrDie, (|>))

data CPU a = CPU
  { regA :: a,
    regB :: a,
    regC :: a,
    regD :: a,
    regE :: a,
    regF :: a,
    regG :: a,
    regH :: a,
    pc :: Int
  }
  deriving (Show, Eq, Ord)

data Operand a
  = Value a
  | RegA
  | RegB
  | RegC
  | RegD
  | RegE
  | RegF
  | RegG
  | RegH
  deriving (Show, Eq)

data Op a
  = Set (Operand a) (Operand a)
  | Sub (Operand a) (Operand a)
  | Mul (Operand a) (Operand a)
  | Jnz (Operand a) (Operand a)
  deriving (Show, Eq)

inputParser :: Parser [Op Int]
inputParser =
  many1 $
    do
      try (Set <$> (string "set " *> operandP) <*> (char ' ' *> operandP <* newline))
      <|> (Sub <$> (string "sub " *> operandP) <*> (char ' ' *> operandP <* newline))
      <|> (Mul <$> (string "mul " *> operandP) <*> (char ' ' *> operandP <* newline))
      <|> (Jnz <$> (string "jnz " *> operandP) <*> (char ' ' *> operandP <* newline))

operandP :: Parser (Operand Int)
operandP =
  (Value <$> negativeNumber)
    <|> (char 'a' $> RegA)
    <|> (char 'b' $> RegB)
    <|> (char 'c' $> RegC)
    <|> (char 'd' $> RegD)
    <|> (char 'e' $> RegE)
    <|> (char 'f' $> RegF)
    <|> (char 'g' $> RegG)
    <|> (char 'h' $> RegH)

get :: CPU a -> Operand a -> a
get _ (Value v) = v
get c RegA = regA c
get c RegB = regB c
get c RegC = regC c
get c RegD = regD c
get c RegE = regE c
get c RegF = regF c
get c RegG = regG c
get c RegH = regH c

put :: CPU a -> a -> Operand a -> CPU a
put _ _ (Value _) = error "writing to constant"
put c v RegA = c {regA = v}
put c v RegB = c {regB = v}
put c v RegC = c {regC = v}
put c v RegD = c {regD = v}
put c v RegE = c {regE = v}
put c v RegF = c {regF = v}
put c v RegG = c {regG = v}
put c v RegH = c {regH = v}

nextPc :: CPU a -> CPU a
nextPc c = c {pc = succ (pc c)}

mkCpu :: CPU Int
mkCpu = CPU 0 0 0 0 0 0 0 0 0

eval :: CPU Int -> Op Int -> CPU Int
eval c (Set to from) = put c (get c from) to |> nextPc
eval c (Sub to from) = put c (get c to - get c from) to |> nextPc
eval c (Mul to from) = put c (get c from * get c to) to |> nextPc
eval c (Jnz x y) =
  if get c x /= 0
    then c {pc = pc c + get c y}
    else c |> nextPc

loopDetected :: S.Set (CPU Int) -> CPU Int -> [Op Int] -> Bool
loopDetected prevStates cpu program = case atMay program (pc cpu) of
  Nothing -> False
  Just op -> S.member (eval cpu op) prevStates

debugMode :: Int -> S.Set (CPU Int) -> CPU Int -> [Op Int] -> Int
debugMode mulCount prev cpu prog | loopDetected prev cpu prog = mulCount
debugMode mulCount prev cpu prog = case atMay prog (pc cpu) of
  Nothing -> mulCount
  Just op@(Mul _ _) -> debugMode (succ mulCount) (S.insert (eval cpu op) prev) (eval cpu op) prog
  Just op -> debugMode mulCount (S.insert (eval cpu op) prev) (eval cpu op) prog

run :: CPU Int -> [Op Int] -> Int
run cpu prog = case atMay prog (pc cpu) of
  Nothing -> regH cpu
  Just op -> run (eval cpu op) prog

-- 5929
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> debugMode 0 S.empty mkCpu

optimize :: [Op Int] -> [Op Int]
optimize = id

isPrime :: Int -> Bool
isPrime k = (k > 1) && null [x | x <- [2 .. k - 1], k `mod` x == 0]

-- 907
solution2 :: Input -> Int
solution2 _input = [107900, (107900 + 17) .. 124900] |> filter (not . isPrime) |> length

--solution2 input =
--  parseOrDie inputParser input
--    |> optimize
--    |> run (mkCpu {regA = 1})

solution :: Solution
solution = PureSolution solution1 5929 solution2 907

{-
After long hard staring at the code, it seems to count all numbers that can't be
evenly divided by two integers. Aka, numbers that are not prime.

# Annotated Assembly
init:
	set b 79
	set c b
	jnz a 2
	jnz 1 5
	mul b 100
	sub b -100000
	set c b
	sub c -17000 # after init: b=107900 c=124900
loop:
	set f 1 # f = 1
	set d 2 # d = 2
labelB:
	set e 2 # e = 2
labelA:
	set g d #
	mul g e #
	sub g b #
	jnz g 2 # if (e * d) == b
	set f 0 #   then f = 0
	sub e -1# e++
	set g e #
	sub g b #
	jnz g -8# if e == b then
	sub d -1#   then d++ else goto labelA
	set g d #
	sub g b #
	jnz g -13#if d != b then goto labelB
	jnz f 2 # if f == 0
	sub h -1 #  then h++
	set g b #
	sub g c #
	jnz g 2 # if b == c
	jnz 1 3 #   then RETURN
	sub b -17#b+=17
	jnz 1 -23#goto loop

# Roughly translates to this pseudo code:
b = 107900
c = 124900
d = 2
e = 2
h = 0
do { # 1000 loops
  if ((e * d) == b) f = true;
  e++
  if e == b then continue
  d++
  if d == b then {
  e = 2
  continue
}
if f then h++
b += 17
f = false
e = 2
d = 2
while (b != c)

# Roughly translates to:
length $ do
  n <- [107900, 107900 + 17, 124900]
  x <- [2..n]
  y <- [2..n]
  if x * y == n then pure [n]
                else pure []

Which can be calculated much more efficiently via
[107900, (107900 + 17) .. 124899] |> filter (not . isPrime) |> length
-}
