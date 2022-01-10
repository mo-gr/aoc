module Y2015.AOC7 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Bits (complement, shift, (.&.), (.|.))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Word (Word16)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (lower, many1, newline, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Signal = Word16

type Wire = String

type World = M.Map Wire Part

data Part
  = Provider Signal Wire
  | Pass Wire Wire
  | And Wire Wire Wire
  | And1 Wire Wire
  | Or Wire Wire Wire
  | Not Wire Wire
  | LShift Int Wire Wire
  | RShift Int Wire Wire
  deriving (Show, Eq)

intToSignal :: Int -> Signal
intToSignal = fromInteger . toInteger

signalToInt :: Signal -> Int
signalToInt = fromInteger . toInteger

wireParser :: Parser Wire
wireParser = many1 lower

outputParser :: Parser Wire
outputParser = string " -> " *> wireParser

partParser :: Parser Part
partParser = do
  part <-
    (string "NOT " *> wireParser >>= (\w -> outputParser <&> Not w))
      <|> try (string "1 AND " *> wireParser >>= \w -> outputParser <&> And1 w)
      <|> try (wireParser >>= \w1 -> string " AND " *> wireParser >>= \w2 -> outputParser <&> And w1 w2)
      <|> try (wireParser >>= \w1 -> string " OR " *> wireParser >>= \w2 -> outputParser <&> Or w1 w2)
      <|> try (wireParser >>= \w -> string " RSHIFT " *> number >>= \i -> outputParser <&> RShift i w)
      <|> try (wireParser >>= \w -> string " LSHIFT " *> number >>= \i -> outputParser <&> LShift i w)
      <|> try (wireParser >>= \w -> outputParser <&> Pass w)
      <|> (number >>= \n -> outputParser >>= \w -> pure $ Provider (intToSignal n) w)
  _ <- newline
  pure part

inputParser :: Parser World
inputParser = do
  parts <- many1 partParser
  pure $ M.fromList $ zip (output <$> parts) parts

output :: Part -> Wire
output (RShift _n _i w) = w
output (LShift _n _i w) = w
output (Not _i w) = w
output (Or _a _b w) = w
output (And _a _b w) = w
output (And1 _b w) = w
output (Provider _s w) = w
output (Pass _w w) = w

prePass :: World -> World
prePass w = foldl f w (M.keys w)
  where
    f :: World -> Wire -> World
    f acc k = case eval 10 acc k of
      Nothing -> acc
      Just s -> M.insert k (Provider s k) acc

tillStable :: Eq a => (a -> a) -> a -> a
tillStable f a = let a' = f a in if a == a' then a else tillStable f a'

eval :: Int -> World -> Wire -> Maybe Signal
eval 0 _ _ = Nothing
eval n w wire =
  let n' = n - 1
   in case M.lookup wire w of
        Nothing -> error $ "unknown wire: " ++ wire ++ "\n" ++ show (length w)
        Just (Provider s _) -> Just s
        Just (And a b _) -> do
          a' <- eval n' w a
          b' <- eval n' (M.insert a (Provider a' a) w) b
          pure $ a' .&. b'
        Just (And1 a _) -> eval n' w a <&> (1 .&.)
        Just (Or a b _) -> do
          a' <- eval n' w a
          b' <- eval n' (M.insert a (Provider a' a) w) b
          pure $ a' .|. b'
        Just (Not i _) -> complement <$> eval n' w i
        Just (RShift s i _) -> eval n' w i <&> flip shift (negate s)
        Just (LShift s i _) -> eval n' w i <&> flip shift s
        Just (Pass w' _) -> eval n' w w'

-- 16076
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> tillStable prePass
    |> flip (eval 10) "a"
    |> fromJust
    |> signalToInt

-- 2797
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> M.insert "b" (Provider 16076 "b")
    |> tillStable prePass
    |> flip (eval 10) "a"
    |> fromJust
    |> signalToInt

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 16076 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 2797 . solution2 =<< input
    ]

solution :: Solution
solution = PureSolution solution1 16076 solution2 2797
