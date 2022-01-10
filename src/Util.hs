module Util where

import qualified Data.ByteString.Char8 as C
import Text.Parsec (char, digit, many1, runP, (<|>))
import Text.Parsec.ByteString (Parser)

type Input = C.ByteString

number :: Parser Int
number = read <$> many1 digit

negativeNumber :: Parser Int
negativeNumber = do
  (negate <$> (char '-' *> number)) <|> number

parseOrDie :: Parser p -> Input -> p
parseOrDie parser input = case runP parser () "" input of
  Left err -> error $ show err
  Right parsed -> parsed

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)
infixl 8 |>

times :: Int -> (a -> a) -> a -> a
times 0 _ a = a
times n f a = times (n - 1) f (f a)