module Util where

import           Text.Parsec            (digit, many1)
import           Text.Parsec.ByteString (Parser)

number :: Parser Int
number = read <$> many1 digit
