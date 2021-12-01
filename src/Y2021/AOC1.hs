module Y2021.AOC1 where

import qualified Data.ByteString.Char8 as C
import Data.Either (fromRight)
import Text.Parsec (char, many1, runP)
import Text.Parsec.ByteString (Parser)

inputParser :: Parser String
inputParser = many1 $ char 'a'

solution1 :: C.ByteString -> String
solution1 input = fromRight [] $ runP inputParser () "" input

solution2 :: C.ByteString -> String
solution2 = error "not yet"
