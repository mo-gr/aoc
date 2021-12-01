module Util where

import AOC
import Control.Exception (bracket)
import System.Directory
import Text.Parsec (digit, many1)
import Text.Parsec.ByteString (Parser)

number :: Parser Int
number = read <$> many1 digit

setupPath :: (AOC a) => a -> IO FilePath
setupPath a = do
  oldDirectory <- getCurrentDirectory
  setCurrentDirectory $ "data" ++ inputDir a
  pure oldDirectory

withPath :: (AOC a) => a -> IO b -> IO b
withPath a op =
  bracket
    (setupPath a)
    setCurrentDirectory
    (const op)
