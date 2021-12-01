module Util where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as C
import System.Directory
import Text.Parsec (digit, many1)
import Text.Parsec.ByteString (Parser)

type Input = C.ByteString

number :: Parser Int
number = read <$> many1 digit

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

setupPath :: String -> IO FilePath
setupPath dir = do
  oldDirectory <- getCurrentDirectory
  setCurrentDirectory $ "data/" ++ dir
  pure oldDirectory

withPath :: String -> IO b -> IO b
withPath dir op =
  bracket
    (setupPath dir)
    setCurrentDirectory
    (const op)

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

unify :: (Show a, Show b) => (IO a, IO b) -> (IO String, IO String)
unify (a, b) = (show <$> a, show <$> b)