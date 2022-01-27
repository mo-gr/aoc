{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC5 where

import AOC (Solution (PureSolution))
import Control.Monad (guard)
import Crypto.Hash (MD5, hash)
import Data.ByteString.Char8 (pack)
import Data.List (find, isPrefixOf)
import Util (Input)

md5 :: String -> String
md5 inp =
  let hashSum :: MD5
      hashSum = hash . pack $ inp
   in show hashSum

hack :: String -> String
hack seed = do
  idx <- ([0 ..] :: [Int])
  let hashed = md5 (seed <> show idx)
  guard $ "00000" `isPrefixOf` hashed
  pure $ hashed !! 5

hack2 :: String -> String
hack2 seed = assemble $ do
  idx <- ([0 ..] :: [Int])
  let hashed = md5 (seed <> show idx)
  guard $ "00000" `isPrefixOf` hashed
  pure (hashed !! 5, hashed !! 6)

assemble :: [(Char, Char)] -> String
assemble steps = do
  pos <- ['0' .. '7']
  case find ((pos ==) . fst) steps of
    Nothing -> error "something went wrong"
    Just (_, c) -> pure c

-- c6697b55
solution1 :: Input -> String
solution1 _input =
  take 8 $ hack input

-- 8c35d1ab
solution2 :: Input -> String
solution2 _input = hack2 input

example, input :: String
example = "abc"
input = "ffykfhsq"

solution :: Solution
solution = PureSolution solution1 "c6697b55" solution2 "8c35d1ab"
