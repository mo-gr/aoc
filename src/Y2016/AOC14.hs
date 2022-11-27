{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC14 (solution) where

import AOC (Solution (PureSolution))
import Control.Monad (guard)
import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy.Char8 (pack)
import Util (Input, times)

md5HexString :: String -> String
md5HexString = show . md5 . pack 

keyData, stretchedKeyData :: String -> [String]
keyData seed = snd <$> iterate (\(idx, _hashed) -> (succ idx, md5HexString (seed <> show (succ idx)))) (0 :: Int, md5HexString (seed <> "0"))
stretchedKeyData seed = snd <$> iterate (\(idx, _hashed) -> (succ idx, times 2017 md5HexString (seed <> show (succ idx)))) (0 :: Int, times 2017 md5HexString (seed <> "0"))

hasTriplet :: String -> Maybe Char
hasTriplet [] = Nothing
hasTriplet [_] = Nothing
hasTriplet [_, _] = Nothing
hasTriplet (a : b : c : rst)
  | a == b && a == c = Just a
  | otherwise = hasTriplet $ b : c : rst

hasQuint :: Char -> String -> Bool
hasQuint _k [] = False
hasQuint k (a : b : c : d : e : rst)
  | k == a && a == b && a == c && a == d && a == e = True
  | otherwise = hasQuint k $ b : c : d : e : rst
hasQuint _k _ = False

keys :: [String] -> [(Int, String)]
keys generator = do
  (idx, candidate) <- zip [0 ..] generator
  k <- case hasTriplet candidate of
    Nothing -> []
    Just a -> [a]
  guard $ any (hasQuint k) <$> take 1000 $ drop (succ idx) generator
  [(idx, candidate)]

-- 15035
solution1 :: Input -> Int
solution1 _input = fst . (!! 63) $ keys (keyData seedInput)

-- 19968
solution2 :: Input -> Int
solution2 _input = fst . (!! 63) $ keys (stretchedKeyData seedInput)

seedInput :: String
seedInput = "ihaygndm"

solution :: Solution
solution = PureSolution solution1 15035 solution2 19968
