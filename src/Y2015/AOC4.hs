module Y2015.AOC4 where

import AOC (Solution (PureSolution))
import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (isPrefixOf)
import Util (Input)

md5HexString :: String -> String
md5HexString = show . md5 . pack 

solve :: Int -> String -> String -> Int
solve n prefix salt | prefix `isPrefixOf` md5HexString (salt <> show n) = n
solve n prefix salt = solve (n + 1) prefix salt

-- 254575
solution1 :: Input -> Int
solution1 _input = solve 0 "00000" inputData

-- 1038736
solution2 :: Input -> Int
solution2 _input = solve 0 "000000" inputData

testData, inputData :: String
testData = "abcdef"
inputData = "bgvyzdsv"

solution :: Solution
solution = PureSolution solution1 254575 solution2 1038736
