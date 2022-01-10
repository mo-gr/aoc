module Y2015.AOC4 where

import AOC (Solution (PureSolution))
import Crypto.Hash (MD5, hash)
import Data.ByteString.Char8 (pack)
import Data.List (isPrefixOf)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Util (Input)

md5 :: String -> String
md5 input =
  let hashSum :: MD5
      hashSum = hash . pack $ input
   in show hashSum

solve :: Int -> String -> String -> Int
solve n prefix salt | prefix `isPrefixOf` md5 (salt <> show n) = n
solve n prefix salt = solve (n + 1) prefix salt

-- 254575
solution1 :: Input -> Int
solution1 _input = solve 0 "00000" inputData

-- 1038736
solution2 :: Input -> Int
solution2 _input = solve 0 "000000" inputData

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 254575 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 1038736 . solution2 =<< input
    ]

testData, inputData :: String
testData = "abcdef"
inputData = "bgvyzdsv"

solution :: Solution
solution = PureSolution solution1 254575 solution2 1038736
