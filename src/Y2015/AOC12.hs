module Y2015.AOC12 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.ByteString.Char8 (unpack)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import qualified Text.JSON as JSON
import Text.Parsec (endBy, letter, many, newline, oneOf)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, parseOrDie, (|>))

extractNumberParser :: Parser [Int]
extractNumberParser = do
  _ <- many (oneOf "[]{}\",:" <|> letter)
  ns <- negativeNumber `endBy` many (oneOf "[]{}\",:" <|> letter)
  _ <- newline
  pure ns

extractNonRedNumbers :: JSON.Result JSON.JSValue -> [Int]
extractNonRedNumbers (JSON.Error err) = error err
extractNonRedNumbers (JSON.Ok json) = extract json
  where
    extract :: JSON.JSValue -> [Int]
    extract (JSON.JSString _) = []
    extract JSON.JSNull = []
    extract (JSON.JSBool _) = []
    extract (JSON.JSRational _ r) = pure . fromEnum $ r
    extract (JSON.JSArray arr) = mconcat $ extract <$> arr
    extract (JSON.JSObject val) =
      let keyVals = JSON.fromJSObject val
       in if any ((== (JSON.JSString $ JSON.toJSString "red")) . snd) keyVals
            then []
            else mconcat $ extract . snd <$> keyVals

parseJson :: Input -> JSON.Result JSON.JSValue
parseJson = JSON.decodeStrict . unpack

-- 111754
solution1 :: Input -> Int
solution1 input =
  parseOrDie extractNumberParser input
    |> sum

-- 65402
solution2 :: Input -> Int
solution2 input =
  parseJson input
    |> extractNonRedNumbers
    |> sum

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 111754 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 65402 . solution2 =<< input
    ]

solution :: Solution
solution = PureSolution solution1 solution2 verify
