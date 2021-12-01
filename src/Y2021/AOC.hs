module Y2021.AOC where

import AOC
import qualified Data.ByteString.Char8 as C
import Test.HUnit (Test (TestLabel, TestList))
import qualified Util (withPath)
import qualified Y2021.AOC1

data Y2021 = Y2021

instance AOC Y2021 where
  showYear Y2021 = "2021"
  inputDir Y2021 = "/2021"
  verify Y2021 = verify2021
  solution Y2021 D1 = run Y2021.AOC1.solution1 Y2021.AOC1.solution2 $ loadInput "AOC1"
  solution Y2021 _ = error "not yet"

loadInput :: String -> IO C.ByteString
loadInput d = C.readFile $ d ++ ".input"

run :: (Show b, Show c) => (a -> b) -> (a -> c) -> IO a -> (IO String, IO String)
run f g a = (show . f <$> a, show . g <$> a)

verify2021 :: Test
verify2021 =
  TestList
    [ TestLabel "Day 1" $ Y2021.AOC1.verify (loadInput "AOC1")
    ]

inRepl1 :: Day -> IO String
inRepl1 d = Util.withPath Y2021 $ fst $ solution Y2021 d

inRepl2 :: Day -> IO String
inRepl2 d = Util.withPath Y2021 $ snd $ solution Y2021 d
