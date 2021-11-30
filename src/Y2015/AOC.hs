module Y2015.AOC where

import AOC
import Test.HUnit (Test (TestLabel, TestList))
import qualified Y2015.AOC1
import qualified Y2015.AOC2
import qualified Y2015.AOC3

data Y2015 = Y2015

instance AOC Y2015 where
  showYear Y2015 = "2015"
  inputDir Y2015 = "/2015"
  verify Y2015 = verify2015
  solution Y2015 D1 = unify (Y2015.AOC1.solution1, Y2015.AOC1.solution2)
  solution Y2015 D2 = unify (Y2015.AOC2.solution1, Y2015.AOC2.solution2)
  solution Y2015 D3 = unify (Y2015.AOC3.solution1, Y2015.AOC3.solution2)
  solution Y2015 _ = error "not yet"

verify2015 :: Test
verify2015 =
  TestList
    [ TestLabel "Day 1" Y2015.AOC1.verify,
      TestLabel "Day 2" Y2015.AOC2.verify,
      TestLabel "Day 3" Y2015.AOC3.verify
    ]
