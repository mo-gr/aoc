module Y2015.AOC where

import AOC
import Test.HUnit (Test (TestLabel, TestList))
import qualified Y2015.AOC1
import qualified Y2015.AOC2
import qualified Y2015.AOC3

solution :: Day -> (IO String, IO String)
solution D1 = unify (Y2015.AOC1.solution1, Y2015.AOC1.solution2)
solution D2 = unify (Y2015.AOC2.solution1, Y2015.AOC2.solution2)
solution D3 = unify (Y2015.AOC3.solution1, Y2015.AOC3.solution2)
solution _ = error "not yet"

verify :: Test
verify =
  TestList
    [ TestLabel "Day 1" Y2015.AOC1.verify,
      TestLabel "Day 2" Y2015.AOC2.verify,
      TestLabel "Day 3" Y2015.AOC3.verify
    ]
