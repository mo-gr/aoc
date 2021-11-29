module Y2015.AOC where

import           AOC
import qualified Y2015.AOC1
import qualified Y2015.AOC2

data Y2015 = Y2015

instance AOC Y2015 where
  showYear Y2015 = "2015"
  inputDir Y2015 = "/2015"
  solution Y2015 D1  = unify (Y2015.AOC1.solution1, Y2015.AOC1.solution2)
  solution Y2015 D2  = unify (Y2015.AOC2.solution1, Y2015.AOC2.solution2)
  solution Y2015 _   = error "not yet"
