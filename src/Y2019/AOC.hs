module Y2019.AOC where

import           AOC
import qualified Y2019.AOC1
import qualified Y2019.AOC10
import qualified Y2019.AOC11
import qualified Y2019.AOC12
import qualified Y2019.AOC13
import qualified Y2019.AOC14
import qualified Y2019.AOC15
import qualified Y2019.AOC16
import qualified Y2019.AOC2
import qualified Y2019.AOC3
import qualified Y2019.AOC4
import qualified Y2019.AOC5
import qualified Y2019.AOC6
import qualified Y2019.AOC7
import qualified Y2019.AOC8
import qualified Y2019.AOC9

data Y2019 = Y2019

instance AOC Y2019 where
  showYear Y2019 = "2019"
  inputDir Y2019 = "/2019"
  solution Y2019 D1  = unify (Y2019.AOC1.solution1, Y2019.AOC1.solution2)
  solution Y2019 D2  = unify (Y2019.AOC2.solution1, Y2019.AOC2.solution2)
  solution Y2019 D3  = unify (Y2019.AOC3.solution1, Y2019.AOC3.solution2)
  solution Y2019 D4  = unify (Y2019.AOC4.solution1, Y2019.AOC4.solution2)
  solution Y2019 D5  = unify (Y2019.AOC5.solution1, Y2019.AOC5.solution2)
  solution Y2019 D6  = unify (Y2019.AOC6.solution1, Y2019.AOC6.solution2)
  solution Y2019 D7  = unify (Y2019.AOC7.solution1, Y2019.AOC7.solution2)
  solution Y2019 D8  = unify (Y2019.AOC8.solution1, Y2019.AOC8.solution2)
  solution Y2019 D9  = unify (Y2019.AOC9.solution1, Y2019.AOC9.solution2)
  solution Y2019 D10 = unify (Y2019.AOC10.solution1, Y2019.AOC10.solution2)
  solution Y2019 D11 = unify (Y2019.AOC11.solution1, Y2019.AOC11.solution2)
  solution Y2019 D12 = unify (Y2019.AOC12.solution1, Y2019.AOC12.solution2)
  solution Y2019 D13 = unify (Y2019.AOC13.solution1, Y2019.AOC13.solution2)
  solution Y2019 D14 = unify (Y2019.AOC14.solution1, Y2019.AOC14.solution2)
  solution Y2019 D15 = unify (Y2019.AOC15.solution1, Y2019.AOC15.solution2)
  solution Y2019 D16 = unify (Y2019.AOC16.solution1, Y2019.AOC16.solution2)
  solution Y2019 _   = error "not yet"
