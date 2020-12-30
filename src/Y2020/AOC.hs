module Y2020.AOC where

import           AOC
import qualified Y2020.AOC1
import qualified Y2020.AOC10
import qualified Y2020.AOC11
import qualified Y2020.AOC12
import qualified Y2020.AOC13
import qualified Y2020.AOC14
import qualified Y2020.AOC15
import qualified Y2020.AOC16
import qualified Y2020.AOC17
import qualified Y2020.AOC18
import qualified Y2020.AOC19
import qualified Y2020.AOC2
import qualified Y2020.AOC20
import qualified Y2020.AOC21
import qualified Y2020.AOC22
import qualified Y2020.AOC23
import qualified Y2020.AOC24
import qualified Y2020.AOC25
import qualified Y2020.AOC3
import qualified Y2020.AOC4
import qualified Y2020.AOC5
import qualified Y2020.AOC6
import qualified Y2020.AOC7
import qualified Y2020.AOC8
import qualified Y2020.AOC9

data Y2020 = Y2020

instance AOC Y2020 where
  showYear Y2020 = "2020"
  inputDir Y2020 = "/2020"
  solution Y2020 D1  = unify (Y2020.AOC1.solution1, Y2020.AOC1.solution2)
  solution Y2020 D2  = unify (Y2020.AOC2.solution1, Y2020.AOC2.solution2)
  solution Y2020 D3  = unify (Y2020.AOC3.solution1, Y2020.AOC3.solution2)
  solution Y2020 D4  = unify (Y2020.AOC4.solution1, Y2020.AOC4.solution2)
  solution Y2020 D5  = unify (Y2020.AOC5.solution1, Y2020.AOC5.solution2)
  solution Y2020 D6  = unify (Y2020.AOC6.solution1, Y2020.AOC6.solution2)
  solution Y2020 D7  = unify (Y2020.AOC7.solution1, Y2020.AOC7.solution2)
  solution Y2020 D8  = unify (Y2020.AOC8.solution1, Y2020.AOC8.solution2)
  solution Y2020 D9  = unify (Y2020.AOC9.solution1, Y2020.AOC9.solution2)
  solution Y2020 D10 = unify (Y2020.AOC10.solution1, Y2020.AOC10.solution2)
  solution Y2020 D11 = unify (Y2020.AOC11.solution1, Y2020.AOC11.solution2)
  solution Y2020 D12 = unify (Y2020.AOC12.solution1, Y2020.AOC12.solution2)
  solution Y2020 D13 = unify (Y2020.AOC13.solution1, Y2020.AOC13.solution2)
  solution Y2020 D14 = unify (Y2020.AOC14.solution1, Y2020.AOC14.solution2)
  solution Y2020 D15 = unify (Y2020.AOC15.solution1, Y2020.AOC15.solution2)
  solution Y2020 D16 = unify (Y2020.AOC16.solution1, Y2020.AOC16.solution2)
  solution Y2020 D17 = unify (Y2020.AOC17.solution1, Y2020.AOC17.solution2)
  solution Y2020 D18 = unify (Y2020.AOC18.solution1, Y2020.AOC18.solution2)
  solution Y2020 D19 = unify (Y2020.AOC19.solution1, Y2020.AOC19.solution2)
  solution Y2020 D20 = unify (Y2020.AOC20.solution1, Y2020.AOC20.solution2)
  solution Y2020 D21 = unify (Y2020.AOC21.solution1, Y2020.AOC21.solution2)
  solution Y2020 D22 = unify (Y2020.AOC22.solution1, Y2020.AOC22.solution2)
  solution Y2020 D23 = unify (Y2020.AOC23.solution1, Y2020.AOC23.solution2)
  solution Y2020 D24 = unify (Y2020.AOC24.solution1, Y2020.AOC24.solution2)
  solution Y2020 D25 = unify (Y2020.AOC25.solution1, Y2020.AOC25.solution2)
