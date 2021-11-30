module Y2021.AOC where

import Test.HUnit (Test(TestList))
import AOC

  
data Y2021 = Y2021

instance AOC Y2021 where
  showYear Y2021 = "2021"
  inputDir Y2021 = "/2021"
  verify Y2021 = verify2021
  solution Y2021 _ = error "not yet"
  
verify2021 :: Test
verify2021 = TestList [
  ]

