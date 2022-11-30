module Y2018.AOC where

import AOC (Day (..), Solution, Year (), mkYear')
import qualified Y2018.AOC1
import qualified Y2018.AOC2

lookUpDay :: Day -> Solution
lookUpDay D1 = Y2018.AOC1.solution
lookUpDay D2 = Y2018.AOC2.solution
lookUpDay _ = error "not started yet"

year :: Year
year = mkYear' "2018" lookUpDay
