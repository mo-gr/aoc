module Y2023.AOC where

import AOC (Day (..), Solution, Year (), mkYear')
import qualified Y2023.AOC1

year :: Year
year = mkYear' "2023" lookUpDay

lookUpDay :: Day -> Solution
lookUpDay D1 = Y2023.AOC1.solution
lookUpDay _ = error "not yet"