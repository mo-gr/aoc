module Main where

import qualified Y2020.AOC1
import qualified Y2020.AOC2
import qualified Y2020.AOC3
import qualified Y2020.AOC4
import qualified Y2020.AOC5
import qualified Y2020.AOC6
import qualified Y2020.AOC7
import qualified Y2020.AOC8
import qualified Y2020.AOC9
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
import qualified Y2020.AOC20
import qualified Y2020.AOC21
import qualified Y2020.AOC22
import qualified Y2020.AOC23
import qualified Y2020.AOC24
import qualified Y2020.AOC25
import           System.Environment
import           System.Directory
import           System.Exit
import           Data.Maybe (fromMaybe)
import           Paths_AOC

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (a : _) = Just a

format :: (Show a, Show b) => IO a -> IO b -> IO ()
format s1 s2 = do
  putStrLn "Solution 1:"
  s1 >>= print
  putStrLn "Solution 2:"
  s2 >>= print

main :: IO ()
main = do
  arg <- getArgs
  putStrLn $ "Advent of Code 2020 - Day " ++ fromMaybe "" (safeHead arg)
  putStrLn "https://adventofcode.com/2020"
  dataDir <- getDataDir
  setCurrentDirectory $ dataDir ++ "/2020"
  case safeHead arg of
    Nothing    -> putStrLn "Usage: AOC <day>" >>= const exitFailure
    Just "1"   -> format Y2020.AOC1.solution1 Y2020.AOC1.solution2
    Just "2"   -> format Y2020.AOC2.solution1 Y2020.AOC2.solution2
    Just "3"   -> format Y2020.AOC3.solution1 Y2020.AOC3.solution2
    Just "4"   -> format Y2020.AOC4.solution1 Y2020.AOC4.solution2
    Just "5"   -> format Y2020.AOC5.solution1 Y2020.AOC5.solution2
    Just "6"   -> format Y2020.AOC6.solution1 Y2020.AOC6.solution2
    Just "7"   -> format Y2020.AOC7.solution1 Y2020.AOC7.solution2
    Just "8"   -> format Y2020.AOC8.solution1 Y2020.AOC8.solution2
    Just "9"   -> format Y2020.AOC9.solution1 Y2020.AOC9.solution2
    Just "10"   -> format Y2020.AOC10.solution1 Y2020.AOC10.solution2
    Just "11"   -> format Y2020.AOC11.solution1 Y2020.AOC11.solution2
    Just "12"   -> format Y2020.AOC12.solution1 Y2020.AOC12.solution2
    Just "13"   -> format Y2020.AOC13.solution1 Y2020.AOC13.solution2
    Just "14"   -> format Y2020.AOC14.solution1 Y2020.AOC14.solution2
    Just "15"   -> format Y2020.AOC15.solution1 Y2020.AOC15.solution2
    Just "16"   -> format Y2020.AOC16.solution1 Y2020.AOC16.solution2
    Just "17"   -> format Y2020.AOC17.solution1 Y2020.AOC17.solution2
    Just "18"   -> format Y2020.AOC18.solution1 Y2020.AOC18.solution2
    Just "19"   -> format Y2020.AOC19.solution1 Y2020.AOC19.solution2
    Just "20"   -> format Y2020.AOC20.solution1 Y2020.AOC20.solution2
    Just "21"   -> format Y2020.AOC21.solution1 Y2020.AOC21.solution2
    Just "22"   -> format Y2020.AOC22.solution1 Y2020.AOC22.solution2
    Just "23"   -> format Y2020.AOC23.solution1 Y2020.AOC23.solution2
    Just "24"   -> format Y2020.AOC24.solution1 Y2020.AOC24.solution2
    Just "25"   -> format Y2020.AOC25.solution1 Y2020.AOC25.solution2
    Just other -> putStrLn $ "No sulution for day " ++ show other
