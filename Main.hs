module Main where

import qualified AOC1
import qualified AOC2
import qualified AOC3
import qualified AOC4
import qualified AOC5
import qualified AOC6
import qualified AOC7
import qualified AOC8
import qualified AOC9
import qualified AOC10
import qualified AOC11
import qualified AOC12
import qualified AOC13
import qualified AOC14
import qualified AOC15
import qualified AOC16
import qualified AOC17
import qualified AOC18
import qualified AOC19
import           System.Environment
import           System.Exit
import           Data.Maybe (fromMaybe)

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
  putStrLn $ "Advent of Code 2020 - Day " ++ (fromMaybe "" (safeHead arg))
  putStrLn "https://adventofcode.com/2020"
  case safeHead arg of
    Nothing    -> putStrLn "Usage: AOC <day>" >>= const exitFailure
    Just "1"   -> format AOC1.solution1 AOC1.solution2
    Just "2"   -> format AOC2.solution1 AOC2.solution2
    Just "3"   -> format AOC3.solution1 AOC3.solution2
    Just "4"   -> format AOC4.solution1 AOC4.solution2
    Just "5"   -> format AOC5.solution1 AOC5.solution2
    Just "6"   -> format AOC6.solution1 AOC6.solution2
    Just "7"   -> format AOC7.solution1 AOC7.solution2
    Just "8"   -> format AOC8.solution1 AOC8.solution2
    Just "9"   -> format AOC9.solution1 AOC9.solution2
    Just "10"   -> format AOC10.solution1 AOC10.solution2
    Just "11"   -> format AOC11.solution1 AOC11.solution2
    Just "12"   -> format AOC12.solution1 AOC12.solution2
    Just "13"   -> format AOC13.solution1 AOC13.solution2
    Just "14"   -> format AOC14.solution1 AOC14.solution2
    Just "15"   -> format AOC15.solution1 AOC15.solution2
    Just "16"   -> format AOC16.solution1 AOC16.solution2
    Just "17"   -> format AOC17.solution1 AOC17.solution2
    Just "18"   -> format AOC18.solution1 AOC18.solution2
    Just "19"   -> format AOC19.solution1 AOC19.solution2
    Just other -> putStrLn $ "No sulution for day " ++ show other
