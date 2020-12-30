module Main where

import           AOC
import           Data.Maybe         (fromMaybe)
import           Paths_AOC
import           System.Directory
import           System.Environment
import           System.Exit
import           Y2020.AOC          (Y2020 (Y2020))

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
  let year = Y2020
  putStrLn $ "Advent of Code " ++ showYear year ++ " - Day " ++ fromMaybe "" (safeHead arg)
  putStrLn $ "https://adventofcode.com/" ++ showYear year
  dataDir <- getDataDir
  setCurrentDirectory $ dataDir ++ inputDir year
  case safeHead arg >>= day of
    Nothing -> putStrLn "Usage: AOC <day>" >>= const exitFailure
    Just d  -> uncurry format (solution year d)
