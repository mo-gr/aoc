{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           AOC
import           Data.Maybe         (fromMaybe)
import           Paths_AOC
import           System.Directory
import           System.Environment
import           System.Exit
import           Y2020.AOC          (Y2020 (Y2020))
import           Y2019.AOC          (Y2019 (Y2019))

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (a : _) = Just a

format :: (Show a, Show b) => IO a -> IO b -> IO ()
format s1 s2 = do
  putStrLn "Solution 1:"
  s1 >>= print
  putStrLn "Solution 2:"
  s2 >>= print

data Year = Y20 | Y19
instance AOC Year where
  showYear Y20 = showYear Y2020
  showYear Y19 = showYear Y2019
  inputDir Y20 = inputDir Y2020
  inputDir Y19 = inputDir Y2019
  solution Y20 = solution Y2020
  solution Y19 = solution Y2019

main :: IO ()
main = do
  arg <- getArgs
  (year, maybeDay) <- pure $  case arg of
    ("2019":d:_) -> (Y19, day d)
    _ -> (Y20, safeHead arg >>= day)
  putStrLn $ "Advent of Code " ++ showYear year ++ " - Day " ++ fromMaybe "" (safeHead arg)
  putStrLn $ "https://adventofcode.com/" ++ showYear year
  dataDir <- getDataDir
  setCurrentDirectory $ dataDir ++ inputDir year
  case maybeDay of
    Nothing -> putStrLn "Usage: AOC <day>" >>= const exitFailure
    Just d  -> uncurry format (solution year d)
