{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import AOC
import Data.Maybe (fromMaybe)
import Paths_AOC
import System.Directory
import System.Environment
import Test.HUnit (runTestTT)
import Util (safeHead)
import qualified Y2015.AOC as Y2015 (year)
import qualified Y2016.AOC as Y2016 (year)
import qualified Y2017.AOC as Y2017 (year)
import qualified Y2019.AOC as Y2019 (year)
import qualified Y2020.AOC as Y2020 (year)
import qualified Y2021.AOC as Y2021 (year)

format :: IO String -> IO String -> IO ()
format s1 s2 = do
  putStrLn "Solution 1:"
  s1 >>= putStrLn
  putStrLn "Solution 2:"
  s2 >>= putStrLn

main :: IO ()
main = do
  arg <- getArgs
  let year = case arg of
        ("2015" : _) -> Y2015.year
        ("2016" : _) -> Y2016.year
        ("2017" : _) -> Y2017.year
        ("2019" : _) -> Y2019.year
        ("2020" : _) -> Y2020.year
        _ -> Y2021.year
  let maybeDay = case arg of
        (_ : d : _) -> day d
        _ -> Nothing
  putStrLn $ "Advent of Code " ++ showYear year ++ " - Day " ++ fromMaybe "" (safeHead . tail $ arg)
  putStrLn $ "https://adventofcode.com/" ++ showYear year
  dataDir <- getDataDir
  setCurrentDirectory $ dataDir ++ inputDir year
  case maybeDay of
    Nothing -> do
      _ <- putStrLn $ "Verify year " ++ showYear year
      _ <- runTestTT $ verify year
      return ()
    Just d -> uncurry format (solution year d)
