{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           AOC
import           Data.Maybe         (fromMaybe)
import           Paths_AOC
import           System.Directory
import           System.Environment
import           Y2021.AOC          (Y2021 (Y2021))
import           Y2020.AOC          (Y2020 (Y2020))
import           Y2019.AOC          (Y2019 (Y2019))
import           Y2015.AOC          (Y2015 (Y2015))
import Test.HUnit (runTestTT)

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (a : _) = Just a

format :: (Show a, Show b) => IO a -> IO b -> IO ()
format s1 s2 = do
  putStrLn "Solution 1:"
  s1 >>= print
  putStrLn "Solution 2:"
  s2 >>= print

data Year = Y21 | Y20 | Y19 | Y15
instance AOC Year where
  showYear Y21 = showYear Y2021
  showYear Y20 = showYear Y2020
  showYear Y19 = showYear Y2019
  showYear Y15 = showYear Y2015
  inputDir Y21 = inputDir Y2021
  inputDir Y20 = inputDir Y2020
  inputDir Y19 = inputDir Y2019
  inputDir Y15 = inputDir Y2015
  solution Y21 = solution Y2021
  solution Y20 = solution Y2020
  solution Y19 = solution Y2019
  solution Y15 = solution Y2015
  verify Y21 = verify Y2021
  verify Y20 = verify Y2020
  verify Y19 = verify Y2019
  verify Y15 = verify Y2015

main :: IO ()
main = do
  arg <- getArgs
  let year = case arg of
              ("2015":_) -> Y15
              ("2019":_) -> Y19
              ("2020":_) -> Y20
              _ -> Y21
  let maybeDay = case arg of
                  (_:d:_) -> day d
                  _ -> Nothing
  putStrLn $ "Advent of Code " ++ showYear year ++ " - Day " ++ fromMaybe "" (safeHead arg)
  putStrLn $ "https://adventofcode.com/" ++ showYear year
  dataDir <- getDataDir
  setCurrentDirectory $ dataDir ++ inputDir year
  case maybeDay of
    Nothing -> do
       _ <- putStrLn $ "Verify year " ++ showYear year
       _ <- runTestTT $ verify year
       return ()
    Just d  -> uncurry format (solution year d)
