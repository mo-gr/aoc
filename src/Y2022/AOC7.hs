{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC7 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Control.Lens (view, _2)
import Data.Either (isLeft, lefts)
import Data.Functor (($>))
import Data.List (sortOn)
import Data.Tree (Tree (Node), flatten, unfoldTree)
import Text.Parsec (char, letter, many1, newline, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Cmd = Cd String | Ls
  deriving (Show, Eq)

type Size = Int

data Listing = Directory String | File String Size
  deriving (Show, Eq, Ord)

data FileNode = FileNode String Size
  deriving (Show, Eq, Ord)

type FileSystem = Tree (String, [FileNode])

type FileSystemWithSize = Tree (String, Int, [FileNode])

mkFs :: [Either Listing Cmd] -> FileSystem
mkFs commands = unfoldTree buildTree "/"
  where
    buildTree :: String -> ((String, [FileNode]), [String])
    buildTree n = walkTil commands "/" n |> foldl (f n) ([], []) |> \(t, d) -> ((n, t), d)
    f :: String -> ([FileNode], [String]) -> Listing -> ([FileNode], [String])
    f "/" (fs, ds) (Directory dn) = (fs, "/" <> dn : ds)
    f prev (fs, ds) (Directory dn) = (fs, prev <> "/" <> dn : ds)
    f _prev (fs, ds) (File fn fz) = (FileNode fn fz : fs, ds)

walkTil :: [Either Listing Cmd] -> String -> String -> [Listing]
walkTil ((Right (Cd "..")) : rest) dirStack target = walkTil rest (dropLastDir dirStack) target
walkTil ((Right (Cd "/")) : rest) _dirStack target = walkTil rest "/" target
walkTil ((Right (Cd dir)) : rest) "/" target = walkTil rest ("/" <> dir) target
walkTil ((Right (Cd dir)) : rest) dirStack target = walkTil rest (dirStack <> "/" <> dir) target
walkTil (Right Ls : rest) dirStack target | dirStack == target = lefts $ takeWhile isLeft rest
walkTil (_ : rest) dirStack target = walkTil rest dirStack target
walkTil [] _ _ = []

dropLastDir :: String -> String
dropLastDir = reverse . tail . dropWhile (/= '/') . reverse

inputParser :: Parser [Either Listing Cmd]
inputParser = many1 (Right <$> commandP <|> Left <$> listingP)
  where
    listingP :: Parser Listing
    listingP = (dirP <|> fileP) <* newline
    dirP = Directory <$> (string "dir " *> many1 letter)
    fileP = do
      sz <- number
      nm <- string " " *> many1 (letter <|> char '.')
      pure $ File nm sz
    commandP :: Parser Cmd
    commandP = (try cdP <|> lsP) <* newline
    cdP = Cd <$> (string "$ cd " *> many1 (char '/' <|> char '.' <|> letter))
    lsP = string "$ ls" $> Ls

fileSize :: FileNode -> Size
fileSize (FileNode _ sz) = sz

dirSize :: Tree (String, [FileNode]) -> Size
dirSize (Node (_name, files) []) = sum (fileSize <$> files)
dirSize (Node (name, files) (c : rest)) = dirSize c + dirSize (Node (name, files) rest)

annotateSize :: FileSystem -> FileSystemWithSize
annotateSize (Node (name, files) subDirs) = Node (name, nodeSize, files) (annotateSize <$> subDirs)
  where
    nodeSize =
      sum (fileSize <$> files)
        + sum (dirSize <$> subDirs)

-- 1667443
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> mkFs
    |> annotateSize
    |> flatten
    |> filter ((>) 100000 . view _2)
    |> fmap (view _2)
    |> sum


totalDiskSpace, requiredSpace :: Size
requiredSpace = 30000000
totalDiskSpace = 70000000

freeSpace :: FileSystemWithSize -> Size
freeSpace fsz = flatten fsz |> head |> view _2 |> \usedSpace -> totalDiskSpace - usedSpace

missingSpace :: FileSystemWithSize -> Size
missingSpace fsz = requiredSpace - freeSpace fsz

directoryToDelete :: FileSystemWithSize -> (String, Int, [FileNode])
directoryToDelete fsz = head . dropWhile ((< missingSpace fsz) . view _2) . sortOn (view _2) . flatten $ fsz

-- 8998590
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> mkFs
    |> annotateSize
    |> directoryToDelete
    |> view _2

solution :: Solution
solution = PureSolution solution1 1667443 solution2 8998590
