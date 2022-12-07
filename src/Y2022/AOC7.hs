{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2022.AOC7 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Control.Lens (folded, makeLenses, sumOf, view)
import Data.Either (isLeft, lefts)
import Data.Functor (($>))
import Data.List (sortOn)
import Data.Tree (Tree (Node), flatten, unfoldTree)
import Text.Parsec (char, letter, many1, newline, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Size = Int

data Cmd = Cd String | Ls
  deriving (Show, Eq)

data Listing = Directory String | File String Size
  deriving (Show, Eq, Ord)

data FileNode = FileNode
  { _fileName :: String,
    _fileSize :: Size
  }
  deriving (Show, Eq, Ord)

makeLenses ''FileNode

data DirectoryNode = DirectoryNode
  { _directoryName :: String,
    _directorySize :: Size,
    _directoryFiles :: [FileNode]
  }
  deriving (Show, Eq, Ord)

makeLenses ''DirectoryNode

type FileSystem = Tree DirectoryNode

mkFs :: [Either Listing Cmd] -> FileSystem
mkFs commands = annotateSize $ unfoldTree buildTree "/"
  where
    buildTree :: String -> ((String, [FileNode]), [String])
    buildTree n = walkTil commands "/" n |> foldl (f n) ([], []) |> \(t, d) -> ((n, t), d)
    f :: String -> ([FileNode], [String]) -> Listing -> ([FileNode], [String])
    f "/" (fs, ds) (Directory dn) = (fs, "/" <> dn : ds)
    f prev (fs, ds) (Directory dn) = (fs, prev <> "/" <> dn : ds)
    f _prev (fs, ds) (File fn fz) = (FileNode fn fz : fs, ds)

annotateSize :: Tree (String, [FileNode]) -> Tree DirectoryNode
annotateSize root@(Node (name, files) dirs) = Node (DirectoryNode name (calcDirSize root) files) (annotateSize <$> dirs)
  where
    calcDirSize :: Tree (String, [FileNode]) -> Size
    calcDirSize (Node (_name, dirFiles) subDirs) = sumOf (folded . fileSize) dirFiles + sum (calcDirSize <$> subDirs)

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

-- 1667443
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> mkFs
    |> flatten
    |> filter ((>) 100000 . view directorySize)
    |> fmap (view directorySize)
    |> sum

totalDiskSpace, requiredSpace :: Size
requiredSpace = 30000000
totalDiskSpace = 70000000

freeSpace :: FileSystem -> Size
freeSpace fsz = flatten fsz |> head |> view directorySize |> \usedSpace -> totalDiskSpace - usedSpace

missingSpace :: FileSystem -> Size
missingSpace fsz = requiredSpace - freeSpace fsz

directoryToDelete :: FileSystem -> DirectoryNode
directoryToDelete fsz = head . dropWhile ((< missingSpace fsz) . view directorySize) . sortOn (view directorySize) . flatten $ fsz

-- 8998590
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> mkFs
    |> directoryToDelete
    |> view directorySize

solution :: Solution
solution = PureSolution solution1 1667443 solution2 8998590
