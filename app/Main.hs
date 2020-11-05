{-# LANGUAGE LambdaCase #-}
module Main where

import Lib
import System.Posix.Types (FileOffset)
import System.Posix.Files (getSymbolicLinkStatus, fileSize, isSymbolicLink)
import System.Directory (doesDirectoryExist, listDirectory, doesFileExist)
import Control.Monad (forM)
import System.FilePath.Posix ((</>))
import System.Environment (getArgs)
import Data.List (sortBy)
import Data.Function (on)
import System.Exit (exitWith, ExitCode(ExitFailure))

-- pure
mySort :: Ord a => [(a, b)] -> [(a, b)]
mySort = sortBy (flip compare `on` fst)

offsetToString :: [(FileOffset, FilePath)] -> [String]
offsetToString [] = []
offsetToString (x:xs) = lines (show (first_t x)) ++ offsetToString xs

first_t :: (FileOffset, FilePath) -> FileOffset
first_t (a,_) = a

mix :: [String] -> [String] -> [[String]]
mix [] _ = []
mix _ [] = []
mix (x:xs) (y:ys) =  [x:y:[]] ++ mix xs ys

twoInternalLst :: [[String]] -> [String]
twoInternalLst [] = []
twoInternalLst (x:xs) = [head x ++ "\t " ++  unwords (tail x)] ++ twoInternalLst xs

-- impure
getFileSize :: FilePath -> IO FileOffset
getFileSize filepath = do
  fileexist <- doesFileExist filepath
  isDir <- doesDirectoryExist filepath
  if fileexist
    then do
    stat <- getSymbolicLinkStatus filepath
    let isSymbLink = isSymbolicLink stat
    if isSymbLink && isDir
      then do
      return 0
      else return (fileSize stat)
    else return 0

ifDirAndSymbolicLink :: FilePath -> IO Bool
ifDirAndSymbolicLink filepath = do
  isDir <- doesDirectoryExist filepath
  stat <- getSymbolicLinkStatus filepath
  let isSymbLink = isSymbolicLink stat
  return (isSymbLink && isDir)

completePath :: FilePath -> IO [FilePath]
completePath mainDir = do
  simpleFileNames <- listDirectory mainDir
  allPaths <- forM simpleFileNames $ \simpleName -> do
    let path = mainDir </> simpleName
    return [path]
  return (concat allPaths)

getSize :: FilePath -> IO [FileOffset]
getSize filepath = do
  isDir <- doesDirectoryExist filepath
  filesize0 <- getFileSize filepath
  isDirAndSymbLk <- ifDirAndSymbolicLink filepath
  if isDirAndSymbLk
    then do
    return [0]
    else (if isDir
          then do
             localFiles <- completePath filepath
             allLocalPaths <- forM localFiles $ \filename -> do
               filesize1 <- getFileSize filename
               isDirInside <- doesDirectoryExist filename
               if isDirInside
                 then getSize filename
                 else return [filesize1]
             return (concat allLocalPaths)
          else return [filesize0])

getSizeSum :: FilePath -> IO FileOffset
getSizeSum filepath = do
  filesizes <- getSize filepath
  return (foldr (+) 0 filesizes)

listGreater :: FilePath -> IO [(FileOffset, FilePath)]
listGreater filepath = do
  isDir <- doesDirectoryExist filepath
  filesize0 <- getFileSize filepath
  isDirAndSymLk <- ifDirAndSymbolicLink filepath
  if isDirAndSymLk
    then do
    return [(0, filepath)]
    else (if isDir
          then do
             localFiles <- completePath filepath
             everyFile <- forM localFiles $ \filename -> do
               sumfile <- getSizeSum filename
               return (sumfile, filename)
             return (mySort everyFile)
          else return [(filesize0, filepath)])

usage :: IO ()
usage = putStr . unlines $
  [ "LG(1)"
  ,""
  ,"NAME"
  ,"    lg - list greater files"
  ,""
  ,"SYNOPSIS"
  ,"    lg [OPTION] File | Directory"
  ,""
  ,"DESCRIPTION"
  ,"    List the files ordered by size from largest to smallest."
  ,""
  ,"    -h | --help"
  ,"        Return this text."
  ,""
  ,"    -b"
  ,"        Return the file sizes in bytes format of a directory sorted from higher to small."
  ,""
  ,"    -v | --version"
  ,"        Show version."
  ,""
  ,"AUTHOR"
  ,"    Write by Inna Petrova and Erbeth Charte."
  ,""
  ,"REPORTING BUGS"
  ,"    Issues in https://github.com/innaky"
  ,"    Report: <innaky@protonmail.com (Inna Petrova), echarte@tutanota.com (Erbeth Charte)>"
  ,""
  ,"COPYRIGHT"
  ,"    Copyright (R) 2020. License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>"
  ,""
  ,"For extended help consult https://github.com/innaky"
  ,""
  ]

synopsis :: IO ()
synopsis = putStr . unlines $
  [ "SYNOPSIS"
  , "    lg [-h | --help ]"
  , "    lg [-b] Filename | Directory"
  , "    lg [-v | --version ]"
  , ""
  , "    For more information lg --help"
  ]

humanOut :: String -> IO ()
humanOut filename = do
  tupleall <- listGreater filename
  let lstOfsets = fmap first_t tupleall
      lstHumanSizes = togigabytes lstOfsets
      lstnames = snd $ unzip tupleall
      lstall = mix lstHumanSizes lstnames
      lstinternal = twoInternalLst lstall
  mapM_ putStrLn lstinternal

bitsOut :: String -> IO ()
bitsOut filename = do
  tupleall <- listGreater filename
  let lstsizes = offsetToString tupleall
      lstnames = snd $ unzip tupleall
      lstall = mix lstsizes lstnames
      lstinternal = twoInternalLst lstall
  mapM_ putStrLn lstinternal

version :: IO ()
version = putStr . unlines $
  [ "list greater files 0.2.8" ]

fileExist :: FilePath -> IO Bool
fileExist f = do
  file <- doesFileExist f
  if file
    then return file
    else do
    dir <- doesDirectoryExist f
    if dir
      then return dir
      else return False

main :: IO ()
main = getArgs >>= \case
  [] -> synopsis >> exitWith (ExitFailure 1)
  ["-h"] -> synopsis >> exitWith (ExitFailure 1)
  ["--help"] -> usage >> exitWith (ExitFailure 1)
  ["-v"] -> version >> exitWith (ExitFailure 1)
  ["--version"] -> version >> exitWith (ExitFailure 1)
  ["-b", path] -> bitsOut path
  [path] -> fileExist path >>= \case
    True -> humanOut path
    False -> synopsis >> exitWith (ExitFailure 1)
