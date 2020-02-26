module Main where

import System.Posix.Types (FileOffset)
import System.Posix.Files (getFileStatus, fileSize)
import System.Directory (doesDirectoryExist, listDirectory, doesFileExist)
import Control.Monad (forM)
import System.FilePath.Posix ((</>))
import System.Environment (getArgs)
import Data.List (sortBy)
import Data.Function (on)

-- pure
mySort :: Ord a => [(a, b)] -> [(a, b)]
mySort = sortBy (flip compare `on` fst)

-- impure
getFileSize :: FilePath -> IO FileOffset
getFileSize filepath = do
  fileexist <- doesFileExist filepath
  if fileexist
    then do
    stat <- getFileStatus filepath
    return (fileSize stat)
    else return 0

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
  if isDir
    then do
    localFiles <- completePath filepath
    allLocalPaths <- forM localFiles $ \filename -> do
      filesize1 <- getFileSize filename
      isDirInside <- doesDirectoryExist filename
      if isDirInside
        then getSize filename
        else return [filesize1]
    return (concat allLocalPaths)
    else return [filesize0]

getSizeSum :: FilePath -> IO FileOffset
getSizeSum filepath = do
  filesizes <- getSize filepath
  return (sum filesizes)

listGreater :: FilePath -> IO [(FileOffset, FilePath)]
listGreater filepath = do
  isDir <- doesDirectoryExist filepath
  filesize0 <- getFileSize filepath
  if isDir
    then do
    localFiles <- completePath filepath
    everyFile <- forM localFiles $ \filename -> do
      sumfile <- getSizeSum filename
      return (sumfile, filename)
    return (mySort everyFile)
    else return [(filesize0, filepath)]

main :: IO ()
main = do
  (filenam:_) <- getArgs
  listgreater <- listGreater filenam
  mapM_ print listgreater 
