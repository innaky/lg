module Main where

import System.Posix.Files (getFileStatus, fileSize)
import System.Directory (doesDirectoryExist, listDirectory)
import Control.Monad (forM)
import System.FilePath.Posix ((</>))
import System.Environment (getArgs)

getFileSize filepath = do
  status <- getFileStatus filepath
  return (fileSize status)

completePath mainDir = do
  simpleFileNames <- listDirectory mainDir
  allPaths <- forM simpleFileNames $ \simpleName -> do
    let path = mainDir </> simpleName
    return [path]
  return (concat allPaths)
  
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
        -- with filename
        --else return [(filename, filesize1)]
    return (concat allLocalPaths)
    else return [filesize0]
    -- with filename
    -- else return [(filepath, filesize0)]

getSizeSum filepath = do
  filesizes <- getSize filepath
  return (sum filesizes)

listGreater filepath = do
  localFiles <- completePath filepath
  everyFile <- forM localFiles $ \filename -> do
    sumfile <- getSizeSum filename
    return (filename, sumfile)
  return everyFile

main = do
  (filenam:_) <- getArgs
  listgreater <- listGreater filenam
  print listgreater
