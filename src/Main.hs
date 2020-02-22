module Main where

import System.Posix.Files (getFileStatus, fileSize)
import System.Posix.Types (FileOffset)
import System.Environment (getArgs)

getFileSize :: FilePath -> IO (FileOfset)
getFileSize filePath = do
  status <- getFileStatus filePath
  return (fileSize status)

main :: IO ()
main = do
  (filepath:_) <- getArgs
  size <- getFileSize filepath
  print size
