module Main where

import System.Posix.Types (FileOffset)
import System.Posix.Files (getSymbolicLinkStatus, fileSize, isSymbolicLink)
import System.Directory (doesDirectoryExist, listDirectory, doesFileExist)
import Control.Monad (forM)
import System.FilePath.Posix ((</>))
import System.Environment (getArgs)
import Data.List (sortBy)
import Data.Function (on)

-- pure
mySort :: Ord a => [(a, b)] -> [(a, b)]
mySort = sortBy (flip compare `on` fst)

-- return the sizes in string format
offsetToString :: [(FileOffset, FilePath)] -> [String]
offsetToString [] = []
offsetToString (x:xs) = lines (show (first_t x)) ++ offsetToString xs

-- return the sizes of the duple
first_t :: (FileOffset, FilePath) -> FileOffset
first_t (a,_) = a

mix :: [String] -> [String] -> [[String]]
mix [] _ = []
mix _ [] = []
mix (x:xs) (y:ys) =  [x:y:[]] ++ mix xs ys

twoInternalLst :: [[String]] -> [String]
twoInternalLst [] = []
twoInternalLst (x:xs) = [head x ++ " " ++  unwords (tail x)] ++ twoInternalLst xs

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
  if isSymbLink && isDir
    then do
    return True
    else return False

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
  return (sum filesizes)

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

main :: IO ()
main = do
  (filenam:_) <- getArgs
  tupleall <- listGreater filenam
  let lstsizes = offsetToString tupleall
      lstnames = snd $ unzip tupleall
      lstall = mix lstsizes lstnames
      lstinternal = twoInternalLst lstall
  mapM_ print lstinternal

