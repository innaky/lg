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
twoInternalLst (x:xs) = [head x ++ "\t " ++  unwords (tail x)] ++ twoInternalLst xs

checkEmpty :: [[Char]] -> Maybe [[Char]]
checkEmpty [] = Nothing
checkEmpty input = Just input

toFilePath :: [String] -> FilePath
toFilePath [string] = string :: FilePath

kilobyte :: Integral a => a -> a
kilobyte bytes
  | bytes <= 1024 = bytes
  | otherwise = div bytes 1024

megabyte :: Integral a => a -> a
megabyte bytes
  | bytes <= 1048576 = kilobyte bytes
  | otherwise = div (kilobyte bytes) 1024

gigabyte :: Integral a => a -> a
gigabyte bytes
  |  bytes <= 1073741824 = megabyte bytes
  | otherwise = div (megabyte bytes) 1024

kilobyteStr :: (Show a, Integral a) => a -> [Char]
kilobyteStr bytes
  | bytes <= 1024 = show bytes
  | otherwise = show (kilobyte bytes) ++ "K"

megabyteStr :: (Show a, Integral a) => a -> [Char]
megabyteStr bytes
  | bytes <= 1048576 = kilobyteStr bytes
  | otherwise = show (megabyte bytes) ++ "M"

gigabyteStr :: (Show a, Integral a) => a -> [Char]
gigabyteStr bytes
  | bytes <= 1073741824 = megabyteStr bytes
  | otherwise = show (gigabyte bytes) ++ "G"

togigabytes :: [FileOffset] -> [String]
togigabytes [] = []
togigabytes (x:xs) = gigabyteStr x : togigabytes xs

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
usage = putStrLn "LG(1)\n\n\
\NAME\n\                                                                                                                                              
\\tlg - list greather files\n\n\
\SYNOPSIS\n\
\\tlg [OPTION] File|Directory\n\n\
\DESCRIPTION\n\                                                                                                                                      
\\tList the filesizes of a directory sorted from higher to small, the value of sizes is zero.\n\n\                                                      
\\t-h\n\t    return this text.\n\n\                                                                                                                            
\\t-b\n\t    return the filesizes in bytesizes format of a directory sorted from higher to small.\n\n\                                                            
\AUTHOR\n\                                                                                                                                            
\\tWrite by Inna Petrova and Erbeth Charte.\n\n\                                                                                                         
\REPORTING BUGS\n\                                                                                                                                    
\\tIssues in https://github.com/innaky\n\n\
\\tReport: <innaky@protonmail.com (Inna Petrova), echarte@tutanota.com (Erbeth Charte)>\n\n\
\COPYRIGHT\n\                                                                                                                                         
\\tCopyright (R) 2020. License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>.\n\n\                                            
\For extended help consult https://github.com/innaky"

synopsis :: IO ()
synopsis = putStrLn "SYNOPSIS\n\
\\tlg [ -h | --help | -b ] File|Directory \n\
\\tFor more information lg --help"

humanOutput :: [String] -> IO ()
humanOutput filename = do
  tupleall <- listGreater (toFilePath filename)
  let lstOfsets = fmap first_t tupleall
      lstHumanSizes = togigabytes lstOfsets
      lstnames = snd $ unzip tupleall
      lstall = mix lstHumanSizes lstnames
      lstinternal = twoInternalLst lstall
  mapM_ putStrLn lstinternal

bitsOutput :: [String] -> IO ()
bitsOutput filename = do
  tupleall <- listGreater (toFilePath filename)
  let lstsizes = offsetToString tupleall
      lstnames = snd $ unzip tupleall
      lstall = mix lstsizes lstnames
      lstinternal = twoInternalLst lstall
  mapM_ putStrLn lstinternal

checkParameters :: [String] -> IO ()
checkParameters list =
  case (length list) of
    1 -> humanOutput list
    2 -> options list

options :: [String] -> IO ()
options list =
  case (head list) of
    "-b" -> bitsOutput (tail list)

main :: IO ()
main = do
  filename <- getArgs
  case (checkEmpty filename) of
    Nothing -> usage
    Just ["-h"] -> synopsis
    Just ["-h", _] -> synopsis
    Just [_, "-h"] -> synopsis
    Just ["-b"] -> synopsis
    Just ["--help"] -> usage
    Just [_, "--help"] -> usage
    Just ["--help", _] -> usage
    Just _ -> checkParameters filename
