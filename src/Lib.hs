module Lib
    ( gigabyteStr,
      togigabytes
    ) where

import System.Posix.Types (FileOffset)

kilobyte :: Integral a => a -> a
kilobyte bytes
  | bytes <= 1000 = bytes
  | otherwise = div bytes 1000

megabyte :: Integral a => a -> a
megabyte bytes
  | bytes <= 1000000 = kilobyte bytes
  | otherwise = div (kilobyte bytes) 1000

gigabyte :: Integral a => a -> a
gigabyte bytes
  | bytes <= 1000000000 = megabyte bytes
  | otherwise = div (megabyte bytes) 1000

kilobyteStr :: (Show a, Integral a) => a -> [Char]
kilobyteStr bytes
  | bytes <= 1000 = show bytes
  | otherwise = show (kilobyte bytes) ++ "K"

megabyteStr :: (Show a, Integral a) => a -> [Char]
megabyteStr bytes
  | bytes <= 1000000 = kilobyteStr bytes
  | otherwise = show (megabyte bytes) ++ "M"

gigabyteStr :: (Show a, Integral a) => a -> [Char]
gigabyteStr bytes
  | bytes <= 1000000000 = megabyteStr bytes
  | otherwise = show (gigabyte bytes) ++ "G"

togigabytes :: [FileOffset] -> [String]
togigabytes [] = []
togigabytes (x:xs) = gigabyteStr x : togigabytes xs
