{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe


minSum :: (Num a, Foldable t, Ord a) => t a -> a
minSum arr = sum arr - maximum arr 

maxSum :: (Num a, Foldable t, Ord a) => t a -> a
maxSum arr = sum arr - minimum arr 

miniMaxSum :: (Show a, Num a, Foldable t, Ord a) => t a -> IO ()
miniMaxSum arr = do
    let min = minSum arr
    let max = maxSum arr 
    putStr (show min)
    putStr " "
    putStr (show max) 

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    arrTemp <- getLine
    let arr = Data.List.map (read :: String -> Int) . words $ arrTemp
    miniMaxSum arr 
