{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.List
import Data.Maybe 
import System.Environment
import System.IO
import System.IO.Unsafe
import Debug.Trace

test = [[1, 1, 1], [1, 2, 1], [1, 1, 1]]

-- Safe indexing into a List
element :: Maybe [a] -> Int -> Maybe a
element Nothing i = Nothing 
element (Just a) i = if i < (length a) && i >= 0 
                     then Just (a !! i) 
                     else Nothing

-- Return a List of the heights of all direct neighbours 
adjacentHeights m i j = do
    let a = Just m `element` (i + 1) `element` (j)
    let b = Just m `element` (i - 1) `element` (j)
    let c = Just m `element` (i) `element` (j + 1)
    let d = Just m `element` (i) `element` (j - 1)
    map (fromMaybe 0) [a, b, c, d] 

diffIfGreater a b = if a > b then a - b else 0

-- Get the surface area contribution of a single stack
stackSurfaceArea m i j = do
    let height = m !! i !! j
    let areas = map (diffIfGreater height) (adjacentHeights m i j)
    sum areas + 2 -- Top and bottom always contribute 

surfaceArea m = do
    let indexes = [(i, j) | i <- [0..(length m - 1)], 
                            j <- [0..(length (head m) - 1)]]
    sum $map (uncurry $ stackSurfaceArea m) indexes  

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode
    hwTemp <- getLine

    let hw = words hwTemp
    let h = read (hw !! 0) :: Int
    let w = read (hw !! 1) :: Int

    aTemp <- readMultipleLinesAsStringArray h
    let a = map (\x -> map (read :: String -> Int) . words $ x) aTemp
    let result = surfaceArea a

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
