{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Foldable
import Data.List
import Data.Sequence
import System.Environment
import System.IO

doPermute :: Int -> Seq Int -> Int -> Int -> Int -> Seq Int
doPermute n s i k t  = do 
    let t' = if i `mod` k == 0 then (- t) else t 
    if i <= n
    then doPermute n (s |> (i + t)) (i + 1) k (t')
    else s 

permute n k = doPermute n Data.Sequence.Empty 1 k k 

absolutePermutation n k = do
    if k == 0 then 
        [1..n]
    else 
        if n `mod` (2 * k) == 0 then 
            toList $ permute n k
        else 
            [-1]

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode
    t <- readLn :: IO Int

    forM_ [1..t] $ \t_itr -> do
        nkTemp <- getLine

        let nk = words nkTemp
        let n = read (nk !! 0) :: Int
        let k = read (nk !! 1) :: Int
        let result = absolutePermutation n k

        hPutStrLn fptr $ intercalate " " $ map (\x -> show x) $ result

    hFlush fptr
    hClose fptr 
