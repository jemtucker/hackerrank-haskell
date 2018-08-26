{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe


addHash l = '#' : l

stair n = iterate addHash [] !! n

leftpad n s = replicate (n - length s) ' ' ++ s

stairs n = map (leftpad n) (map stair [1..n])

staircase n = do
    intercalate "\n" (stairs n)    

putStairs n = putStrLn (staircase n)

main :: IO()
main = do
    n <- readLn :: IO Int 
    putStairs n
