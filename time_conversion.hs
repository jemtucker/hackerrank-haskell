{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Data.List
import Data.Time
import System.Environment
import System.IO

iFormat = "%I:%M:%S%p"
oFormat = "%T"

parse x = parseTimeM False defaultTimeLocale iFormat x :: Maybe TimeOfDay

write t = formatTime defaultTimeLocale oFormat t

-- Complete the timeConversion function below.
timeConversion s = do
    case parse s of
        Nothing -> error "Shitballs"
        Just t  -> write t

main :: IO()
main = do
    outputPath <- getEnv "OUTPUT_PATH"
    fptr <- openFile outputPath WriteMode
    s <- getLine

    let result = timeConversion s

    hPutStrLn fptr result
    hFlush fptr
    hClose fptr
