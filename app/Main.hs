module Main where

import Joa (joa)
import System.Environment (getArgs)

main :: IO ()
main = joa . unwords =<< getArgs
