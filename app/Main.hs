module Main where

import Data.List (intercalate)
import Joa (joa)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  joa $ intercalate " " args
