module Joa where

import Data.List (group, sort)
import Data.Map (fromListWith, toList)
import Dice (Face, applyDefence, rolldices)
import Parser (parse)
import System.Random (getStdGen)

-- parse a command and print the result
joa :: String -> IO ()
joa command = do
  let (attackDices, defenceDices, isDefence) = parse command

  gen <- getStdGen
  let (attackRoll, gen') = rolldices gen attackDices
  let (defenceRoll, _) = rolldices gen' defenceDices

  if isDefence
    then do
      print ("attack", frequency attackRoll)
      print ("defence", frequency defenceRoll)
      print ("result", frequency (applyDefence attackRoll defenceRoll))
    else print (frequency attackRoll)

-- frequency of each face in the roll (using a map)
frequency :: [Face] -> [(Face, Int)]
frequency roll = toList (fromListWith (+) [(face, 1) | face <- roll])

-- frequency of each face in the roll (using list group)
frequency' :: [Face] -> [(Face, Int)]
frequency' = map (\(x : xs) -> (x, length xs + 1)) . group . sort