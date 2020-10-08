module Joa where

import Control.Monad.Random (getStdGen, runRand)
import Data.List (group, sort)
import Dice (Face, applyDefence, rolldices)
import Parser (parse)

-- parse a command and print the result
joa :: String -> IO ()
joa command = do
  let (attackDices, defenceDices, isDefence) = parse command

  gen <- getStdGen
  let (attackRoll, gen') = runRand (rolldices attackDices) gen
  let (defenceRoll, _) = runRand (rolldices defenceDices) gen'

  if isDefence
    then do
      print ("attack", frequency attackRoll)
      print ("defence", frequency defenceRoll)
      print ("result", frequency (applyDefence attackRoll defenceRoll))
    else print (frequency attackRoll)

-- frequency of each face in the roll (using list group)
frequency :: [Face] -> [(Face, Int)]
frequency = map (\(x : xs) -> (x, length xs + 1)) . group . sort

-- -- frequency of each face in the roll (using a map)
-- frequency :: [Face] -> [(Face, Int)]
-- frequency roll = toList (fromListWith (+) [(face, 1) | face <- roll])
