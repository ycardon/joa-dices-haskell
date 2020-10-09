module Joa where

import Control.Monad.Random (getStdGen, runRand)
import Data.List (intercalate)
import Data.Map (fromListWith, toList)
import Dice (Face (..), Roll, rolldices)
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
      putStrLn $ "attack:  " ++ showRoll attackRoll
      putStrLn $ "defence: " ++ showRoll defenceRoll
      putStrLn $ "result:  " ++ showRoll (applyDefence attackRoll defenceRoll)
    else putStrLn $ showRoll attackRoll

-- count the number of a given face in a roll
count :: Face -> Roll -> Int
count face = length . filter (== face)

-- cancel roll faces by an amount of shield count [fold version]
cancel :: Face -> (Roll, Int) -> (Roll, Int)
cancel face (roll, shieldCount) = foldr f ([], shieldCount) roll
  where
    f x (xs, n) = if n <= 0 then (x : xs, 0) else if x == face then (xs, n - 1) else (x : xs, n)

-- apply defence shields on the attack and remove unrelevant faces from the attack
applyDefence :: Roll -> Roll -> Roll
applyDefence attack defence = filter (/= Blank) . filter (/= Shield) $ roll
  where
    (roll, _) = cancel Push . cancel Disrupt . cancel Kill $ (attack, count Shield defence)

-- frequency of each face in the roll [map variant]
frequency :: [Face] -> [(Face, Int)]
frequency roll = toList . fromListWith (+) $ [(face, 1) | face <- roll]

-- pretty print a roll
showRoll :: Roll -> String
showRoll = intercalate " | " . map (\(f, n) -> show n ++ " " ++ show f) . frequency

--------- implementation variants ---------

-- -- cancel roll faces by an amount of shield count [recursive variant]
-- cancel' :: Face -> (Roll, Int) -> (Roll, Int)
-- cancel' _ ([], n) = ([], n)
-- cancel' face (x : xs, n)
--   | n <= 0 = (x : xs, 0)
--   | x == face = cancel' face (xs, n -1)
--   | otherwise = (x : xs', n')
--   where
--     (xs', n') = cancel' face (xs, n)

-- import Data.List (group, sort)

-- -- frequency of each face in the roll [list group variant]
-- frequency' :: [Face] -> [(Face, Int)]
-- frequency' = map (\(x : xs) -> (x, length xs + 1)) . group . sort
