module Joa where

import Control.Monad.Random (getStdGen, runRand)
import Data.List (group, sort)
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
      putStrLn $ "attack  + " ++ showRoll attackRoll
      putStrLn $ "defence - " ++ showRoll defenceRoll
      putStrLn $ "result  = " ++ showRoll (applyDefence attackRoll defenceRoll)
    else putStrLn $ showRoll attackRoll

-- apply defence shields on the attack and remove unrelevant faces from the attack
applyDefence :: Roll -> Roll -> Roll
applyDefence attack defence = filter (/= Blank) . filter (/= Shield) $ result
  where
    (result, _) = cancel Push . cancel Disrupt . cancel Kill $ (attack, count Shield defence)

-- cancel roll faces by an amount of shield count [fold version]
cancel :: Face -> (Roll, Int) -> (Roll, Int)
cancel face (roll, shieldCount) = foldr f ([], shieldCount) roll
  where
    f x (xs, n) = if n <= 0 then (x : xs, 0) else if x == face then (xs, n - 1) else (x : xs, n)

--------- utilities ---------

-- pretty print a roll
showRoll :: Roll -> String
showRoll = concat . map align . frequency
  where
    align (face, n) = padL 2 (show n) ++ " " ++ padR 8 (show face)

-- count the number of a given face in a roll
count :: Face -> Roll -> Int
count face = length . filter (== face)

-- -- frequency of each face in the roll [list group variant]
frequency :: [Face] -> [(Face, Int)]
frequency = map (\(x : xs) -> (x, length xs + 1)) . group . sort

-- right pad a string
padR :: Int -> String -> String
padR n s = if length s < n then s ++ replicate (n - length s) ' ' else s

-- left pad a string
padL :: Int -> String -> String
padL n s = reverse (padR n (reverse s))

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

-- import Data.Map (fromListWith, toList)

-- -- frequency of each face in the roll [map variant]
-- frequency :: [Face] -> [(Face, Int)]
-- frequency roll = toList . fromListWith (+) $ [(face, 1) | face <- roll]
