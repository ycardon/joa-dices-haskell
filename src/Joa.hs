-- | Johan of Arc business rules and CLI
module Joa where

import Control.Monad.Random (getStdGen, runRand)
import Data.List (group, sort)
import Dice (Face (..), Roll, rollDices)
import Parser (parse)

-- | parse a command and print the result
joa :: String -> IO ()
joa command = do
  let (attackDices, defenseDices, isDefense) = parse command

  gen <- getStdGen
  let (attackRoll, gen') = runRand (rollDices attackDices) gen
  let (defenseRoll, _) = runRand (rollDices defenseDices) gen'

  if isDefense
    then do
      putStrLn $ "attack  + " ++ showRoll attackRoll
      putStrLn $ "defense - " ++ showRoll defenseRoll
      putStrLn $ "result  = " ++ showRoll (applyDefense attackRoll defenseRoll)
    else putStrLn $ showRoll attackRoll

-- | apply defense shields on the attack and remove unrelevant faces from the attack
applyDefense :: Roll -> Roll -> Roll
applyDefense attack defense = filter (/= Blank) . filter (/= Shield) $ result
  where
    (result, _) = cancel Push . cancel Disrupt . cancel Kill $ (attack, count Shield defense)

-- | cancel roll faces by an amount of shield count [fold version]
cancel :: Face -> (Roll, Int) -> (Roll, Int)
cancel face (roll, shieldCount) = foldr f ([], shieldCount) roll
  where
    f x (xs, n)
      | x == face && n > 0 = (xs, n - 1)
      | otherwise = (x : xs, n)

--------- utilities ---------

-- | pretty print a roll
showRoll :: Roll -> String
showRoll = concatMap align . frequency
  where
    align (face, n) = padR 3 (show n) ++ " " ++ padL 8 (show face)

-- | count the number of a given face in a roll
count :: Face -> Roll -> Int
count face = length . filter (== face)

-- | frequency of each face in the roll [list group variant]
frequency :: [Face] -> [(Face, Int)]
frequency = map (\x -> (head x, length x)) . group . sort

-- | right pad a string
padR :: Int -> String -> String
padR n s = replicate (n - length s) ' ' ++ s

-- | left pad a string
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
