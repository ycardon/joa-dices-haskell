module Dice (Dice, Face, blackDice, redDice, yellowDice, whiteDice, giganticDice, doomDice, rolln', rolldices, applyDefence) where

import System.Random (RandomGen, randomR)

-- the different faces of a dice
data Face
  = Kill
  | Disrupt
  | Push
  | Shield
  | Blank
  | Trample
  | Death
  | Rally
  | DelayedRally
  deriving (Enum, Show, Eq, Ord, Bounded)

-- some dice
type Dice = [Face]

blackDice :: Dice
blackDice = [Kill, Disrupt, Disrupt, Shield, Shield, Shield]

redDice :: Dice
redDice = [Kill, Kill, Disrupt, Disrupt, Push, Shield]

yellowDice :: Dice
yellowDice = [Disrupt, Push, Push, Shield, Blank, Blank]

whiteDice :: Dice
whiteDice = [Disrupt, Disrupt, Push, Shield, Shield, Blank]

giganticDice :: Dice
giganticDice = [Kill, Disrupt, Disrupt, Push, Trample, Trample]

doomDice :: Dice
doomDice = [Disrupt, Death, Death, Rally, Rally, DelayedRally]

-- the result of rolling several dices
type Roll = [Face]

-- roll a dice several times
rolln' :: RandomGen gen => gen -> Int -> Dice -> Roll
rolln' gen n dice
  | n <= 0 = []
  | otherwise = face : (rolln' gen' (n - 1) dice)
  where
    (index, gen') = randomR (1, length dice) gen
    face = dice !! (index - 1)

rolln :: RandomGen gen => gen -> (Int, Dice) -> (Roll, gen)
rolln gen (n, dice)
  | n <= 0 = ([], gen)
  | otherwise = (face : faces, gen'')
  where
    (index, gen') = randomR (1, length dice) gen
    face = dice !! (index - 1)
    (faces, gen'') = rolln gen' (n - 1, dice)

rolldices :: RandomGen gen => gen -> [(Int, Dice)] -> (Roll, gen)
rolldices gen roll = foldl f ([], gen) roll
  where
    f (xs, gen') (n, dice) = (faces ++ xs, gen'')
      where
        (faces, gen'') = rolln gen' (n, dice)

-- count the number of a given face in a roll result
count :: Face -> Roll -> Int
count face roll = length (filter (== face) roll)

-- cancel roll faces by an amount of shield count (recursive version)
cancel :: Face -> (Roll, Int) -> (Roll, Int)
cancel _ ([], n) = ([], n)
cancel face (x : xs, n)
  | n <= 0 = (x : xs, 0)
  | x == face = cancel face (xs, n -1)
  | otherwise = (x : xs', n')
  where
    (xs', n') = cancel face (xs, n)

-- cancel roll faces by an amount of shield count (fold version)
cancel' :: Face -> (Roll, Int) -> (Roll, Int)
cancel' face (roll, shieldCount) = foldr f ([], shieldCount) roll
  where
    f x (xs, n) = if n <= 0 then (x : xs, 0) else if x == face then (xs, n - 1) else (x : xs, n)

-- apply defence shields on the attack and remove shields from the attack)
applyDefence :: Roll -> Roll -> Roll
applyDefence attack defence = filter (/= Blank) . filter (/= Shield) $ roll
  where
    (roll, _) = cancel Push . cancel Disrupt . cancel' Kill $ (attack, count Shield defence)
