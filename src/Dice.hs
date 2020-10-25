-- | Johan of Arc dices
module Dice (Dice, Face (..), Roll, blackDice, redDice, yellowDice, whiteDice, giganticDice, doomDice, rollDices) where

import Control.Monad.Random (Rand, getRandomR, replicateM)
import Data.List (genericIndex)
import System.Random (RandomGen)

-- | the possible faces of a dice
data Face = Kill | Disrupt | Push | Shield | Blank | Trample | Death | Rally | DelayedRally
  deriving (Enum, Show, Eq, Ord, Bounded)

-- | the JoA dices
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

-- | the result of rolling several dices
type Roll = [Face]

--------- roll dices ---------

-- | roll one dice
roll1 :: RandomGen g => Dice -> Rand g Face
roll1 dice = genericIndex dice <$> getRandomR (0, length dice - 1)

-- | roll a dice several times
rollN :: RandomGen g => (Int, Dice) -> Rand g Roll
rollN (n, dice) = replicateM n (roll1 dice)

-- | roll a set of dices
rollDices :: RandomGen g => [(Int, Dice)] -> Rand g Roll
rollDices dices = concat <$> mapM rollN dices

--------- with do notations ---------

-- -- roll one dice
-- roll1' :: RandomGen g => Dice -> Rand g Face
-- roll1' dice = do
--   index <- getRandomR (1, length dice)
--   return (genericIndex dice (index - 1))

-- -- roll a dice several times [recursive variant]
-- rollN' :: RandomGen g => (Int, Dice) -> Rand g Roll
-- rollN' (n, dice)
--   | n <= 0 = return []
--   | otherwise = do
--     x <- roll1' dice
--     xs <- rollN' (n -1, dice)
--     return (x : xs)

-- -- roll a set of dices  [recursive variant]
-- rollDices' :: RandomGen g => [(Int, Dice)] -> Rand g Roll
-- rollDices' [] = return []
-- rollDices' (x : xs) = do
--   x' <- rollN' (x)
--   xs' <- rollDices' (xs)
--   return (x' ++ xs')

--------- with >>= notations ---------

-- -- | roll one dice
-- roll1 :: RandomGen g => Dice -> Rand g Face
-- roll1 dice = getRandomR (0, length dice - 1) >>= return . genericIndex dice

-- -- | roll a dice several times
-- rollN :: RandomGen g => (Int, Dice) -> Rand g Roll
-- rollN (n, dice) = mapM roll1 (replicate n dice)

-- -- | roll a set of dices
-- rollDices :: RandomGen g => [(Int, Dice)] -> Rand g Roll
-- rollDices dices = mapM rollN dices >>= return . concat

--------- same with a newtype ---------

-- newtype Roll' = Roll' [Face]

-- rollN' :: RandomGen g => (Int, Dice) -> Rand g Roll'
-- rollN' (n, dice) = Roll' <$> replicateM n (roll1 dice)

-- rollDices' :: RandomGen g => [(Int, Dice)] -> Rand g Roll'
-- rollDices' dices = Roll' . concat <$> mapM rollN dices
