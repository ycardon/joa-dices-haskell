module Dice (Dice, Face (..), Roll, blackDice, redDice, yellowDice, whiteDice, giganticDice, doomDice, rolldices) where

import Control.Monad.Random (Rand, getRandomR)
import Data.List (genericIndex)
import System.Random (RandomGen)

-- the different faces of a dice
data Face = Kill | Disrupt | Push | Shield | Blank | Trample | Death | Rally | DelayedRally
  deriving (Enum, Show, Eq, Ord, Bounded)

-- the JoA dices
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

-- roll one dice
roll1 :: RandomGen g => Dice -> Rand g Face
roll1 dice = return . genericIndex dice =<< getRandomR (0, length dice - 1)

-- roll a dice several times
rolln :: RandomGen g => (Int, Dice) -> Rand g Roll
rolln (n, dice) = mapM roll1 (replicate n dice)

-- roll a set of dices
rolldices :: RandomGen g => [(Int, Dice)] -> Rand g Roll
rolldices dices = return . concat =<< mapM rolln dices

--------- with do notations ---------

-- -- roll one dice
-- roll1' :: RandomGen g => Dice -> Rand g Face
-- roll1' dice = do
--   index <- getRandomR (1, length dice)
--   return (genericIndex dice (index - 1))

-- -- roll a dice several times [recursive variant]
-- rolln' :: RandomGen g => (Int, Dice) -> Rand g Roll
-- rolln' (n, dice)
--   | n <= 0 = return []
--   | otherwise = do
--     x <- roll1' dice
--     xs <- rolln' (n -1, dice)
--     return (x : xs)

-- -- roll a set of dices  [recursive variant]
-- rolldices' :: RandomGen g => [(Int, Dice)] -> Rand g Roll
-- rolldices' [] = return []
-- rolldices' (x : xs) = do
--   x' <- rolln' (x)
--   xs' <- rolldices' (xs)
--   return (x' ++ xs')