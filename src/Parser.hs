module Parser (parse) where

import Data.Char (toUpper)
import Dice (Dice, blackDice, doomDice, giganticDice, redDice, whiteDice, yellowDice)

-- parse a fight in the form : 3R Y - 2B 1W
parse :: String -> ([(Int, Dice)], [(Int, Dice)], Bool)
parse = foldl f ([], [], False) . words
  where
    f (att, def, isDef) s
      | s == "-" = (att, def, True)
      | otherwise = case dice . toUpper . last $ s of
        Just d ->
          if not isDef
            then ((n, d) : att, def, isDef)
            else (att, (n, d) : def, isDef)
          where
            n = parseInt . init $ s
        Nothing -> (att, def, isDef)

-- return the int value, or 1
parseInt :: String -> Int
parseInt s = if length (reads s :: [(Int, String)]) == 1 then read s else 1

-- return the corresponding dice
dice :: Char -> Maybe Dice
dice 'B' = Just blackDice
dice 'R' = Just redDice
dice 'Y' = Just yellowDice
dice 'W' = Just whiteDice
dice 'G' = Just giganticDice
dice 'D' = Just doomDice
dice _ = Nothing
