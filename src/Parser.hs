-- | Johan of Arc command line parser
module Parser (parse) where

import Data.Char (toUpper)
import Dice (Dice, blackDice, doomDice, giganticDice, redDice, whiteDice, yellowDice)

-- | parse a fight in the form : 3R Y - 2B 1W
parse :: String -> ([(Int, Dice)], [(Int, Dice)], Bool)
parse = foldl f ([], [], False) . words
  where
    f (att, def, isDef) str
      | str `elem` ["-", "/"] = (att, def, True)
      | otherwise = case parseDice . toUpper . last $ str of
        Nothing -> (att, def, isDef)
        Just d ->
          if not isDef
            then ((n, d) : att, def, isDef)
            else (att, (n, d) : def, isDef)
          where
            n = parseInt . init $ str

-- | return an integer value or 1
parseInt :: String -> Int
parseInt s = case reads s of
  [(i, "")] -> i
  _ -> 1

-- | return the corresponding dice
parseDice :: Char -> Maybe Dice
parseDice 'B' = Just blackDice
parseDice 'R' = Just redDice
parseDice 'Y' = Just yellowDice
parseDice 'W' = Just whiteDice
parseDice 'G' = Just giganticDice
parseDice 'D' = Just doomDice
parseDice _ = Nothing
