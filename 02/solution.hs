-- |

module Main where

import Data.List.Split (splitOn)
import Data.Maybe

main :: IO ()
main = interact $ \s -> (show . length . filter valid) $ readPasswords $ lines s

data Policy = Policy Char (Int, Int)
  deriving (Show)

data Password = Password { policy :: Policy,
                           secret :: String
                         } deriving (Show)

readPasswords :: [String] -> [Password]
readPasswords [] = []
readPasswords s = mapMaybe readPassword s

readPassword :: String -> Maybe Password
readPassword s
  | length s < 8 = Nothing -- must by min 8 characters
  | otherwise = Just (Password (Policy c (mi, ma)) sec)
  where
    byColon = splitOn ": " s
    pol = head byColon
    sec = head $ tail byColon

    byWhitespace = splitOn " " pol
    pair = head byWhitespace
    c = (head . head) $ tail byWhitespace

    byDash = splitOn "-" pair
    mi = read $ head byDash :: Int
    ma = read $ head $ tail byDash :: Int

valid :: Password -> Bool
valid (Password (Policy c (mi, ma)) s)
  | mi > ma                     = False
  | mi == 0                     = True
  | count >= mi && count <= ma  = True
  | otherwise                   = False
  where
    count = length $ filter (== c) s
