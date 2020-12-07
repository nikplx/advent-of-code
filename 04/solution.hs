-- |

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Internal.Search
import Data.Text (pack)
import Data.List

data Passport = Passport { birthYear :: Int,
                           issYear :: Int,
                           expYear :: Int,
                           height :: Int,
                           hairCol :: String,
                           eyeCol :: String,
                           pId :: String,
                           cId :: Maybe String
                         }

readPassport :: String -> Passport
readPassport s = undefined


readField :: String -> String -> Maybe String
readField q s
  | null idx = Nothing
  | otherwise = Just $ drop (head idx) s
  where
    idx = indices (pack q) (pack s)


main :: IO ()
main = undefined