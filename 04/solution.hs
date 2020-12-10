-- |

module Main where

import Prelude hiding (lookup)
import Data.Map
import Text.ParserCombinators.ReadP

data Passport = Passport { birthYear    :: Int,
                           issueYear    :: Int,
                           expiryYear   :: Int,
                           height       :: Int,
                           hairColour   :: String,
                           eyeColour    :: String,
                           pId          :: String,
                           cId          :: Maybe String
                         } deriving (Show)


splitColon :: ReadP (String, String)
splitColon = do
  key <- munch (/=':')
  _ <- string ":"
  value <- munch (/=' ')
  _ <- string " "
  return (key, value)


pairParser :: ReadP [(String, String)]
pairParser = many splitColon

readPass :: String -> Maybe Passport
readPass s = do
  bY <- lookupNum "byr" m
  iY <- lookupNum "iyr" m
  eY <- lookupNum "eyr" m
  h <- lookupNum "hgt" m
  hC <- lookup "hcl" m
  eC <- lookup "ecl" m
  p <- lookup "pid" m
  c <- lookup "cid" m

  return (Passport bY, iY, eY, h, hC, eC, p, c)
  where
    m = (fromList . fst . last . readP_to_S pairParser) s

lookupNum :: String -> Map String String -> Maybe Int
lookupNum s m = read <$> lookup s m


main :: IO ()
main = undefined
