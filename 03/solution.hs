-- |

module Main where

import Data.Maybe

main :: IO ()
main = interact $ show . countTrees . path (Trajectory 3 1) . readField

data Trajectory = Trajectory { right :: Int,
                               down :: Int
                             } deriving (Eq, Show)

data Cell = Empty | Tree deriving (Eq, Show, Ord)
type Field = [[Cell]]

readField :: String -> Field
readField = map (cycle . readCells) . lines

readCells :: String -> [Cell]
readCells = mapMaybe readCell

readCell :: Char -> Maybe Cell
readCell s
  | s == '.' = Just Empty
  | s == '#' = Just Tree
  | otherwise = Nothing

countTrees :: [Cell] -> Int
countTrees = length . filter (== Tree)

path :: Trajectory -> Field -> [Cell]
path _ []  = []
path (Trajectory r d) f
  | d == 0 = []
  | otherwise = map (\(i, l) -> l !! i) withIndex
  where
    withIndex = zip [i * r | i <- [0..]] $ takeEvery (abs d) f

takeEvery :: Int -> [a] -> [a]
takeEvery n = map snd . filter ((== n) . fst) . zip (cycle [1..n])
