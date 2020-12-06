-- |

module Main where

main :: IO ()
main = interact $ show . multPair . findSum 2020 . readInts . words

multPair :: Maybe (Int, Int) -> Int
multPair Nothing = 0
multPair (Just (x, y)) = x * y

readInts :: [String] -> [Int]
readInts s = map read s :: [Int]

findSum :: Int -> [Int] -> Maybe (Int, Int)
findSum _ [] = Nothing
findSum i l
  | i < 2       = Nothing
  | otherwise   = findDouble $ zip sml compl
  where
    sml = filter (< i) l
    compl = map (i -) sml

findDouble :: [(Int, Int)] -> Maybe (Int, Int)
findDouble [] = Nothing
findDouble ((x, y):xs)
  | (y, x) `elem` xs = Just (x, y)
  | otherwise = findDouble xs
