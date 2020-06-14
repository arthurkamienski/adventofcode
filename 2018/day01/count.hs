import System.IO
import Data.HashSet (HashSet, insert, member, singleton)

main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let is = map toInt ls
  
  print $ sum is

  let fs = frequencies 0 (cycle is)
  
  print $ repeated fs (singleton 0)

repeated :: [Int] -> HashSet Int -> Int
repeated [] hs = 0
repeated (i:is) hs
  | member i hs = i
  | otherwise = repeated is (insert i hs)

frequencies :: Int -> [Int] -> [Int]
frequencies k [] = k
frequencies k (i:is) = n : frequencies n is
  where n = k+i

toInt :: String -> Int
toInt (s:i)
  | s == '+' = read i 
  | otherwise = read $ s:i