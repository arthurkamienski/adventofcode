import Data.HashMap as Map

main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  print $ checksum ls
  print $ findBox ls

countLetters :: String -> Map Char Int -> Map Char Int
countLetters s = countLetters' s empty 
  where
    countLetters' [] mp = mp
    countLetters' (c:cs) mp = countLetters' cs $ insertWith (+) c 1 mp

has :: String -> Int -> Bool
has s k = not . null . Map.filter (==k) $ countLetters s

checksum :: [String] -> Int
checksum ss = (numberOf 3) * (numberOf 2)
    where numberOf k = length . Prelude.filter (`has` k) $ ss

common :: String -> String -> [Char]
common s1 s2 = [c1 | (c1, c2) <- (zip s1 s2), c1 == c2]

findBox :: [String] -> String
findBox ss = findBox' [(s1, s2) | s1 <- ss, s2 <- ss]
  where
    findBox' [] = ""
    findBox' ((s1,s2):ss)
      | (length c) == (length s1)-1 = c
      | otherwise = findBox' ss
        where c = common s1 s2