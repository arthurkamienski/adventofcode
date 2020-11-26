main = do
  fileContent <- readFile "input.txt"
  let moduleMasses = map toInt $ lines fileContent
  print $ sum $ map requiredFuel moduleMasses
  print $ sum $ map fuelMass moduleMasses

requiredFuel :: Int -> Int
requiredFuel m = max 0 (m `div` 3 - 2)

toInt :: [Char] -> Int
toInt s = read s :: Int

fuelMass :: Int -> Int
fuelMass m
  | m <= 0    = 0
  | otherwise = fm + fuelMass fm
    where
      fm = requiredFuel m
