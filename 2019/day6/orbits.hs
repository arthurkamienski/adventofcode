import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn)

type Object   = String
type Orbits   = [Object]
type OrbitMap = Map.Map Object Object

main = do
  m <- readInputFile "input.txt"
  let total = totalOrbits m
  let minTs = minTransfers m "YOU" "SAN"
  print $ total
  print $ minTs

readInputFile :: String -> IO OrbitMap
readInputFile path = do
  input <- readFile $ path
  return $ parseInput input

parseInput :: String -> OrbitMap
parseInput input = foldl addToMap Map.empty orbitPairs
  where
    orbitPairs = map (splitOn ")") $ lines input
    addToMap m [o1, o2] = Map.insert o2 o1 m

orbitsOf :: OrbitMap -> String -> [String]
orbitsOf m o1 = case o1 of
                "COM" -> []
                _  -> o2 : (orbitsOf m o2)
                where
                  o2 = m Map.! o1

numberOrbits :: OrbitMap -> String -> Int
numberOrbits m o = (length $ orbitsOf m o)

totalOrbits :: OrbitMap -> Int
totalOrbits m = sum $ map (numberOrbits m) (Map.keys m)

minTransfers :: OrbitMap -> Object -> Object -> Int
minTransfers m o1 o2 = countDiffs orbs1 orbs2
  where
    revOrbit o = reverse $ orbitsOf m o
    orbs1      = revOrbit o1
    orbs2      = revOrbit o2
    countDiffs (x:xs) (y:ys)
      | x == y    = countDiffs xs ys
      | otherwise = length xs + length ys + 2
