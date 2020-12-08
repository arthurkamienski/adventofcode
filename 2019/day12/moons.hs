import Data.List.Split (splitOn)
import qualified Data.Set as Set

type Coord    = (Int, Int, Int)
type Velocity = Coord
data Moon  = Moon {c :: Coord, v :: Velocity} deriving (Show)

instance Eq Moon where
  (Moon c1 v1) == (Moon c2 v2) = (c1, v1) == (c2, v2)

instance Ord Moon where
  (Moon c1 v1) `compare` (Moon c2 v2) = (c1, v1) `compare` (c2, v2)

main = do
  moons <- readInputFile "input.txt"
  let selectors = map selectMoon [0..2]
  let firstRepeat = map (runUntilRepeat moons) selectors

  print $ totalSystemEnergy $ runNTicks moons 1000
  print $ foldl1 lcm firstRepeat

readInputFile :: String -> IO [Moon]
readInputFile path = do
  input <- readFile path
  return $ parseInput input

parseInput :: String -> [Moon]
parseInput i = map toMoon $ init $ splitOn "\n" i
  where
    toMoon s = Moon (x,y,z) (0,0,0)
      where
        [x, y, z] = getCoords s

getCoords :: String -> [Int]
getCoords []     = []
getCoords (c:cs) = case c of
                  '=' -> (toInt . takeUntilEnd $ cs) : keepGoing
                  _   -> keepGoing
  where
    toInt i = read i :: Int
    takeUntilEnd (i:is) = case i of
                            ',' -> []
                            '>' -> []
                            _   -> i : takeUntilEnd is
    keepGoing = getCoords cs

coordDiff :: Coord -> Coord -> Coord
coordDiff (x1, y1, z1) (x2, y2, z2) = (signum (x2-x1), signum (y2-y1), signum (z2-z1))

coordSum :: Coord -> Coord -> Coord
coordSum (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

sumCoordValues :: Coord -> Int
sumCoordValues (x, y, z) = (abs x)+(abs y)+(abs z)

netGravity :: Moon -> [Moon] -> Coord
netGravity m ms = foldl1 coordSum $ map moonCoordDiff ms
  where
    moonCoordDiff m2 = coordDiff (c m) (c m2)

updateVelocity :: Moon -> [Moon] -> Moon
updateVelocity m ms = m {v = coordSum (v m) (netGravity m ms)}

updatePos :: Moon -> Moon
updatePos m = m {c = coordSum (c m) (v m)}

moonTick :: [Moon] -> Moon -> Moon
moonTick ms m = updatePos $ updateVelocity m ms

tick :: [Moon] -> [Moon]
tick ms = map (moonTick ms) ms

potEnergy :: Moon -> Int
potEnergy m = sumCoordValues (c m)

kinEnergy :: Moon -> Int
kinEnergy m = sumCoordValues (v m)

totalEnergy :: Moon -> Int
totalEnergy m = (potEnergy m) * (kinEnergy m)

totalSystemEnergy :: [Moon] -> Int
totalSystemEnergy ms = sum $ map totalEnergy ms

runNTicks :: [Moon] -> Int -> [Moon]
runNTicks ms 0 = ms
runNTicks ms n = runNTicks (tick ms) (n-1)

runUntilRepeat :: [Moon] -> (Moon -> (Int, Int)) -> Int
runUntilRepeat ms f = runUntilRepeat' 1 (tick ms)
  where
    initState = map f ms
    runUntilRepeat' i ms 
      | state == initState = i+1
      | otherwise = runUntilRepeat'(i+1) (tick ms)
      where
        state = map f ms

select :: Coord -> Int -> Int
select (x, _, _) 0 = x
select (_, y, _) 1 = y
select (_, _, z) 2 = z

selectMoon :: Int -> Moon -> (Int, Int)
selectMoon i m = ((select (c m) i), (select (c m) i))
