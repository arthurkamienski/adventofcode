import qualified Data.Set as Set
import Data.List (sort)

type Coord       = (Int, Int)
type Asteroid    = Coord

main = do
  asteroids <- readInputFile "input.txt"
  let maxSight = snd $ maximum $ sightCount asteroids
  let maxInSight = asteroidsInSight asteroids maxSight
  let vaporizeOrder = asteroidOrder maxInSight maxSight

  print maxSight
  let n200 = vaporizeOrder !! 199
  print n200

readInputFile :: String -> IO [Asteroid]
readInputFile path = do
  input <- readFile path
  return $ parseInput input

parseInput :: String -> [Asteroid]
parseInput input = parseRows 0 $ lines input
  where
    parseRows _ []     = []
    parseRows y (r:rs) = others ++ thisRow
      where
        thisRow = parseCols (0, y) r
        others  = parseRows (y+1) rs
        parseCols _ []     = []
        parseCols coord@(x, y) (c:cs) = case c of
          '#' -> coord : others
          _   -> others
          where
            others = parseCols (x+1, y) cs

sightCount :: [Asteroid] -> [(Int, Coord)]
sightCount asts = map (coordCount) asts
  where
    coordCount c = (length $ asteroidsInSight asts c, c)


asteroidsInSight :: [Asteroid] -> Asteroid -> [Asteroid]
asteroidsInSight asts c = filter (inSight astSet c) asts
  where
    astSet = Set.fromList asts

inSight :: Set.Set Asteroid -> Asteroid -> Asteroid -> Bool
inSight asts c1 c2 = not $ any inMap between
  where
    between = coordsBetween c1 c2
    inMap c = Set.member c asts

coordsBetween :: Coord -> Coord -> [Coord]
coordsBetween c1@(x1, y1) c2@(x2, y2)
  | c1 == c2  = [c1]
  | otherwise = zip xRange yRange
    where
      (xd, yd) = (x2-x1, y2-y1)
      divisor  = gcd xd yd
      (stepX, stepY) = (xd `div` divisor, yd `div` divisor)
      xRange = [x1+stepX, x1+2*stepX..x2-stepX]
      yRange = [y1+stepY, y1+2*stepY..y2-stepY]


asteroidOrder :: [Asteroid] -> Asteroid -> [Asteroid]
asteroidOrder asts (x, y) = map snd $ sort $ map angleAst asts
  where
    angleAst c = (angle c, c)
    angle    c = angleBetween refVector $ shiftOrigin c
    refVector = (0, -1)
    shiftOrigin (x1, y1) = (x1-x, y1-y)

angleBetween :: Asteroid -> Asteroid -> (Int, Float)
angleBetween c1@(x1, y1) c2@(x2, y2) = (dir, angle)
  where
    angle      = (fromIntegral dir) * (dotProduct / ((mag c1) * (mag c2)))
    mag (x, y) = sqrt . fromIntegral $ (x ^ 2) + (y ^ 2)
    dotProduct = fromIntegral (x1*x2+y1*y2)
    dir | x2 >= 0    = -1
        | otherwise  = 1
