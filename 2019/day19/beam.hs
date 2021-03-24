import IntCodeComputer
import qualified Data.Map.Strict as Map
import Data.List.Split (chunksOf)

type Beam = Map.Map Int (Int, Int)
type Coord = (Int, Int)

main :: IO ()
main = do
  p <- readProgramFile "input.txt"
  print $ countArea $ findBeam p 50
  print $ findArea p 100

isPulled :: Program -> Coord -> Bool
isPulled p (x, y) = progOut == 1 
  where
    xInt = toInteger x
    yInt = toInteger y
    state = startState p [xInt, yInt]
    progOut = head $ output $ execUntilOutput state []

findLeft :: Program -> Coord -> Int -> Maybe Int
findLeft p c@(x, y) maxCoord
  | x > maxCoord  = Nothing
  | isPulled p c  = Just x
  | otherwise     = findLeft p (x+1, y) maxCoord

findRight :: Program -> Coord -> Int -> Int
findRight p c@(x, y) maxCoord
  | not $ isPulled p c = x
  | x > maxCoord       = maxCoord
  | otherwise          = findRight p (x+1, y) maxCoord

findBounds :: Program -> Coord -> Int -> Int -> Maybe Coord
findBounds p c@(x, y) maxX d = case left of
                            Nothing -> Nothing
                            Just xl -> Just (xl, xr)
                              where
                                xr = findRight p (xl+d, y) maxX
                            where
                              left = findLeft p c maxX

findBeam :: Program -> Int -> Beam
findBeam p maxCoord = findBeam' Map.empty (0, 0) 1
  where
    findBeam' m c@(x, y) d
      | y > maxCoord   = m
      | otherwise      = findBeam' nextM (xs, y+1) newD
      where
        newD = xe - xs
        (nextM, (xs, xe)) = case findBounds p c maxCoord d of
                  Nothing -> (m, (x, x+d))
                  Just bs -> (Map.insert y bs m, bs)

--findArea :: Program -> Int -> Coord
findArea p size = (x-size, y) 
  where
    (y, (_, x)) = head $ Map.toList candidates
    candidates = Map.filterWithKey fits beam
    beam = findBeam p 1500
    fits y (xs, xe) = fitsH && fitsV
      where
        fitsH = xe - xs >= size
        fitsV = xs2 == (xe - size)
          where
            xs2 = case beam Map.!? (y+99) of
                         Nothing -> xs
                         Just (x, _)  -> x

countArea :: Beam -> Int
countArea b = sum $ Map.map (\(s, e) -> e - s) b

printBeam :: Int -> Beam -> IO ()
printBeam maxC b = do
  let points = [(x, y) | y <- [0..maxC], x <- [0..maxC]]
  let chars = chunksOf (maxC+1) $ map getPoint points
  mapM_ putStrLn chars

  where
    getPoint (x, y) = case b Map.!? y of
                        Nothing -> '.'
                        Just (x1, x2)
                          | x >= x1 && x < x2 -> '#'
                          | otherwise -> '.'
