import IntCodeComputer
import qualified Data.Map.Strict as Map
import qualified Data.HashPSQ as Q
import qualified Data.Set as Set
import Data.List (sort)

data Tile = Empty | Wall | OxTank deriving (Show, Eq)
data Direction = North | South | East | West deriving (Show, Eq, Ord)
type Coord  = (Integer, Integer)
type TileMap = Map.Map Coord Tile
type Queue = Q.HashPSQ Coord Int ProgramState 

type Path = [Direction]

main = do
  p <- readProgramFile "input.txt"
  let (q, map, movesToTank) = findO2Tank p
  let fullMap = exploreAll q map
  let oxCoord = case Q.findMin q of
                  Nothing -> (0, 0)
                  Just (c, _, _) -> c
  print movesToTank
  print $ timeToFill fullMap oxCoord

toCommand :: Direction -> Integer
toCommand North = 1
toCommand South = 2
toCommand West  = 3
toCommand East  = 4

toTile :: Integer -> Tile
toTile 0 = Wall
toTile 1 = Empty
toTile 2 = OxTank

updateCoord :: Coord -> Direction -> Coord
updateCoord (x, y) d = case d of
  North -> (x, y-1)
  South -> (x, y+1)
  East  -> (x+1, y)
  West  -> (x-1, y)

findO2Tank :: Program -> (Queue, TileMap, Int)
findO2Tank p = exploreUntilFound (startQ, tileMap)
  where
    tileMap = Map.singleton (0, 0) Empty
    state   = startState p []
    stateWithOutput = state {output = [0]}
    startQ  = Q.singleton (0, 0) 0 stateWithOutput
    exploreUntilFound (q, m) = case Q.findMin q of
                          Nothing -> (q, m, -1)
                          Just (_, p, nextState) -> steps
                            where
                              currPos = toTile $ head $ output nextState
                              steps
                                | currPos == OxTank = (q, m, p)
                                | otherwise = exploreUntilFound $ expandNext q m

exploreAll :: Queue -> TileMap -> TileMap
exploreAll q m = exploreUntilFound (q, m)
  where
    exploreUntilFound (q, m) = case Q.findMin q of
                           Nothing -> m
                           Just _ -> exploreUntilFound $ expandNext q m

expandNext :: Queue -> TileMap -> (Queue, TileMap)
expandNext queue map =
  case Q.minView queue of
    Nothing -> (Q.empty, map)
    Just (coord, prio, curr, newQ) -> foldl expand (newQ, map) [North, South, West, East]
      where
        currNoOutput = curr {output = []}
        expand (q, m) d = case m Map.!? nextCoord of
                            Just _  -> (q, m)
                            Nothing -> (insertedQ, updatedM)
          where
            nextState = execUntilInput currNoOutput [toCommand d]
            nextCoord = updateCoord coord d
            out = toTile $ head $ output nextState
            updatedM  = Map.insert nextCoord out m
            insertedQ
              | out /= Wall  = Q.insert nextCoord (prio + 1) nextState q
              | otherwise = q

timeToFill :: TileMap -> Coord -> Int
timeToFill m c = timeToFill' [c] visited (-1)
  where
    visited = Set.singleton c
    timeToFill' [] _ i = i
    timeToFill' cs v i = timeToFill' nextCs newV (i+1)
      where
        newV = foldl (\a b -> Set.insert b a) v nextCs
        nextCs = notWall . notVisited $ concat $ map neighbors cs
        neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
        notWall cs = filter (\a -> (m Map.! a) /= Wall) cs
        notVisited cs = filter (\a -> not $ Set.member a v) cs

