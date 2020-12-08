import IntCodeComputer
import qualified Data.Map.Strict as Map

data Tile = Empty | Wall | OxTank | Unexplored | Robot deriving (Show)
data Direction = North | South | East | West deriving (Show)
type Coord  = (Integer, Integer)
type TileMap = Map.Map Coord Tile
data State = State {pos :: Coord, tiles :: TileMap} deriving (Show)

main = do
  p <- readProgramFile "input.txt"
  tilemap <- explore p
  print $ tilemap

toCommand :: Direction -> Integer
toCommand North = 1
toCommand South = 2
toCommand West  = 3
toCommand East  = 4

toDir :: Integer -> Direction
toDir 1 = North
toDir 2 = South
toDir 3 = West
toDir 4 = East

oppositeDir :: Direction -> Direction
oppositeDir South = North
oppositeDir West = East
oppositeDir East = West
oppositeDir North = South

explore :: Program -> IO TileMap
explore p = exploreLoop progState robotState
  where
    robotState = State (0, 0) (Map.singleton (0, 0) Empty)
    progState = startState p []
    exploreLoop s r = do
      let i = getInput r

      let state = execUntilInput s [toCommand i]
      let out = head $ output state
      let robotState = changeState r i out
      let nextState = case out of
          0 -> state {output = []}
          _ -> backState {output = []}
            where
              backState = execUntilInput state [toCommand $ oppositeDir i]

      printRoom robotState

      case out of
        2 -> return $ tiles r
        _ -> exploreLoop nextState robotState

decideInput :: State -> IO Direction
decideInput r = do
  l <- getLine
  let value = read l :: Int
  return $ toDir $ toInteger value

getInput :: State -> Direction
getInput r = head $ filter isUnexplored [North, South, West, East]
  where
    nextPos d = dirToPos (pos r) d
    isUnexplored d = Map.member () (tiles r)
    unexplored = head $ filter isUnexplored [North, South, West, East]
    dir
      | unexplored == [] = notWall
      | otherwise = unexplored

printRoom :: State -> IO ()
printRoom s = mapM_ print [[showTile (x, y) | x <- [minX..maxX]] | y <- [minY..maxY]]
  where
    ts = tiles s
    withBot = Map.insert (pos s) Robot ts
    showTile coord = toChar $ Map.findWithDefault Unexplored coord withBot
    toChar t = case t of
                 Robot -> 'O'
                 Unexplored -> ' '
                 Empty  -> '.'
                 Wall   -> '#'
                 OxTank -> '%'
    allCoords = Map.keys withBot
    xs = map fst allCoords
    ys = map snd allCoords
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys

dirToPos :: Coord -> Direction -> Coord
dirToPos (x, y) d = case d of
  North -> (x, y-1)
  South -> (x, y+1)
  East  -> (x+1, y)
  West  -> (x-1, y)

changeState :: State -> Direction -> Integer -> State
changeState r d o = case o of
  0 -> r {tiles = insert Wall}
  1 -> r {pos = targetPos, tiles = insert Empty}
  2 -> r {pos = targetPos, tiles = insert OxTank}
  where
    targetPos = dirToPos (pos r) d
    insert t = Map.insert targetPos t (tiles r)
