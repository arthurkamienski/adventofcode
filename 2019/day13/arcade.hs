import IntCodeComputer
import qualified Data.Map.Strict as Map
import Data.List.Split (chunksOf)

data Tile = Empty | Wall | Block | Paddle | Ball deriving (Show, Eq)

type Coord  = (Integer, Integer)
type TileMap = Map.Map Coord Tile
data GameState = GameState {
tiles :: TileMap,
score :: Integer,
ballPos :: Coord,
oldBallPos :: Coord,
padPos :: Coord} deriving (Show)

main = do
  arcadeProgram <- readProgramFile "input.txt"
  let freeProgram = putQuarter arcadeProgram
  let initState   = initialGameState $ execute arcadeProgram []
  return $ length $ Map.filter (Block==) $ tiles initState

  play freeProgram

updateGameState :: [Integer] -> GameState -> GameState
updateGameState output gs = updateBallPos $ foldl insert gs trios
  where
    trios = chunksOf 3 output
    insert gs [x, y, v]
      | (x, y) == (-1, 0) = gs {score = v}
      | otherwise         = gs {tiles = newMap}
        where
          newMap = Map.insert (x, y) (toTile v) (tiles gs)

updateBallPos :: GameState -> GameState
updateBallPos g = g {oldBallPos = (ballPos g), ballPos = findBallPos g}

updatePadPos :: GameState -> Integer -> GameState
updatePadPos g i = g {padPos = newPadPos $ padPos g}
  where
    newPadPos (x, y) = (x+i, y)

findBallPos :: GameState -> Coord
findBallPos g = head $ Map.keys $ Map.filter (Ball==) $ tiles g

findPadPos :: GameState -> Coord
findPadPos g = head $ Map.keys $ Map.filter (Paddle==) $ tiles g

initialGameState :: [Integer] -> GameState
initialGameState output =  updateGameState output base
  where
    base  = GameState {
    tiles = Map.empty,
    score = 0,
    ballPos = (0, 0),
    oldBallPos = (0, 0),
    padPos = (0, 0)}

toTile :: Integer -> Tile
toTile 0 = Empty
toTile 1 = Wall
toTile 2 = Block
toTile 3 = Paddle
toTile 4 = Ball

printTiles :: GameState -> IO ()
printTiles gs = mapM_ print [[showTile (x, y) | x <- [minX..maxX]] | y <- [minY..maxY]]
  where
    ts = tiles gs
    showTile coord = toChar $ Map.findWithDefault Empty coord ts
    toChar t = case t of
                 Empty  -> ' '
                 Wall   -> '|'
                 Block  -> '#'
                 Paddle -> '-'
                 Ball   -> 'O'
    allCoords = Map.keys ts
    xs = map fst allCoords
    ys = map snd allCoords
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys

play :: Program -> IO ()
play p = playLoop (game {padPos = initPos}) start
  where
    start = execUntilInput (startState p []) [-1]
    game  = initialGameState $ reverse $ output start
    initPos = findPadPos game
    playLoop gs ps = do
      let input = decideInput gs
      
      let state = execUntilInput ps [input]
      let gameState = updateGameState (reverse $ output state) gs
      let nextState = state {output = []}

      let newGame = updatePadPos gameState input

      case op nextState of 
        Stop -> print $ score $ newGame
        _ -> playLoop newGame nextState

decideInput gs = followBall (ballPos gs) (padPos gs)
  where
    followBall (x, _) (px, _)
      | x == px = 0
      | x > px  = 1
      | x < px  = -1

putQuarter :: Program -> Program
putQuarter p = Map.insert 0 2 p
