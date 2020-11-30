import IntCodeComputer
import qualified Data.Map.Strict as Map
import Data.List (intercalate)


type Coord = (Int, Int)
data Color = Black | White deriving (Show, Eq)

type Hull  = Map.Map Coord Color

type Direction = Coord
type Robot     = (Coord, Direction)

main = do
  paintProgram   <- readProgramFile "input.txt"
  let hull         = Map.empty
  let initialRobot = ((0,0), (0, -1))
  let painted      = paint hull initialRobot paintProgram
  let whiteFirst   = paintHull hull (0,0) White
  let paintedWhiteFirst = paint whiteFirst initialRobot paintProgram
  
  print $ length painted
  printHull paintedWhiteFirst

parseColor :: Integer -> Color
parseColor 0 = Black
parseColor 1 = White

toIntColor :: Color -> Integer
toIntColor c = case c of
  White -> 1
  Black -> 0

changeDir :: Direction -> Integer -> Direction
changeDir (x, y) 0 = (y, -x)
changeDir (x, y) 1 = (-y, x)

getHullColor :: Hull -> Coord -> Color
getHullColor h c = Map.findWithDefault Black c h

paintHull :: Hull -> Coord -> Color -> Hull
paintHull h pos color = Map.insert pos color h

move :: Coord -> Direction -> Coord
move (cx, cy) (dx, dy) = (cx + dx, cy + dy)

paint :: Hull -> Robot -> Program -> Hull
paint h r p = paintNext h r programState 
  where
    programState = startState p []
    paintNext h r currState = case op currState of
      Stop -> h
      _    -> paintNext newH newR next
      where
        (currPos, currDir) = r
        currC              = getHullColor h currPos
        (newC, newDir, next) = nextState currState currC currDir
        newH = paintHull h currPos newC
        newPos = move currPos newDir
        newR = (newPos, newDir)

nextState :: ProgramState -> Color -> Direction -> (Color, Direction, ProgramState)
nextState ps color dir = (newColor, newDir, nextState {output = []})
  where
    nextState        = execUntilInput ps [toIntColor color]
    [turn, colorInt] = output $ nextState
    newDir           = changeDir dir turn
    newColor         = parseColor colorInt

printHull :: Hull -> IO ()
printHull h = mapM_ print [[showColor (x, y) | x <- [minX..maxX]] | y <- [minY..maxY]]
  where
    showColor coord = toChar $ getHullColor h coord
    toChar c = case c of
                 White -> '#'
                 Black -> ' '
    paintedCoords = Map.keys h
    xs = map fst paintedCoords
    ys = map snd paintedCoords
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys
