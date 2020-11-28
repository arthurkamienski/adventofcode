import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn)
import Data.List (sortBy, group, sort)

data Dir      = R | L | U | D deriving (Show, Read)

type Coord     = (Int, Int)
type WireNum   = Int
type Steps     = Int
type WireSteps = Map.Map WireNum Steps
type Grid      = Map.Map Coord WireSteps
type Instr     = (Dir, Int)
type WirePath  = (WireNum, [Instr])
type PathState = (Grid, WireNum, Coord, Steps)

main = do
  paths <- readInput "input.txt"
  let grid = plot paths
  print $ minInterceptDist grid
  print $ minInterceptSteps grid

minInterceptDist :: Grid -> Int
minInterceptDist g = minDist . interceptCoords $ g

minInterceptSteps :: Grid -> Int
minInterceptSteps g = minimum . interceptSteps $ g

interceptSteps :: Grid -> [Int]
interceptSteps g = map steps $ Map.elems . intercepts $ g
  where
    steps ws = sum $ Map.elems ws

interceptCoords :: Grid -> [Coord]
interceptCoords g = Map.keys . intercepts $ g

intercepts :: Grid -> Grid
intercepts g = Map.filter isIntercept g
  where
    isIntercept v = (length $ Map.keys v) > 1

parseInput :: String -> [WirePath]
parseInput i =  map parsePath $ zip [0..] $ lines i
  where
    parsePath (w, line) = (w, path)
      where
        path      = map toInstr $ splitOn "," line
        toInstr s = (dir, moves)
          where
            dir   = read [head s] :: Dir
            moves = read (tail s) :: Int

readInput :: String -> IO [WirePath]
readInput p = do
  input <- readFile p
  return $ parseInput input

plot :: [WirePath] -> Grid
plot ps = foldl plotPath Map.empty ps

plotPath :: Grid -> WirePath -> Grid
plotPath g (w, p) = final
  where
    (final, _, _, _) = foldl moveWire (g, w, (0,0), 0) p

moveWire :: PathState -> Instr -> PathState
moveWire ps (d, 0) = ps
moveWire ps (d, i) = moveWire newState (d, i-1)
  where
    (g, w, c, s) = ps
    newState = (newG, w, newC, s+1)
      where
        newC = moveCoord d c
        newG = insertWire (g, w, newC, s+1)
        newS = (newG Map.! c) Map.! w

insertWire :: PathState -> Grid
insertWire (g, w, c, s) = Map.insertWith revUnion c newMap g
  where
    newMap = Map.singleton w s
    revUnion a b = Map.union b a

moveCoord :: Dir -> Coord -> Coord
moveCoord R (x, y) = (x+1, y)
moveCoord L (x, y) = (x-1, y)
moveCoord U (x, y) = (x  , y+1)
moveCoord D (x, y) = (x  , y-1)



minDist :: [Coord] -> Int
minDist cs = minimum $ map dist cs
  where
    dist (x, y) = abs x + abs y

printGrid g = mapM_ printLine allCoords
  where
    printLine cs = do
      mapM_ printCoord cs
      putStrLn ""
    printCoord c
      | c == (0, 0)   = putStr "o"
      | length w > 1  = putStr "X"
      | length w == 1 = putStr "."
      | otherwise     = putStr " "
        where
          w = Map.findWithDefault [] c g
    allCoords = [[(x, y) | x <- [minX..maxX]] | y <- [maxY,maxY-1..minY]]
      where
        (minX, maxX) = boundaries xs
        (minY, maxY) = boundaries ys
        xs = uniqCoords fst
        ys = uniqCoords snd
        uniqCoords f = map head $ group $ sort $ map (\c -> f c) $ Map.keys g
        boundaries cs = (minimum cs - 1, maximum cs + 2)


