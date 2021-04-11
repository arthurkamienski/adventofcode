import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List.Split (chunksOf)

type Past = Set.Set Integer
type Coord = (Int, Int)
type Layout = Map.Map Coord Char
type RecursLayout = Map.Map RecursCoord Char
type RecursCoord = (Coord, Int)

main :: IO ()
main = do
  p <- readFile "input.txt"
  let l = parseInput p

  print $ findRepeated l

  let rl = parseInputRecurs p

  print $ countBugs $ iterate nextRecursLayout rl !! 200

countBugs :: RecursLayout -> Int
countBugs rl = length $ Map.filter (== '#') rl

findRepeated :: Layout -> Integer
findRepeated l = findRepeated' l Set.empty
  where
    findRepeated' l s
      | Set.member b s = b
      | otherwise = findRepeated' nextL (Set.insert b s)
      where
        nextL = nextLayout l
        b = biodiversity l

parseInput :: String -> Layout
parseInput s = Map.fromList [((i, j), c) | (j, l) <- zip [0..] (lines s), (i, c) <- zip [0..] l]

parseInputRecurs :: String -> RecursLayout
parseInputRecurs s = foldl addNeighbs init bugs
  where
    init = Map.filterWithKey hasBug rl
    hasBug ((2, 2), _) v = False
    hasBug k v = any (== '#') $ v : neighborCharsRecurs rl k
    rl = Map.fromList [(((i, j), 0), c) | (j, l) <- zip [0..] (lines s), (i, c) <- zip [0..] l]
    bugs = Map.keys $ Map.filter (== '#') init

biodiversity :: Layout -> Integer
biodiversity l = sum $ Map.keys $ Map.mapKeys tileValue $ Map.filter (== '#') l
  where
    tileValue (i, j) = 2^(j*5+i)

nextLayout :: Layout -> Layout
nextLayout l = Map.mapWithKey (nextState l) l

nextState :: Layout -> Coord -> Char -> Char
nextState l (i, j) curr
  | curr == '.' && (bugs == 1 || bugs == 2) = '#'
  | curr == '#' && bugs /= 1 = '.'
  | otherwise  = curr
    where
      bugs = length $ filter (== '#') neigbs
      neigbs = map get [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]
      get c = Map.findWithDefault '.' c l

nextRecursLayout :: RecursLayout -> RecursLayout
nextRecursLayout rl = newLayout
  where
    updated = Map.mapWithKey (nextStateRecurs rl) rl
    bugs = Map.keys $ Map.filter (== '#') updated
    newLayout = foldl addNeighbs updated bugs

neighborsRecurs :: RecursLayout -> RecursCoord -> [RecursCoord]
neighborsRecurs rl (c@(i,j), d) = concat $ map get [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]
  where
    get c1@(x, y)
      | c1 == (2, 2) = map (\c2 -> (c2, d+1)) innerNeighbs
      | x > 4        = [((3, 2), d-1)]
      | x < 0        = [((1, 2), d-1)]
      | y > 4        = [((2, 3), d-1)]
      | y < 0        = [((2, 1), d-1)]
      | otherwise    = [(c1, d)]
    innerNeighbs
      | c == (2, 1) = [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]
      | c == (2, 3) = [(0, 4), (1, 4), (2, 4), (3, 4), (4, 4)]
      | c == (1, 2) = [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4)]
      | c == (3, 2) = [(4, 0), (4, 1), (4, 2), (4, 3), (4, 4)]

addNeighbs :: RecursLayout -> RecursCoord -> RecursLayout
addNeighbs rl c = foldl (\m k -> Map.insert k '.' m) rl notInMap
  where
    notInMap = filter (\k -> not $ Map.member k rl) ns
    ns = neighborsRecurs rl c

neighborCharsRecurs :: RecursLayout -> RecursCoord -> [Char]
neighborCharsRecurs rl a = map getDefault $ neighborsRecurs rl a
  where
    getDefault c = Map.findWithDefault '.' c rl

nextStateRecurs :: RecursLayout -> RecursCoord -> Char -> Char
nextStateRecurs _ ((2, 2), _) curr = curr
nextStateRecurs rl c curr
  | curr == '.' && (bugs == 1 || bugs == 2) = '#'
  | curr == '#' && bugs /= 1 = '.'
  | otherwise  = curr
  where
    bugs = length $ filter (== '#') neigbs
    neigbs = neighborCharsRecurs rl c

printLayout :: Layout -> IO ()
printLayout l = do
  let cs = [(i, j) | j <- [0..4], i <- [0..4]]
  let chars = chunksOf 5 $ map (l Map.!) cs
  mapM_ putStrLn chars

printRecursLayout :: RecursLayout -> IO ()
printRecursLayout rl = do
  let maxLayer = maximum $ map snd $ Map.keys rl
  let minLayer = minimum $ map snd $ Map.keys rl

  let nLayers = [minLayer..maxLayer]
  let cs = [((i, j), k) | k <- nLayers, j <- [0..4], i <- [0..4]]

  let chars = chunksOf 5 $ map (\c -> Map.findWithDefault '.' c rl) cs
  let layers = chunksOf 5 chars

  mapM_ (\(l, i) -> putStrLn ("Depth: " ++ i) >> (mapM_ putStrLn l) >> putStrLn "") (zip layers (map show nLayers))
