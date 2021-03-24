import qualified Data.Map.Strict as Map
import qualified Data.HashPSQ as Q
import qualified Data.Set as Set
import Data.List.Split (chunksOf)
import Data.List (sort)

type Coord = (Int, Int)
type Maze  = Map.Map Coord Char
type Graph = Map.Map Coord [(Coord, Int)]
type Queue = Q.HashPSQ (Coord, Int) Int ([(String, Int)], Coord)
type Portals = Map.Map String [Coord]

main = do
  c <- readFile "input.txt"
  let m = readMap c
  let simplified = fillCrossroads $ closeDeadEnds m

  printMap simplified
  putStrLn ""
  print $ movesToEnd simplified
  print $ movesToEndRecurs simplified

portalName :: Maze -> Coord -> String
portalName m c = sort [m Map.! c, head $ Map.elems $ portals] 
  where
    portals = Map.filter (\x -> elem x ['A'..'Z']) ns
    ns = neighbors m c

isPortal :: Maze -> Coord -> Bool
isPortal m c = portal && onePath && onePortal
  where
    portal = elem (m Map.! c) ['A'..'Z']
    onePath = (Map.size paths) == 1
    onePortal = (Map.size portals) == 1
    paths = Map.filter ('.' ==) ns
    portals = Map.filter (\x -> elem x ['A'..'Z']) ns
    ns = neighbors m c

filterType :: [Char] -> Maze -> Maze 
filterType s m = Map.filter (\c -> elem c s) m

neighbors :: Maze -> Coord -> Maze
neighbors m (x, y) = Map.restrictKeys m coords
  where
    coords = Set.fromList [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

notWalls :: Maze -> Maze
notWalls m = Map.filter ('#' /=) m

isDeadEnd :: Maze -> Coord -> Char -> Bool
isDeadEnd m coord c = isNotWall && hasOnePath
  where
    isNotWall = c /= '#'
    hasOnePath = (Map.size $ filterType "#" $ neighbors m coord) == 3

closeDeadEnds :: Maze -> Maze
closeDeadEnds m
  | (length emptyEnds) == 0 = m
  | otherwise = closeDeadEnds $ foldl fillDeadEnd m emptyEnds
  where
    fillDeadEnd m c = Map.insert c '#' m
    isEmptyEnd coord c = (c == '.') && isDeadEnd m coord c
    emptyEnds = Map.keys $ Map.filterWithKey isEmptyEnd m

fillCrossroads :: Maze -> Maze
fillCrossroads m = foldl fill m crossRoads
  where
    fill m c = Map.insert c '+' m
    isCrossRoad coord c = (c == '.') && ((Map.size $ notWalls $ neighbors m coord) >= 3)
    ns c = neighbors c
    crossRoads = Map.keys $ Map.filterWithKey isCrossRoad m

readMap :: String -> Maze
readMap s = Map.fromList [((j, i), c) |
  (j, r) <- zip [0..] (lines s),
  (i, c) <- zip [0..] r,
  c /= ' ']

portalMap :: Maze -> Portals
portalMap m = foldl insertName Map.empty portalNames
  where
    portals = Map.keys $ Map.filterWithKey (\k _ -> isPortal m k) m
    portalNames = map (\c -> (portalName m c, c)) portals
    insertName m (n, c) = Map.insert n newCoords m
      where
        newCoords = c : Map.findWithDefault [] n m

movesToEnd :: Maze -> Int
movesToEnd m = moves m False

movesToEndRecurs :: Maze -> Int
movesToEndRecurs m = moves m True

moves :: Maze -> Bool -> Int
moves m recur = findDistance startQ
  where
    a q = expandNext q m portals
    portals = portalMap m
    startPortal = head $ portals Map.! "AA"
    start = fst $ head $ mazeNeighbors m 0 portals startPortal recur
    startQ = Q.singleton (start, 0) 0 ([], start)
    findDistance q = case Q.findMin q of
                       Nothing -> -1
                       Just ((_, lvl), s, ((("ZZ", _):ps), _))
                         | not recur -> s-1
                         | lvl == 0  -> s-1
                         | otherwise -> findDistance $ expandNext q m portals recur
                       Just _ -> findDistance $ expandNext q m portals recur

mazeNeighbors :: Maze -> Int -> Portals -> Coord -> Bool -> [(Coord, Char)]
mazeNeighbors m i p c r
  | portal && (length others == 1) && isOpen = (nextPortal) : selfNs
  | otherwise = selfNs
    where
      isOpen = (not r) || (not $ (isOuter c m) && (i == 0))
      portal = isPortal m c
      name = portalName m c
      others = filter (c /=) $ Map.findWithDefault [] name p
      nextPortal = (head $ others, m Map.! (head $ others))
      selfNs = Map.toList $ notWalls $ neighbors m c

isOuter :: Coord -> Maze -> Bool
isOuter c@(x, y) m = (x == 1) || (y == 1) || (x == (maxX-1)) || (y == (maxY-1))
  where
    coords = Map.keys m
    maxY    = maximum $ map snd coords
    maxX    = maximum $ map fst coords

portalLevel :: Coord -> Int -> Maze -> Int
portalLevel c@(x, y) i m
  | isOuter c m  = i+1
  | otherwise = i-1

expandNext :: Queue -> Maze -> Portals -> Bool -> Queue
expandNext queue m p recur = case Q.minView queue of
    Nothing -> queue
    Just ((coord, lvl), prio, (ps, prev), newQ) -> foldl expand newQ ns
      where
        ns = mazeNeighbors m lvl p coord recur
        expand q (c, s)
          | c == prev = q
          | otherwise = nextQ
          where
            currPortal = isPortal m c
            prevPortal = isPortal m coord
            newLvl
              | currPortal && prevPortal = portalLevel c lvl m
              | otherwise = lvl
            (newPs, newPrio)
              | currPortal = (((portalName m c), newLvl) : ps, prio)
              | otherwise = (ps, prio+1)
            insertedQ = Q.insert (c, newLvl) newPrio (newPs, coord) q
            nextQ = case Q.lookup (c, newLvl) q of
              Just (prio, v)
                | prio < prio+1 -> q
                | otherwise     -> insertedQ
              Nothing -> insertedQ

printMap :: Maze -> IO()
printMap m = do
  let coords = Map.keys m
  let maxY   = maximum $ map snd coords
  let maxX   = maximum $ map fst coords
  let all    = [(x, y) | x <- [0..maxX], y <- [0..maxY]]
  let chars  = map (\c -> Map.findWithDefault ' ' c m) all
  let rows   = chunksOf (maxY+1) chars
  mapM_ putStrLn rows
