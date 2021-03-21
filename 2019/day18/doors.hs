import qualified Data.Map.Strict as Map
import qualified Data.HashPSQ as Q
import qualified Data.Set as Set
import Data.Char (toUpper, toLower)
import Data.List (sort)
import Data.List.Split (chunksOf)

type Coord = (Int, Int)
type VaultMap = Map.Map Coord Char
type Doors = [Char]
type KeysCoords  = Set.Set Coord
type Keys = [Char]
type DistanceQueue = Q.HashPSQ Coord (Int, Int) (Doors, Keys, Set.Set Coord)
type KeyDistances = Map.Map (Coord, Coord) (Int, Doors, Keys)
type PathsQueue = Q.HashPSQ (Coord, Doors) Int (Set.Set Coord)
type Graph = Map.Map Coord [(Coord, Int)]

main = do
  c <- readFile "input.txt"
  let vm = readMap c
  let simplified = fillCrossroads $ closeDeadEnds vm
  print $ findShortestPath simplified
  print $ findShortestPathRobots simplified


connectMissing :: VaultMap -> Graph -> Graph
connectMissing vm g = foldl insertMissing g missing
  where
    missing = Map.keysSet $ notWalls vm
    insertMissing m c = Map.insert c (ns ++ prevList) m
      where
        ns = [(n, 1) | n <- Map.keys $ notWalls $ neighbors vm c]
        prevList = Map.findWithDefault [] c g

readMap :: String -> VaultMap
readMap s = Map.fromList [((j, i), c) | (j, r) <- zip [0..] (lines s), (i, c) <- zip [0..] r]

filterType :: [Char] -> VaultMap -> VaultMap
filterType s vm = Map.filter (\c -> elem c s) vm

notWalls :: VaultMap -> VaultMap
notWalls vm = Map.filter ('#' /=) vm

currentPos :: VaultMap -> Coord
currentPos vm = head $ Map.keys $ filterType "@" vm

keysPos :: VaultMap -> KeysCoords
keysPos vm = Set.fromList $ Map.keys $ filterType ['a'..'z'] vm

splitMap :: VaultMap -> [VaultMap]
splitMap vm = [q1, q2, q3, q4]
  where
    q1 = Map.filterWithKey (\(i, j) _ -> (i <= x) && (j <= y)) withRobots
    q2 = Map.filterWithKey (\(i, j) _ -> (i >= x) && (j <= y)) withRobots
    q3 = Map.filterWithKey (\(i, j) _ -> (i <= x) && (j >= y)) withRobots
    q4 = Map.filterWithKey (\(i, j) _ -> (i >= x) && (j >= y)) withRobots
    withWalls = foldl (\vm c -> Map.insert c '#' vm) vm (cp : ns)
    withRobots = foldl (\vm c -> Map.insert c '@' vm) withWalls robots
    cp@(x, y) = currentPos vm
    ns = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    robots = [(x+1, y+1), (x-1, y+1), (x+1, y-1), (x-1, y-1)]

neighbors :: VaultMap -> Coord -> VaultMap
neighbors vm (x, y) = Map.restrictKeys vm coords
  where
    coords = Set.fromList [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

isDeadEnd :: VaultMap -> Coord -> Char -> Bool
isDeadEnd vm coord c = isNotWall && hasOnePath
  where
    isNotWall = c /= '#'
    hasOnePath = (Map.size $ filterType "#" $ neighbors vm coord) == 3

closeDeadEnds :: VaultMap -> VaultMap
closeDeadEnds vm
  | (length emptyEnds) == 0 = vm
  | otherwise = closeDeadEnds $ foldl fillDeadEnd vm emptyEnds
  where
    fillDeadEnd vm c = Map.insert c '#' vm
    isEmptyEnd coord c = (elem c ('.' : ['A'..'Z'])) && isDeadEnd vm coord c
    emptyEnds = Map.keys $ Map.filterWithKey isEmptyEnd vm

fillCrossroads :: VaultMap -> VaultMap
fillCrossroads vm = foldl fill vm crossRoads
  where
    fill vm c = Map.insert c '+' vm
    isCrossRoad coord c = (c == '.') && ((Map.size $ notWalls $ neighbors vm coord) >= 3)
    ns c = neighbors c
    crossRoads = Map.keys $ Map.filterWithKey isCrossRoad vm

toGraph :: VaultMap -> Graph
toGraph vm = toGraph' (vm, Map.empty)
  where
    toGraph' t@(vm, g)
      | (length deadEnds) == 0 = connectMissing vm g
      | otherwise = toGraph' $ foldl closeBranch t deadEnds
       where
         deadEnds = Map.keys $ Map.filterWithKey (isDeadEnd vm) vm

closeBranch :: (VaultMap, Graph) -> Coord -> (VaultMap, Graph)
closeBranch (m, g) coord = closeBranch' m coord 0
  where
    closeBranch' vm c i
      | (length $ notWalls ns) == 0 = (nextVM, g)
      | (length emptyNeighbors) == 0 = (nextVM, newG)
      | otherwise = closeBranch' nextVM nextC (i + 1)
      where
        ns = neighbors vm c
        emptyNeighbors = filterType "." ns
        nextVM = Map.insert c '#' vm
        nextC = fst $ head $ Map.toList $ notWalls ns
        newG = insertInGraph coord nextC (i+1) $ insertInGraph nextC coord (i+1) g
        insertInGraph c1 c2 i g = Map.insert c1 ((c2, i) : prevList) g
          where
            prevList = Map.findWithDefault [] c1 g

distanceBetween :: VaultMap -> Graph -> Coord -> Coord -> ((Coord, Coord), (Int, Doors, Keys))
distanceBetween vm g c1 c2 = findDistance $ startQ
  where
    a q = expandNext q c2 g vm
    startQ = Q.singleton c1 (0, 0) ([], [], Set.fromList [c1])
    findDistance q = case Q.findMin q of
                       Nothing -> ((c1, c2), (-1, [], []))
                       Just (c, (p, d), (ds, ks, _))
                         | c == c2   -> ((c1, c2), (p-d, ds, keys ks))
                         | otherwise -> findDistance $ expandNext q c2 g vm
                        where
                          keys [] = []
                          keys ks = tail ks

expandNext :: DistanceQueue -> Coord -> Graph -> VaultMap -> DistanceQueue
expandNext queue dest g vm = case Q.minView queue of
    Nothing -> queue
    Just (coord, (p, d), (doors, keys, prev), newQ) -> foldl expand newQ $ g Map.! coord
      where
        expand q (c, i)
          | Set.member c prev = q
          | otherwise = nextQ
          where
            tile = vm Map.! c
            newKeys
              | elem tile ['a'..'z'] = tile : keys
              | otherwise = keys
            newDoors
              | elem tile ['A'..'Z'] = tile : doors
              | otherwise = doors
            nextPrio = (p + i) + dist
            dist = (manhattan c dest)
            newPrev = Set.insert coord prev
            manhattan (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))
            insertedQ = Q.insert c (nextPrio, d+dist) (newDoors, newKeys, newPrev) q
            nextQ = case Q.lookup c q of
              Just (t@(prio, _), v)
                | prio < nextPrio -> Q.insert c t (newDoors, newKeys, newPrev) q
                | otherwise   -> insertedQ
              Nothing -> insertedQ

keyDists :: VaultMap -> KeyDistances
keyDists vm = Map.fromList allCombs
  where
    g = toGraph vm
    combs = [distanceBetween vm g c1 c2 | (c1, c2) <- keyCombs keys]
    allCombs = concat [[((c1, c2), v), ((c2, c1), v)] | ((c1, c2), v) <- combs]
    keys = currPos : (Set.toList $ keysPos vm)
    currPos = currentPos vm
    keyCombs [] = []
    keyCombs (x:xs) = map ((,) x) xs ++ keyCombs xs

keyChoices :: Coord -> KeyDistances -> Doors -> KeysCoords -> [(Coord, Int)]
keyChoices curr dists doors keys = [(c, i) | (c, (i, _, _)) <- inReach]
  where
    keyDists = map (\c -> (c, dists Map.! (curr, c))) (Set.toList keys)
    hasAll l1 l2 = Set.null $ (Set.fromList l1) Set.\\ (Set.fromList l2)
    openDoors (_, (_, ds, _)) = hasAll ds doors
    hasKey (_, (_, _, ks)) = hasAll ks (map toLower doors)
    inReach = filter (\c -> (openDoors c) && (hasKey c)) keyDists

reachable :: VaultMap -> KeyDistances -> Set.Set Coord
reachable vm dists = Set.fromList inReach
  where
    cp = currentPos vm
    kps = Set.toList $ keysPos vm
    kds = map (\c -> (c, dists Map.! (cp, c))) kps
    keys = Set.fromList $ Map.elems $ filterType ['a'..'z'] vm
    hasAll ds = Set.null $ (Set.fromList ds) Set.\\ keys
    hasKey (_, (_, ds, _)) = hasAll (map toLower ds)
    inReach = map fst $ filter hasKey kds

shortestPathAllKeys :: (VaultMap, KeyDistances) -> (Int, Doors, Coord)
shortestPathAllKeys (vm, dists)
  | Set.null keys = (0, [], currPos)
  | otherwise = getAllKeys startQ
  where
    startQ  = Q.singleton (currPos, []) 0 keys
    currPos = currentPos vm
    keys = reachable vm dists
    getAllKeys q = case Q.findMin q of
                     Nothing -> (-1, [], currPos)
                     Just ((c, ds), p, ks)
                       | Set.null ks -> (p, ds, c)
                       | otherwise -> getAllKeys $ expandChoices vm q dists

expandChoices :: VaultMap -> PathsQueue -> KeyDistances -> PathsQueue
expandChoices vm queue dists = case Q.minView queue of
                       Nothing -> queue
                       Just ((c, ds), p, ms, newQ) -> resQueue
                         where
                           resQueue = foldl insertChoice newQ keysInReach
                           keysInReach = keyChoices c dists ds ms
                           insertChoice q (coord, i) = insertedQ
                             where
                               missing = Set.delete coord ms
                               door    = toUpper $ vm Map.! coord
                               openDoors = door : ds
                               newPrio = (p + i)
                               key = (coord, sort openDoors)
                               insertedQ = case Q.lookup key q of
                                             Just (prio, v)
                                              | prio < (p + i) -> q
                                              | otherwise   -> Q.insert key newPrio missing q
                                             Nothing -> Q.insert key newPrio missing q

findShortestPath :: VaultMap -> Int
findShortestPath vm = i
  where
    (i, _, _) = shortestPathAllKeys (vm, keyDists vm)

findShortestPathRobots :: VaultMap -> Int
findShortestPathRobots vm = shortest chambers 0
  where
    chambers = splitMap vm
    shortest ms p
      | all (\(c, _, _) -> c == 0) keysFound = p
      | otherwise = shortest newMaps (p + s)
      where
        cds = [(m, keyDists m) | m <- ms]
        keysFound = map shortestPathAllKeys cds
        doors = concat $ map (\(_, ds, _) -> ds) keysFound
        coords = map (\(_, _, c) -> c) keysFound
        newMaps = [updateMap m c doors | (m, c) <- zip ms coords]
        s = sum (map (\(c, _, _) -> c) keysFound)
        

updateMap :: VaultMap -> Coord -> Doors -> VaultMap
updateMap vm pos ds =  Map.insert pos '@' $ Map.insert cp '.' updated
  where
    updated = foldl (\m c -> Map.insert c '.' m) vm (keys ++ doors)
    cp = currentPos vm
    keys  = Map.keys $ filterType (map toLower ds) vm
    doors = Map.keys $ filterType ds vm

printMap :: VaultMap -> IO()
printMap vm = do
  let coords = Map.keys vm
  let maxY = maximum $ map snd coords
  let minY = minimum $ map snd coords
  let rows = chunksOf (maxY-minY+1) coords
  let chars = map (map (vm Map.!)) rows
  mapM_ putStrLn chars
