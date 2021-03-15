import IntCodeComputer
import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn, chunksOf)
import Data.Char (intToDigit, ord)
import Data.String.Utils (replace)

type Coord = (Int, Int)
type Scaffolds = Map.Map Coord Char

main :: IO ()
main = do
  p <- readProgramFile "input.txt"
  let scaffold = scaffoldMap p
  printScaffold scaffold

  let intersections = findIntersections scaffold

  putStrLn ""
  print $ calibrate intersections
  
  print $ collectedDust scaffold p



scaffoldMap :: Program -> Scaffolds
scaffoldMap p = Map.fromList $ toCoords splitString 0
  where
    progOut = output $ execUntilInput (startState p []) []
    chars = map (\i -> toEnum (fromInteger i) :: Char) progOut
    splitString = splitOn "\n" $ reverse chars
    toCoords [] _  = []
    toCoords (r:rs) i = [((i, j), c) | (j, c) <- zip [0..] r] ++ toCoords rs (i+1)

findIntersections :: Scaffolds -> [Coord]
findIntersections s = Map.keys $ Map.filterWithKey isIntersection s
  where
    isIntersection c _ = all ((==) '#') $ neighbors c
    neighbors (x, y) = map findScaffold [(x, y), (x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    findScaffold c = Map.findWithDefault '.' c s

calibrate :: [Coord] -> Int
calibrate is = sum $ map (\(x, y) -> x*y) is

printScaffold :: Scaffolds -> IO ()
printScaffold s = do
  let coords = Map.keys s
  let maxY = maximum $ map snd coords
  let rows = chunksOf (maxY+1) coords
  let chars = map (map (s Map.!)) rows
  putStrLn $ "++" ++ (concat $ map (\i -> show $ (i `div` 10)) [0..maxY])
  putStrLn $ "++" ++ (concat $ map (\i -> show $ (i `mod` 10)) [0..maxY])
  mapM_ (\(i, r) -> putStrLn $ concat [show (i `div` 10), show (i `mod` 10), r]) (zip [0..] chars)

commandSequence :: Scaffolds -> [Char]
commandSequence s = compress $ commandSequence' s robot
  where
    robot = head $ Map.toList $ Map.filter (\c -> elem c ['^', '<', '>', 'v']) s
    commandSequence' s r = case nextCommand r s of
                             (Nothing, _) -> []
                             (Just m, newR)  -> m : commandSequence' s newR

nextCommand :: (Coord, Char) -> Scaffolds -> (Maybe Char, (Coord, Char))
nextCommand (p, dir) s = move
  where
    (x, y) = p
    neighbors = case dir of
                    '^' -> [(x-1, y), (x, y-1), (x, y+1)]
                    'v' -> [(x+1, y), (x, y+1), (x, y-1)]
                    '>' -> [(x, y+1), (x-1, y), (x+1, y)]
                    '<' -> [(x, y-1), (x+1, y), (x-1, y)]
    findTile c = Map.findWithDefault '.' c s
    [fTile, lTile, rTile] = map findTile neighbors
    move
      | fTile == '#' = (Just '1', (head neighbors, dir))
      | lTile == '#' = (Just 'L', (p, rotated 'L'))
      | rTile == '#' = (Just 'R', (p, rotated 'R'))
      | otherwise    = (Nothing, (p, dir))
    rotated move = case (dir, move) of
                     ('^', 'L') -> '<'
                     ('^', 'R') -> '>'
                     ('v', 'L') -> '>'
                     ('v', 'R') -> '<'
                     ('<', 'L') -> 'v'
                     ('<', 'R') -> '^'
                     ('>', 'L') -> '^'
                     ('>', 'R') -> 'v'

threeFunctions :: [Char] -> (Int, Int, Int) -> ([[Char]], [Char])
threeFunctions cs (i, j, k) = ([a, b, c], unassigned)
  where
    a          = take i cs
    restA      = splitOn a cs
    b          = take j $ head (filter ((/=) "") restA)
    restB      = concat $ map (splitOn b) restA
    c          = take k $ head (filter ((/=) "") restB)
    restC      = splitOn c (concat restB)
    unassigned = concat restC

compress :: [Char] -> [Char]
compress [] = []
compress (c:cs)
  | c == '1' = newNum ++ "," ++ compress rest
  | otherwise = c : ',' : compress cs
  where 
    newNum = show $ 1 + (length $ takeWhile ((==) '1') cs)
    rest = dropWhile ((==) '1') cs

toFunctions :: [Char] -> [[Char]]
toFunctions commands = (normalize mainRoutine) : (map normalize fs) 
  where
    combs = [(i, j, k) | i <- [1..20], j <- [1..20], k <- [1..20]]
    fitsCommands c = (snd $ threeFunctions commands c) == ""
    fittingComb = head $ filter fitsCommands combs
    fs@[a, b, c] = fst $ threeFunctions commands fittingComb
    mainRoutine = replace c "C," $ replace b "B," $ replace a "A," commands
    normalize = escape . removeTraillingComma
    escape f = f ++ "\n"
    removeTraillingComma f
      | last f == ',' = init f
      | otherwise = f

collectedDust :: Scaffolds -> Program -> Integer
collectedDust s p = head $ output $ execUntilInput state []
  where
    newP = Map.insert 0 2 p
    state = startState newP progInput
    functions = toFunctions $ commandSequence s
    progInput = concat $ map (map (toInteger.ord)) (functions ++ ["n\n"])
