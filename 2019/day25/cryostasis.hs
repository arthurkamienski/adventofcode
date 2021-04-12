import IntCodeComputer
import qualified Data.Map.Strict as Map
import Data.List.Split (chunksOf)
import Data.List (intercalate)

type Coord = (Int, Int)
type Ship = Map.Map Coord Char
data Dir = N | W | E | S

main = do
  p <- readProgramFile "input.txt"
  let s = run p path
  putStrLn $ convertOutput $ reverse $ output s
  runInt s

allItems = ["wreath", "weather machine", "candy cane", "prime number", "astrolabe", "food ration", "hypercube", "space law space brochure"]

path = ["n", "e", "e", "e", "w", "w", "w", "s", "s", "s", "n", "w", "w", "e", "e", "n", "e", "s", "e", "s", "e", "n"] -- collected manually

runInt :: ProgramState -> IO ()
runInt s = case op s of
  Stop -> putStrLn "Game Over"
  _ -> do
    i <- getLine
    let next = runCommand s i
    print $ parseOutput next
    runInt (next {output = []})

parsed :: String -> String
parsed "e" = "east"
parsed "w" = "west"
parsed "n" = "north"
parsed "s" = "south"
parsed i   = i

run :: Program -> [String] -> ProgramState
run p l = run' ps l
  where
    ps = startState p []
    run' ps [] = ps
    run' ps (i:is) = case op ps of
                       Stop -> ps
                       _ 
                         | room == "== Security Checkpoint ==" -> findCombination erased
                         | otherwise -> run' erased nextInputs
      where
        erased = nextState {output = []}
        nextState = runCommand ps i
        (room, desc, doors, items) = parseOutput nextState
        toGrab = map (\s -> "take " ++ s) $ filter (\e -> elem e allItems) items
        nextInputs = toGrab ++ is

runCommand :: ProgramState -> String -> ProgramState
runCommand ps i = execUntilInput ps (convertInput $ (parsed i) ++ "\n")

parseOutput :: ProgramState -> (String, String, [String], [String])
parseOutput ps = (room, desc, doors, items)
  where
    desc = head $ (filter (\d -> head d /= '=') out) ++ [""]
    room = head $ (filter (\d -> head d == '=') out) ++ [""]
    line1 = head out
    dirs = ["north", "south", "west", "east"]
    doors = filter (\d -> elem d dirs) opts
    items = filter (\d -> not $ elem d dirs) opts
    opts = map (drop 2) $ filter (\s -> head s == '-') out
    ascii = filter (/= "") $ lines $ convertOutput $ reverse $ output ps
    out 
      | head ascii == "== Pressure-Sensitive Floor ==" = drop 5 ascii
      | otherwise = ascii

findCombination :: ProgramState -> ProgramState
findCombination s = find s combs
  where
    combs = allCombs allItems
    find ps (c:cs)
      | r == "== Security Checkpoint ==" = find ps cs
      | otherwise = nextState
      where
        nextState = tryComb ps c
        (r, _, _, _) = parseOutput nextState

tryComb :: ProgramState -> [String] -> ProgramState
tryComb s c = foldl runAll s ((map (\s -> "drop " ++ s) c) ++ ["west"])
  where
    runAll s i = runCommand (s {output = []}) i

allCombs :: [String] -> [[String]]
allCombs is = concat [subsets n is | n <- [1..length is]]
  where
    subsets 0 _ = [[]]
    subsets _ [] = []
    subsets n (x:xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs
