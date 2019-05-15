import Data.HashMap as Map (Map, empty, insertWith, filter)
import Data.List.Split


main=do
  contents <- readFile "input.txt"
  let ls = lines contents
  let cs = map interpret ls

  let ps = map pointsOf cs

  print $ head cs
  print $ length $ Map.filter ((>= 2) . length) $ countPoints ps

interpret :: String -> (Int, [(Int, Int)])
interpret s = (id, coords)
  where
    l = splitOn " " s
    id = toInt . tail $ l !! 0
    coords = toCoords pos size
    toCoords p s = zip p [a+b | (a, b) <- zip p s]
    pos = map (succ . toInt) $ splitOn "," . init $ l !! 2
    size =  map (pred . toInt) $ splitOn "x" $ l !! 3
    toInt s = read s :: Int

pointsOf :: (Int, [(Int, Int)]) -> (Int, [(Int, Int)])
pointsOf (id, claim) = (id, [(x, y) | x <- rangeW, y <- rangeH])
  where
    rangeH = [fst h .. snd h]
    rangeW = [fst w .. snd w]
    h = claim !! 1
    w = claim !! 0

countPoints :: (Int, [(Int, Int)]) -> Map (Int, Int) [Int]
countPoints (id,cs) = countPoints' cs empty
  where
    countPoints' [] mp = mp
    countPoints' (c:cs) mp = countPoints' cs $ insertWith (++) c [id] mp