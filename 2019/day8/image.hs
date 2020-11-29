type Color = Int
type Row   = [Color] 
type Layer = [Row]  
type Image = [Layer]

main = do
  img <- readInputFile "input.txt" 25 6
  let (_, l) = minimum $ countNImg img 0
  let n1     = countNLayer l 1
  let n2     = countNLayer l 2

  print $ n1 * n2
  showImage $ stackImage img

showImage :: Image -> IO ()
showImage img = mapM_ print $ map (merge . (map toChar)) $ head img
  where
    merge    = foldl (++) []
    toChar 1 = "o"
    toChar 0 = " "

readInputFile :: String -> Int -> Int -> IO Image
readInputFile path w h = do
  f <- readFile path
  return $ parseInput f w h

parseInput :: String -> Int -> Int -> Image
parseInput "\n" w h = []
parseInput i    w h = l : (parseInput s w h)
  where
    (l, s) = readLayer i w h

readRow :: String -> Int -> (Row, String)
readRow i w = (row, snd s)
  where
    s = splitAt w i
    row = map (\x -> read [x] :: Int) (fst s)

readLayer :: String -> Int -> Int -> (Layer, String)
readLayer i w 0 = ([], i)
readLayer i w h = (r:l, s2)
  where
    (r, s1) = readRow i w
    (l, s2) = readLayer s1 w (h-1)

countNImg :: Image -> Int -> [(Int, Layer)]
countNImg img n = map layerCount img
  where
    layerCount l = (countNLayer l n, l)

countNLayer :: Layer -> Int -> Int
countNLayer l n = sum $ map (countRow n) l
  where
    countRow n r = length $ filter (n==) r

stackImage :: Image -> Image
stackImage img = [compareLayers img]
  where
    compareLayers img@(l:ls) = case l of
      [] -> []
      _  -> newRow : otherRows
      where
        newRow    = compareRows $ map head img
        otherRows = compareLayers $ map tail img
        compareRows img@(r:rs) = case r of
          []  -> []
          _   -> newColor : otherColors
          where
            newColor    = compareColors $ map head img
            otherColors = compareRows $ map tail img
            compareColors []  = 2
            compareColors row = head $ filter (2/=) row
