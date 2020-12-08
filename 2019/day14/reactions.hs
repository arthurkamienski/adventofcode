import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn)

type Chemical    = String
type Ingredients = Map.Map Chemical Integer
type Recipes     = Map.Map Chemical (Integer, Ingredients)
type Stock       = Ingredients

main = do
  recipes <- readInputFile "input.txt"
  print $ oresFor recipes ("FUEL", 1)
  print $ maxFuelWith recipes 1000000000000

readInputFile :: String -> IO Recipes
readInputFile path = do
  input <- readFile path
  return $ parseInput input

parseInput :: String -> Recipes
parseInput i = foldl addToList Map.empty $ map parseLine $ lines i
  where
    addToList m (k, v) = Map.insert k v m
    parseLine l   = (resChem, (amount, ingList))
      where
        [ings, res]   = splitOn " => " l
        (resChem, amount) = toAmount res
        ingList       = Map.fromList $ map toAmount $ splitOn ", " ings
        toAmount c    = (chem, read amount :: Integer)
          where
            [amount, chem] = splitOn " " c

ingsFor :: Recipes -> (Chemical, Integer) -> (Ingredients, Integer)
ingsFor rs (c, i) = (adjstIngs, leftOver)
  where
    (amount, ings)
      | Map.member c rs = rs Map.! c
      | otherwise = error c
    nReactions = (i `div` amount) + (signum $ i `rem` amount)
    adjstIngs  = Map.filter (0/=) $ Map.map adjst ings
    adjst a = a*nReactions
    leftOver = (nReactions*amount) - i

checkStock :: Recipes -> Stock -> (Chemical, Integer) -> (Ingredients, Stock)
checkStock rs s (c, i) = (newIngs, newStock)
  where
    nInStock = Map.findWithDefault 0 c s
    nRequired = max (i - nInStock) 0
    (newIngs, leftOver) = ingsFor rs (c, nRequired)
    remaining = leftOver + (max (nInStock - i) 0)
    newStock = Map.insert c remaining s

oresFor :: Recipes -> (Chemical, Integer) -> Integer
oresFor reacts (c, i) = oresFor' reacts Map.empty (Map.singleton c i)
  where
    oresFor' reacts stock reqs
      | isOre = reqs Map.! "ORE"
      | otherwise = oresFor' reacts newStock newReqs
      where
        isOre = ((length $ Map.keys reqs) == 1) && (Map.member "ORE" reqs)
        (newStock, newReqs) = Map.foldlWithKey produce (stock, Map.empty) reqs
        produce (s, rs) c i = case c of
          "ORE" -> (s, Map.insertWith (+) c i rs)
          _     -> (ns, nrs)
          where
            nrs = Map.unionWith (+) rs others
            (others, ns) = checkStock reacts s (c, i)

maxFuelWith :: Recipes -> Integer -> Integer
maxFuelWith rs i = tryFuelAmount (toInteger 1) (maxAmount 1)
  where
    maxAmount c
      | c >= i    = c
      | otherwise = maxAmount (c*2)
    tryFuelAmount minFuel maxFuel
      | minFuel == (maxFuel - 1) = minFuel
      | otherwise = tryFuelAmount newMin newMax
      where
        (newMin, newMax)
          | reqOres > i = (minFuel, midPoint)
          | otherwise   = (midPoint, maxFuel)
          where
            midPoint = (maxFuel + minFuel) `div` 2
            reqOres = oresFor rs ("FUEL", midPoint)
