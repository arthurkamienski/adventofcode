import Data.List (group)

type Password = Integer

main = do
  let passRange = [178416..676461+1]
  let diffPasses = length $ filter testPass passRange
  let diffPassesNoGroups = length $ filter testPassNoGroup passRange
  print diffPasses
  print diffPassesNoGroups

testPass :: Password -> Bool
testPass p = doubleDigits p && (not $ decreases p)

testPassNoGroup :: Password -> Bool
testPassNoGroup p = doubleDigitsNoGroup p && (not $ decreases p)

doubleDigitsNoGroup :: Password -> Bool
doubleDigitsNoGroup p = any (\g -> length g == 2) $ group pStr
  where
    pStr  = show p

doubleDigits :: Password -> Bool
doubleDigits p = any (\(a, b) -> a == b) pairs
  where
    pStr  = show p
    pairs = zip pStr $ tail pStr

decreases :: Password -> Bool
decreases p = any (\(a, b) -> a > b) pairs
  where
    pStr  = show p
    pairs = zip pStr $ tail pStr
