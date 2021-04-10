import qualified Data.Map.Strict as Map
import Data.Bits

data Op = Stack | Cut | Deal deriving (Show)

main = do
  input <- readFile "input.txt"
  let cs = parseInput input
  
  let n = 10007

  let f = converge cs n (1, 0)

  print $ shuffle n 2019 f

  let cards = 119315717514047
  let times = 101741582076661
  
  let f = converge cs cards (1, 0)
  let fk = kTimes times cards f

  print $ invShuffle cards 2020 fk

parseInput :: String -> [(Op, Integer)]
parseInput s = parse ls
  where
    ls = lines s
    parse [f]    = [toFunction f]
    parse (f:fs) = toFunction f : parse fs
    toFunction f
      | take 6 f == "deal w" = (Deal, read (drop 20 f) :: Integer)
      | take 6 f == "deal i" = (Stack, -1)
      | otherwise            = (Cut, read (drop 4 f) :: Integer)

stack :: Integer -> (Integer, Integer) -> (Integer, Integer)
stack  n (a, b) = (-a, (-b + n - 1) `mod` n)
cut :: Integer -> Integer -> (Integer, Integer) -> (Integer, Integer)
cut  i n (a, b) = (a, (b-i) `mod` n)
deal :: Integer -> Integer -> (Integer, Integer) -> (Integer, Integer)
deal i n (a, b) = ((i*a) `mod` n, (i*b) `mod` n)

shuffle :: Integer -> Integer -> (Integer, Integer) -> Integer
shuffle n p (a, b) = ((p*a) + b) `mod` n

invShuffle :: Integer -> Integer -> (Integer, Integer) -> Integer
invShuffle n p (a, b) = (((p - b) `mod` n) * (modExp a (n-2) n)) `mod` n

converge :: [(Op, Integer)] -> Integer -> (Integer, Integer) -> (Integer, Integer)
converge [] _ t = t
converge ((Deal , i):ps) n t = converge ps n (deal i n t)
converge ((Cut  , i):ps) n t = converge ps n (cut i n t)
converge ((Stack, i):ps) n t = converge ps n (stack n t)

kTimes :: Integer -> Integer -> (Integer, Integer) -> (Integer, Integer)
kTimes k n (a, b) = (ak, b * (1-ak) * (modExp (1 - a) (n-2) n))
  where
    ak = modExp a k n

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1
