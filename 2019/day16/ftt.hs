main = do
  let inputDigits = digits input
  print $ nPhases phase inputDigits 100
  let newInput = repeatOffset inputDigits
  print $ nPhases phaseOffset newInput 100

posPattern :: Int -> [Integer]
posPattern pos = posPattern' pattern
  where
    posPattern' [] = []
    posPattern' (i:is) = take pos (repeat i) ++ posPattern' is

digits :: Integer -> [Integer]
digits 0 = []
digits i = (digits $ i `div` 10) ++ [i `mod` 10]

toString :: [Integer] -> String 
toString [] = ""
toString (a:as) = show a ++ toString as

phase :: [Integer] -> [Integer]
phase input = makeOutput 1
  where
    makeOutput pos
      | pos > (length input) = []
      | otherwise = positive `mod` 10 : makeOutput (pos + 1)
       where
         patternPos = tail $ cycle $ posPattern pos
         zipped = zip input patternPos
         summed = sum [a*b | (a, b) <- zipped]
         positive = summed * (signum summed)

phaseOffset :: [Integer] -> [Integer]
phaseOffset input = reverse (makeOutput 0 rev)
  where
    rev = reverse input
    makeOutput _ [] = []
    makeOutput prev (a:as) = this : (makeOutput this as)
      where
        this = (prev + a) `mod` 10

repeatOffset :: [Integer] -> [Integer]
repeatOffset i = drop offset $ take toTake $ cycle i
  where
    offset = read (toString $ take 7 i) :: Int
    toTake = (10000*(length i))

nPhases :: ([Integer] -> [Integer]) -> [Integer] -> Int -> String
nPhases phaseF input n = toString $ take 8 $ iterate phaseF input !! n

input :: Integer
input = 59719811742386712072322509550573967421647565332667367184388997335292349852954113343804787102604664096288440135472284308373326245877593956199225516071210882728614292871131765110416999817460140955856338830118060988497097324334962543389288979535054141495171461720836525090700092901849537843081841755954360811618153200442803197286399570023355821961989595705705045742262477597293974158696594795118783767300148414702347570064139665680516053143032825288231685962359393267461932384683218413483205671636464298057303588424278653449749781937014234119757220011471950196190313903906218080178644004164122665292870495547666700781057929319060171363468213087408071790

pattern :: [Integer]
pattern = [0, 1, 0, -1]
