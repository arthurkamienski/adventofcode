import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

type Program      = Map.Map Int Int
type OpCode       = Int
type Values       = (Int, Int)
type ValueLocs    = (Int, Int)
type StoreLoc     = Int
type Instruction  = (OpCode, ValueLocs, StoreLoc)
type ProgramState = (Program, Int)

main = do
  input <- readFile "input.txt"
  let program = parseInput input
  print $ executeNounVerb program (12, 2)
  
  let (n, v) = searchForOutput program 19690720
  print $ 100*n + v

  
searchForOutput :: Program -> Int -> (Int, Int)
searchForOutput p o = search [(n, v) | n <- [0..], v <- [0..n]]
  where
    search (nv:nvs)
      | isRightNounVerb p nv o = nv
      | otherwise              = search nvs

isRightNounVerb :: Program -> (Int, Int) -> Int -> Bool
isRightNounVerb p nv o = (executeNounVerb p nv) == o

executeNounVerb :: Program -> (Int, Int) -> Int
executeNounVerb p nv = execute $ replaceNounVerb p nv

replaceNounVerb :: Program -> (Int, Int) -> Program
replaceNounVerb p (n, v) = store 2 v $ store 1 n p

parseInput :: String -> Program
parseInput i = program
  where
    codes = map (\c -> read c :: Int) $ splitOn "," i
    program = Map.fromList $ zip [0..] codes


execute :: Program -> Int
execute p = execute' (p, 0)
  where execute' s@(p, i)
          | opCode == 99 = p Map.! 0
          | otherwise    = execute' $ nextState s
            where (opCode, _, _) = getInstruction s

getInstruction :: ProgramState -> Instruction
getInstruction (p, i) = (opCode, (v1, v2), store)
  where
    atPos i = p Map.! i
    opCode = atPos i
    v1     = atPos (i+1)
    v2     = atPos (i+2)
    store  = atPos (i+3)

nextState :: ProgramState -> ProgramState
nextState s@(p, _) = runInst s i
  where i = getInstruction s
          
runInst :: ProgramState -> Instruction -> ProgramState
runInst (p, i) inst = (newProg, i+4)
  where
    (op, vLocs, storeLoc) = inst
    values  = getValues p vLocs
    result  = runOp op values
    newProg = store storeLoc result p

getValues :: Program -> ValueLocs -> Values 
getValues p (l1, l2) = (v1, v2)
  where
    v1 = p Map.! l1
    v2 = p Map.! l2

runOp :: OpCode -> Values -> Int
runOp op (v1, v2)
  | op == 1 = v1 + v2
  | op == 2 = v1 * v2
  | otherwise = error "Unknown OpCode"

store :: StoreLoc -> Int -> Program -> Program
store l i p = Map.insert l i p
