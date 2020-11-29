import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

data Op = Add |
  Mult |
  Input |
  Output |
  Stop |
  IfTrue |
  IfFalse |
  Equals |
  Less |
  AdjBase deriving (Show)

type OpFunc       = ProgramState -> Params -> ProgramState

data OpCode       = OpCode {
  opType :: Op,
  nParams :: Int,
  func :: OpFunc
}

data Mode         = Relative | Immediate | Position deriving (Show)

type MemValue     = Integer 
type Program      = Map.Map MemValue MemValue

type Param        = (MemValue, Mode)
type Params       = [Param]

type Instruction  = (OpCode, Params)
type Input        = [MemValue]
type Output       = [MemValue]
type Base         = MemValue

data ProgramState = ProgramState {
                     prog    :: Program,
                     pointer :: MemValue,
                     input   :: Input,
                     output  :: Output,
                     base    :: Base} deriving (Show)

type Amplifiers = [ProgramState]

opCodes = Map.fromList
 [( 1, OpCode Add     3 addOp),
  ( 2, OpCode Mult    3 multOp),
  ( 3, OpCode Input   1 inputOp),
  ( 4, OpCode Output  1 outputOp),
  ( 5, OpCode IfTrue  2 ifTrueOp),
  ( 6, OpCode IfFalse 2 ifFalseOp),
  ( 7, OpCode Less    3 lessOp),
  ( 8, OpCode Equals  3 equalsOp),
  ( 9, OpCode AdjBase 1 adjBaseOp),
  (99, OpCode Stop    0 defaultOp)]

modes = Map.fromList
 [('2', Relative),
  ('1', Immediate),
  ('0', Position)]

main = do
  fileContents <- readFile "input.txt"
  let program = parseInput fileContents
  print $ execute program [1]
  print $ execute program [2]

readInputFile :: String -> IO Program
readInputFile path = do
  i <- readFile path
  return $ parseInput i

parseInput :: String -> Program
parseInput i = program
  where
    codes = map (\c -> read c :: MemValue) $ splitOn "," i
    program = Map.fromList $ zip [0..] codes

toOpCode :: MemValue -> OpCode
toOpCode i = opCodes Map.! i

toMode :: Char -> Mode
toMode i = modes Map.! i

execute :: Program -> Input -> Output
execute p i = execute' $ startState p i
  where
    execute' currState = case op of
      Stop -> reverse $ output currState
      _    -> execute' nextState
      where
        i@(opCode, _) = getInstruction currState
        op            = opType opCode
        nextState     = runInst currState i

executeStack :: Program -> Input -> [ProgramState]
executeStack p i = execute' $ startState p i
  where
    execute' currState = case op of
      Stop -> [currState]
      _    -> currState : execute' nextState
      where
        i@(opCode, _) = getInstruction currState
        op            = opType opCode
        nextState     = runInst currState i

startState :: Program -> Input -> ProgramState
startState p i = ProgramState p 0 i [] 0

retrieve  :: Program -> MemValue -> MemValue
retrieve p i = Map.findWithDefault 0 i p

store :: Program -> MemValue -> MemValue -> Program
store p i v = Map.insert i v p

getInstruction :: ProgramState -> Instruction
getInstruction ps = (op, paramModes)
  where
    p        = prog ps
    ip       = pointer ps
    (op, ms) = parseOpCode $ retrieve p ip
    n        = toInteger $ nParams op
    params   = map (retrieve p) [ip+1..ip+n+1]
    paramModes = zip params ms

parseOpCode :: MemValue -> (OpCode, [Mode])
parseOpCode o = (opCode, modes)
  where
    opCode = toOpCode $ o `mod` 100
    ps     = nParams opCode
    modes  = map toMode charModes
      where
        padding   = repeat '0'
        intModes  = o `div` 100
        revModes  = reverse . show $ intModes
        charModes = take ps $ revModes ++ padding

runInst :: ProgramState -> Instruction -> ProgramState
runInst ps (op, params) = (func op) ps params

getParamValues :: ProgramState -> Params -> [MemValue]
getParamValues ps params = map (getParamValue ps) params

getParamValue :: ProgramState -> Param -> MemValue
getParamValue ps (i, m) = case m of
  Immediate -> i
  Position  -> retrieve p i
  Relative  -> retrieve p (i + b)
  where
    p = prog ps
    b = base ps

getAddress :: ProgramState -> Param -> MemValue
getAddress ps (i, m) = case m of
  Relative -> i + (base ps)
  _        -> i

defaultOp :: OpFunc
defaultOp ps _ = ps

applyStore :: ProgramState -> ([MemValue] -> MemValue) -> Params -> ProgramState
applyStore ps f params = ps {prog = newP, pointer = newIp}
  where
    a = getAddress ps $ last params
    vs  = getParamValues ps $ init params
    newP  = store (prog ps) a (f vs)
    newIp = (pointer ps) + (toInteger $ length vs) + 2

addOp :: OpFunc
addOp ps params = applyStore ps sum params

multOp :: OpFunc
multOp ps params = applyStore ps product params

inputOp :: OpFunc
inputOp ps [p] = ps {prog = newP, pointer = newIp, input = is}
  where
    a      = getAddress ps p
    (i:is) = input ps
    newP   = store (prog ps) a i
    newIp  = (pointer ps) + 2

outputOp :: OpFunc
outputOp ps [p] = ps {pointer = newIp, output = newO}
  where
    o = getParamValue ps p
    newO  = o : (output ps)
    newIp = (pointer ps) + 2

ifTrueOp :: OpFunc
ifTrueOp ps params = ps {pointer = newIp}
  where
    [v, i] = getParamValues ps params
    newIp = case v of
              0 -> (pointer ps) + 3
              _ -> i

ifFalseOp :: OpFunc
ifFalseOp ps params = ps {pointer = newIp}
  where
    [v, i] = getParamValues ps params
    newIp = case v of
              0 -> i
              _ -> (pointer ps) + 3

equalsOp :: OpFunc
equalsOp ps params = applyStore ps f params
  where
    f [v1, v2]
      | v1 == v2  = 1
      | otherwise = 0


lessOp :: OpFunc
lessOp ps params = applyStore ps f params
  where
    f [v1, v2]
      | v1 < v2   = 1
      | otherwise = 0

adjBaseOp :: OpFunc
adjBaseOp ps params = ps {pointer=newIp, base = newBase}
  where
    [adj]  = getParamValues ps params
    newBase = (base ps) + adj
    newIp = (pointer ps) + 2
