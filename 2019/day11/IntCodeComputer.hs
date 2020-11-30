module IntCodeComputer
  ( readProgramFile
  , parseProgram
  , execute
  , executeStack
  , execUntilOutput
  , execUntilInput
  , startState
  , Op (Stop)
  , ProgramState (output, op)
  , Program
  ) where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Text.Show.Functions

data Op = Add |
  Mult |
  Input |
  Output |
  Stop |
  Start |
  IfTrue |
  IfFalse |
  Equals |
  Less |
  AdjBase deriving (Show)

type OpFunc       = ProgramState -> Params -> ProgramState

data OpCode       = OpCode {
  opType :: Op,
  nParams :: Int
} deriving (Show)

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
                     base    :: Base,
                     op      :: Op,
                     params  :: Params} deriving (Show)

opCodes = Map.fromList
 [( 1, OpCode Add     3),
  ( 2, OpCode Mult    3),
  ( 3, OpCode Input   1),
  ( 4, OpCode Output  1),
  ( 5, OpCode IfTrue  2),
  ( 6, OpCode IfFalse 2),
  ( 7, OpCode Less    3),
  ( 8, OpCode Equals  3),
  ( 9, OpCode AdjBase 1),
  (-1, OpCode Start   0),
  (99, OpCode Stop    0)]

modes = Map.fromList
 [('2', Relative),
  ('1', Immediate),
  ('0', Position)]

readProgramFile :: String -> IO Program
readProgramFile path = do
  i <- readFile path
  return $ parseProgram i

parseProgram :: String -> Program
parseProgram i = program
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
    execute' currState = case op currState of
      Stop -> reverse $ output currState
      _    -> execute' nextState
      where
        nextState = getInstruction $ runInst currState

execUntilOutput :: ProgramState -> Input -> ProgramState
execUntilOutput ps i = exec $ ps {input = newInput}
  where
    newInput = (input ps) ++ i
    exec currState = case op currState of
      Stop -> currState
      _    -> case out of
        [] -> exec nextState
        _  -> nextState
      where
        nextState     = getInstruction $ runInst currState
        out           = output nextState

execUntilInput :: ProgramState -> Input -> ProgramState
execUntilInput ps i = exec $ ps {input = newInput}
  where
    newInput = (input ps) ++ i
    exec currState = case op currState of
      Stop  -> currState
      Input -> case input currState of
                 [] -> currState
                 _  -> exec nextState
      _     -> exec nextState
      where
        nextState     = getInstruction $ runInst currState
        out           = output nextState

executeStack :: Program -> Input -> [ProgramState]
executeStack p i = execute' $ startState p i
  where
    execute' currState = case op currState of
      Stop -> [currState]
      _    -> currState : execute' nextState
      where
        nextState     = getInstruction $ runInst currState

startState :: Program -> Input -> ProgramState
startState p i = ProgramState p 0 i [] 0 Start []

retrieve  :: Program -> MemValue -> MemValue
retrieve p i = Map.findWithDefault 0 i p

store :: Program -> MemValue -> MemValue -> Program
store p i v = Map.insert i v p

getInstruction :: ProgramState -> ProgramState
getInstruction ps = ps {op = opType op, params = paramModes}
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

runInst :: ProgramState -> ProgramState
runInst ps = f ps (params ps)
  where
    f = case op ps of
      Add     -> addOp
      Mult    -> multOp
      Input   -> inputOp
      Output  -> outputOp
      IfTrue  -> ifTrueOp
      IfFalse -> ifFalseOp
      Less    -> lessOp
      Equals  -> equalsOp
      AdjBase -> adjBaseOp
      _       -> defaultOp

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
