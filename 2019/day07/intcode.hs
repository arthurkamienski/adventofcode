import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

type Program      = Map.Map Int Int

data Op           = Add | Mult | Input | Output | Stop | IfTrue | IfFalse | Equals | Less deriving (Show)

type OpFunc       = ProgramState -> Params -> ProgramState

data OpCode       = OpCode {
  opType :: Op,
  nParams :: Int,
  func :: OpFunc
}

data Mode         = Immediate | Position deriving (Show)

type Param        = (Int, Mode)
type Params       = [Param]

type Instruction  = (OpCode, Params)
type Input        = [Int]
type Output       = [Int]
type Phases       = [Int]
type Thrust       = Int

data ProgramState = ProgramState {
                     prog    :: Program,
                     pointer :: Int,
                     input   :: Input,
                     output  :: Output} deriving (Show)

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
  (99, OpCode Stop    0 defaultOp)]

modes = Map.fromList
 [('1', Immediate),
  ('0', Position)]

main = do
  fileContents <- readFile "input.txt"
  let program = parseInput fileContents
  let phases = combinations [0..4]
  print $ maximum $ map (getThrust program) phases
  let newPhases = combinations [5..9]
  print $ maximum $ map (feedbackLoop program) newPhases


amplifiers :: Program -> Phases -> Amplifiers
amplifiers p phs = [startState p [ph] | ph <- phs]

feedbackLoop :: Program -> Phases -> Thrust
feedbackLoop p phs = last $ loop amps [0]
  where
    amps = amplifiers p phs
    loop amps i = case op of
      Stop -> out
      _    -> out ++ (loop newAmps out)
      where
        (newAmps, out, op) = nextAmpStates amps i

nextAmpStates :: Amplifiers -> Input -> (Amplifiers, Output, Op)
nextAmpStates amps i = (erasedAmps, out, op)
  where
    newAmps          = onePass amps i
    lastAmp          = last newAmps
    op               = opType $ fst $ getInstruction lastAmp
    out              = output lastAmp
    erasedAmps       = [a {output = []} | a <- newAmps]

onePass :: Amplifiers -> Input -> Amplifiers
onePass []     _ = []
onePass (a:as) i = nextAmpState : (onePass as out)
  where
    nextAmpState = execUntilOutput a i
    out          = output nextAmpState

execUntilOutput :: ProgramState -> Input -> ProgramState
execUntilOutput ps i = exec $ ps {input = newInput}
  where
    newInput = (input ps) ++ i
    exec currState = case op of
      Stop -> currState
      _    -> case out of
        [] -> exec nextState
        _  -> nextState
      where
        i@(opCode, _) = getInstruction currState
        op            = opType opCode
        nextState     = runInst currState i
        out           = output nextState

combinations :: Phases -> [Phases]
combinations l = cs l
  where
    cs l = foldl (++) [] $ map combinations' $ rotations l
    combinations' []      = [[]]
    combinations' (i:is)  = map unite $ cs is
     where unite l = i : l

rotations :: Phases -> [Phases]
rotations [] = [[]]
rotations l = [rotate n l | n <- [0..(length l)-1]]
  where
    rotate    = drop <> take -- <<<< black magic

getThrust :: Program -> Phases -> Thrust
getThrust prog phases = runAmplifier prog 0 phases
  where
    runAmplifier prog i (p:ps) =
      case ps of
        [] -> ampOut
        _  -> runAmplifier prog ampOut ps
        where
          ampOut = head $ execute prog [p, i]

readInputFile :: String -> IO Program
readInputFile path = do
  i <- readFile path
  return $ parseInput i

parseInput :: String -> Program
parseInput i = program
  where
    codes = map (\c -> read c :: Int) $ splitOn "," i
    program = Map.fromList $ zip [0..] codes

toOpCode :: Int -> OpCode
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
startState p i = ProgramState p 0 i []

getInstruction :: ProgramState -> Instruction
getInstruction ps = (op, paramModes)
  where
    p        = prog ps
    ip       = pointer ps
    (op, ms) = parseOpCode $ p Map.! ip
    n        = nParams op
    params   = map (p Map.!) [ip+1..ip+n+1]
    paramModes = zip params ms

parseOpCode :: Int -> (OpCode, [Mode])
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

getParamValue :: Program -> Param -> Int
getParamValue p (i, m) = case m of
  Immediate -> i
  Position  -> p Map.! i

store :: Program -> Int -> Int -> Program
store p i v = Map.insert i v p

retrieve :: Program -> Int -> Int
retrieve p i = p Map.! i

defaultOp :: OpFunc
defaultOp ps _ = ps

applyStore :: ProgramState -> ([Int] -> Int) -> Params -> ProgramState
applyStore ps f params = ps {prog = newP, pointer = newIp}
  where
    a = fst $ last params
    vs  = map (getParamValue (prog ps)) $ init params
    newP  = store (prog ps) a (f vs)
    newIp = (pointer ps) + (length vs) + 2

addOp :: OpFunc
addOp ps params = applyStore ps sum params

multOp :: OpFunc
multOp ps params = applyStore ps product params

inputOp :: OpFunc
inputOp ps params = ps {prog = newP, pointer = newIp, input = tail is}
  where
    a = fst $ last params
    is = input ps
    newP  = store (prog ps) a (head is)
    newIp = (pointer ps) + 2

outputOp :: OpFunc
outputOp ps params = ps {pointer = newIp, output = newO}
  where
    (a, m) = last params
    newO  = case m of
              Position  -> (retrieve (prog ps) a) : (output ps) 
              Immediate -> a : (output ps)
    newIp = (pointer ps) + 2

ifTrueOp :: OpFunc
ifTrueOp ps params = ps {pointer = newIp}
  where
    [v, i] = map (getParamValue (prog ps)) params
    newIp = case v of
              0 -> (pointer ps) + 3
              _ -> i

ifFalseOp :: OpFunc
ifFalseOp ps params = ps {pointer = newIp}
  where
    [v, i] = map (getParamValue (prog ps)) params
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
