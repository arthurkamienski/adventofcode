import IntCodeComputer
import qualified Data.Map.Strict as Map

data Network = Network {computers :: Map.Map Integer ProgramState,
                       idle :: Map.Map Integer Bool,
                       next :: Integer,
                       nat :: [Integer],
                       sentNats :: [Integer]} deriving (Show)

type Packet = (Integer, [Integer])

main = do
  p <- readProgramFile "input.txt"

  let net = startNetwork p

  print $ runUntil255 net
  print $ runUntilRepeat net

runUntilRepeat :: Network -> Integer
runUntilRepeat net
  | length (sentNats net) < 2 = runUntilRepeat $ runWithNat net
  | repeated  = head $ sentNats net
  | otherwise = runUntilRepeat $ runWithNat net
  where
    repeated = (sentNats net !! 0) == (sentNats net !! 1)

runUntil255 :: Network -> Integer
runUntil255 n 
  | (nat n) == [] = runUntil255 $ runNet n
  | otherwise = nat n !! 1

runWithNat :: Network -> Network
runWithNat net
  | not $ allIdle net = runNet net
  | otherwise = newNet {sentNats = (toSend !! 1) : (sentNats net), idle = newIdle}
    where
      toSend = (nat net)
      newNet = sendPackets net [(0, toSend)]
      newIdle = Map.insert 0 False (idle net)

runNet :: Network -> Network
runNet net = newNet {next = nextC, idle = newIdle}
  where
    nextC = ((next net) + 1) `mod` 50
    comp = (computers net) Map.! (next net)
    (newComp, packets) = run comp
    newNet = sendPackets (updateComputer net (next net) newComp) packets
    isIdle = (input comp == []) && (packets == [])
    newIdle = Map.insert (next net) isIdle (idle net)

allIdle :: Network -> Bool
allIdle net = all (\(k, v) -> v) (Map.toList (idle net))

updateComputer :: Network -> Integer -> ProgramState -> Network
updateComputer n i ps = n {computers = Map.insert i ps (computers n)}
                          
sendPackets :: Network -> [Packet] -> Network
sendPackets net [] = net
sendPackets net ((a, p):ps) 
  | a == 255  = sendPackets (net {nat = p}) ps
  | otherwise = sendPackets newNet ps
  where
    cs = computers net
    dest = cs Map.! a
    updated = dest {input = (input $ dest) ++ p}
    newNet = updateComputer net a updated

startNetwork :: Program -> Network
startNetwork p = Network cs idle 0 [] []
  where
    cs = Map.fromList [(i, startState p [i]) | i <- [0..49]]
    idle = Map.fromList [(i, False) | i <- [0..49]]

run :: ProgramState -> (ProgramState, [Packet])
run ps = (newPs {output = []}, packets)
  where
    packets = toPackets $ reverse $ output $ newPs
    newPs = execUntilInput ps i
    i
      | input ps == [] = [-1]
      | otherwise = []

toPackets :: [Integer] -> [Packet]
toPackets [] = []
toPackets (a:x:y:os) = (a, [x, y]) : toPackets os
