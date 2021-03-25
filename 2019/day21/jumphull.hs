import IntCodeComputer

type Command = String

main = do
  p <- readProgramFile "input.txt"
  let walkCommand = ["NOT A T", "NOT B J", "OR T J", "NOT C T", "OR T J", "AND D J"]
  let runCommand = ["NOT A T", "NOT B J", "OR T J", "NOT C T", "OR T J", "AND D J", "NOT E T", "NOT T T", "OR H T", "AND T J"]
  
  putStrLn $ walk p walkCommand
  putStrLn $ run p runCommand

joinCommands :: [Command] -> String
joinCommands cs = concat [a ++ b | (a, b) <- zip cs $ repeat "\n"]

walk :: Program -> [Command] -> String
walk p cs = runProgram p command
  where
    command = joined ++ "WALK\n"
    joined = joinCommands cs

run :: Program -> [Command] -> String
run p cs = runProgram p command
  where
    command = joined ++ "RUN\n"
    joined = joinCommands cs

runProgram :: Program -> Command -> String
runProgram p c = res
  where
    out = executeASCII p c
    res
      | all (128 >=) out = convertOutput out
      | otherwise = show $ last out

