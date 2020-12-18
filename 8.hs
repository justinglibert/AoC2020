data OpCode = Nop | Acc | Jmp deriving (Show, Eq)

type Instruction = (OpCode, Int)

parseInstruction :: String -> Instruction
parseInstruction xs = (opCode, arg)
  where
    (unparsedOpCode, _ : unparsedArg) = splitAt 3 xs
    arg = readSignedInt unparsedArg
    opCode = parseOpCode unparsedOpCode

readSignedInt :: String -> Int
readSignedInt xs =
  if '+' `elem` xs
    then read $ tail xs
    else read xs

parseOpCode :: String -> OpCode
parseOpCode xs = case xs of
  "nop" -> Nop
  "acc" -> Acc
  "jmp" -> Jmp
  _ -> error "Unrecognised Op Code"

runComputer :: [Instruction] -> Int -> Int -> [Int] -> Either Int Int
runComputer ints acc l vis
  | l `elem` vis = Left acc -- Infinite Loop
  | l == length ints = Right acc -- Succesfully Exited
  | otherwise =
    let (nextAcc, nextLine) = run1 ints acc l
     in runComputer ints nextAcc nextLine (l : vis)

run1 :: [Instruction] -> Int -> Int -> (Int, Int)
run1 insts acc l = case opCode of
  Nop -> (acc, l + 1)
  Acc -> (acc + arg, l + 1)
  Jmp -> (acc, l + arg)
  where
    (opCode, arg) = insts !! l

findCorruptedOpCode :: [Instruction] -> Int -> Maybe Int
findCorruptedOpCode insts i
  | i == length insts = Nothing
  | otherwise =
    let newInsts = patchInstruction insts i
        out = runComputer newInsts 0 0 []
     in case out of
          Left _ -> findCorruptedOpCode insts (i + 1)
          Right acc -> Just acc

patchInstruction :: [Instruction] -> Int -> [Instruction]
patchInstruction insts i = before ++ [newInst] ++ after
  where
    (before, inst : after) = splitAt i insts
    (opCode, arg) = inst
    newOpCode = case opCode of
      Nop -> Jmp
      Jmp -> Nop
      Acc -> Acc
    newInst = (newOpCode, arg)

main :: IO Int
main = do
  contents <- readFile "8.txt"
  let l = lines contents
      instructions = parseInstruction <$> l
  -- Part 1
  print $ runComputer instructions 0 0 []
  -- Part 2
  print $ findCorruptedOpCode instructions 0
  return 1
