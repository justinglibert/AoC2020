import Data.Map (Map)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

type Memory = Map Int Int
data MaskCell = T | F | X deriving (Show, Eq)
data InputLine = Write (Int, Int)
          | Mask [MaskCell]
          deriving (Show, Eq)
data FloatingBool = TT | FF | XX deriving (Show, Eq)

cell :: GenParser Char st MaskCell
cell = do
    c <- oneOf "X10"
    case c of
      'X' -> return X
      '1' -> return T
      '0' -> return F

parseMask :: GenParser Char st InputLine
parseMask = do
    _ <- string "mask = "
    cs <- many cell
    return $ Mask cs

parseWrite :: GenParser Char st InputLine
parseWrite = do
    _ <- string "mem["
    p <- many digit
    _ <- string "] = "
    d <- many digit
    return $ Write (read p, read d)

parseLine :: GenParser Char st InputLine
parseLine = do
    out <- (try parseWrite) <|> parseMask
    char '\n'
    return out

parseInput :: GenParser Char st [InputLine]
parseInput = do
    lines <- many parseLine
    return lines

p :: String -> [InputLine]
p xs = case parse parseInput "Masks" xs of
    Left err -> error $ show err
    Right val -> val

bitsToInt :: [Bool] -> Int
bitsToInt xs = sum $ zipWith b (reverse [0..length xs - 1]) xs
    where b exp bit = (fromBool bit) * (2 ^ exp)
          fromBool True = 1
          fromBool False = 0

intsToBit :: Int -> Int -> [Bool]
intsToBit i bits = reverse . init . map (intToBit . fst) $ scanr f (0,i) [0..bits - 1]
    where f curr (_, r) = (r `div` (2 ^ curr), r `rem` (2 ^ curr)) 
          intToBit 1 = True
          intToBit 0 = False

-- Part 1

combineMask :: [Bool] -> [MaskCell] -> [Bool]
combineMask bs ms = zipWith f bs ms
    where f x T = True
          f x F = False
          f x X = x

runMaskFile :: (Maybe [MaskCell], Memory) 
               -> InputLine 
               -> (Maybe [MaskCell], Memory)

runMaskFile (mask, mem) x = case x of
    Write (dest, input) -> case mask of
        Nothing -> error "no mask"
        Just m -> let mInput = bitsToInt $ combineMask (intsToBit input 36) m
                  in (mask, M.insert dest mInput mem)
    Mask m -> (Just m, mem)

-- Part 2

combineMaskFloating :: [Bool] -> [MaskCell] -> [FloatingBool]
combineMaskFloating bs ms = zipWith f bs ms
    where f x T = TT
          f x F = case x of
            True -> TT
            False -> FF
          f x X = XX 

expandFloatingMask :: [FloatingBool] -> [[Bool]]
expandFloatingMask = sequence . map f
    where f TT = [True]
          f FF = [False]
          f XX = [True, False]

runMaskFileFloating :: (Maybe [MaskCell], Memory) 
               -> InputLine 
               -> (Maybe [MaskCell], Memory)

runMaskFileFloating (mask, mem) x = case x of
    Write (dest, input) -> case mask of
        Nothing -> error "no mask"
        Just m -> let mDests = map bitsToInt . expandFloatingMask
                                  . combineMaskFloating (intsToBit dest 36) $ m
                      newMemory = foldl (\m d -> M.insert d input m) mem mDests 
                  in (mask, newMemory)
    Mask m -> (Just m, mem)

-- Main

main = do
    c <- readFile "14.txt"
    let inputLines = p c
        -- (_, mem) = foldl runMaskFile (Nothing, M.empty) inputLines
        (_, mem) = foldl runMaskFileFloating (Nothing, M.empty) inputLines
    print $ sum . map snd $ M.toList mem
