import           Control.Monad.State
import           Control.Parallel.Strategies
import           System.Environment
import           Text.ParserCombinators.Parsec  ( char
                                                , many
                                                , parse
                                                )
import qualified Text.ParserCombinators.Parsec as P

-- Parsing

cell = do
  c <- many P.digit
  return $ read c

emptyCells = do
  _ <- char 'x'
  remainingCells

cells = do
  first <- cell
  next  <- remainingCells
  return (first : next)

remainingCells =
  (char '\n' >> return []) P.<|> (char ',' >> P.try (emptyCells P.<|> cells))

parseFile :: P.GenParser Char st (Int, [Int])
parseFile = do
  a <- many P.digit
  let arrival = read a
  char '\n'
  buses <- cells
  return (arrival, buses)

digits = do
  c <- many P.digit
  return $ read c

parseFile2 :: P.GenParser Char st [Int]
parseFile2 = do
  a <- many P.digit
  let bus1 = read a
  mid <- many (char ',' >> ((char 'x' >> return 0) P.<|> digits))
  return $ bus1 : mid

parseInput :: String -> (Int, [Int])
parseInput xs = case parse parseFile "parser" xs of
  Left  err -> error $ show err
  Right val -> val

parseInput2 :: String -> [Int]
parseInput2 xs = case parse parseFile2 "parser2" xs of
  Left  err -> error $ show err
  Right val -> val

-- Part 1

findFirstMatch :: Int -> [Int] -> State Int Int
findFirstMatch arrival buses = do
  current <- get
  let matches = filter ((== 0) . mod (arrival + current)) buses
  case length matches of
    0 -> do
      modify (+ 1)
      findFirstMatch arrival buses
    1 -> return $ current * head matches
    _ -> error "More than one match"

-- Part 2

verify :: Int -> [Int] -> Bool
verify t buses = and $ zipWith v [0 ..] buses
 where
  v i b = case b of
    0 -> True
    _ -> (t + i) `rem` b == 0

brute :: [Int] -> (Int, Int) -> Maybe Int
brute buses (curr, high)
  | curr == high = Nothing
  | otherwise = if verify curr buses
    then Just curr
    else brute buses (curr + 1, high)

chunkBrute :: Int -> Int -> Int -> [(Int, Int)]
chunkBrute low high chunks = map createChunks [0 .. chunks - 1]
 where
  chunkS = (high - low) `div` chunks
  createChunks i = (low + i * chunkS, low + (i + 1) * chunkS)

reduceMaybe :: [Maybe Int] -> Maybe Int
reduceMaybe xs = case foldr red (False, 0) xs of
  (True , a) -> Just a
  (False, _) -> Nothing
 where
  red res acc = case res of
    Just a  -> (True, a)
    Nothing -> acc

iterSize :: Int
iterSize = 10 ^ 10

chunkSize :: Int
chunkSize = 10 ^ 7

chunks :: Int
chunks = iterSize `div` chunkSize

-- Multithreaded brute forcer
solve curr buses = do
  let found =
        reduceMaybe
          $ withStrategy (parList rdeepseq)
          . map (brute buses)
          $ chunkBrute curr next chunks
      next = curr + iterSize
  case found of
    Just a  -> print $ "Found! " ++ show a
    Nothing -> print ("Iterating..." ++ show next) >> solve next buses

main = do
  [f, start] <- getArgs
  c          <- readFile f
  -- part 1
  -- let (arrival, buses) = parseInput c
  --     match = evalState (findFirstMatch arrival buses) 0
  -- print match
  -- part 2
  let buses = parseInput2 . last . lines $ c
      low   = read start
  solve low buses
