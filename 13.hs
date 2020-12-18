import Control.Monad.State
import Control.Parallel.Strategies
import System.Environment
import Text.ParserCombinators.Parsec (char, many, parse)
import qualified Text.ParserCombinators.Parsec as P

cell = do
  c <- many P.digit
  return $ read c

emptyCells = do
  _ <- char 'x'
  next <- remainingCells
  return next

cells = do
  first <- cell
  next <- remainingCells
  return (first : next)

remainingCells =
  (char '\n' >> return []) P.<|> (char ',' >> P.try (emptyCells P.<|> cells))

parseFile :: P.GenParser Char st (Integer, [Integer])
parseFile = do
  a <- many P.digit
  let arrival = read a
  char '\n'
  buses <- cells
  return (arrival, buses)

digits = do
  c <- many P.digit
  return $ read c

parseFile2 :: P.GenParser Char st [Integer]
parseFile2 = do
  a <- many P.digit
  let bus1 = read a
  mid <- many (char ',' >> ((char 'x' >> return 0) P.<|> digits))
  return $ bus1 : mid

parseInput :: String -> (Integer, [Integer])
parseInput xs = case parse parseFile "parser" xs of
  Left err -> error $ show err
  Right val -> val

parseInput2 :: String -> [Integer]
parseInput2 xs = case parse parseFile2 "parser2" xs of
  Left err -> error $ show err
  Right val -> val

findFirstMatch :: Integer -> [Integer] -> State Integer Integer
findFirstMatch arrival buses = do
  current <- get
  let matches = filter ((== 0) . mod (arrival + current)) buses
  case length matches of
    0 -> do
      modify (+ 1)
      findFirstMatch arrival buses
    1 -> return $ current * head matches
    _ -> error "More than one match"

verify :: Integer -> [Integer] -> Bool
verify t buses = and $ zipWith v [0 ..] buses
  where
    v i b = case b of
      0 -> True
      _ -> (t + i) `mod` b == 0

simpleBrute :: [Integer] -> State Integer ()
simpleBrute buses = do
  current <- get
  case verify current buses of
    True -> return ()
    False -> do
      modify (+ 1)
      simpleBrute buses

red :: [Bool] -> Maybe Integer
red = go 0
  where
    go i [] = Nothing
    go i (True : _) = Just i
    go i (False : xs) = go (i + 1) xs

brute :: [Integer] -> (Integer, Integer) -> Maybe Integer
brute buses (curr, high)
  | curr == high = Nothing
  | otherwise = case verify curr buses of
    True -> Just curr
    False -> brute buses ((curr + 1), high)

chunkBrute :: Integer -> Integer -> Integer -> [(Integer, Integer)]
chunkBrute low high chunks = map createChunks [0 .. chunks - 1]
  where
    chunkS = (high - low) `div` chunks
    createChunks i = (low + i * chunkS, low + (i + 1) * chunkS)

reduceMaybe :: [Maybe Integer] -> Maybe Integer
reduceMaybe xs = case foldr red (False, 0) xs of
  (True, a) -> Just a
  (False, _) -> Nothing
  where
    red res acc = case res of
      Just a -> (True, a)
      Nothing -> acc

iterSize :: Integer
iterSize = 10 ^ 9

chunkSize :: Integer
chunkSize = 10 ^ 7

chunks :: Integer
chunks = iterSize `div` chunkSize

-- Use a solve that chunk stuff using with strategy for 10B iterations. If no succes we print and recurse
-- Still spending most of its time on GC. Need to have functions that return a Maybe Integer over a range of
solve curr buses = do
  let found = reduceMaybe $ withStrategy (parList rdeepseq) . map (brute buses) $ chunkBrute curr next chunks
      next = curr + iterSize
  case found of
    Just a -> print $ "Found! " ++ show a
    Nothing -> (print $ "Iterating..." ++ show next) >> solve next buses

main = do
  [f] <- getArgs
  c <- readFile f
  -- part 1
  -- let (arrival, buses) = parseInput c
  --     match = evalState (findFirstMatch arrival buses) 0
  -- print match
  -- part 2
  let buses = parseInput2 . last . lines $ c
  -- t = execState (simpleBrute buses) 0
  solve 0 buses
