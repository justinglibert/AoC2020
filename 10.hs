import qualified Control.Monad as CM
import Control.Monad.State
import qualified Data.Map as M
import Test.QuickCheck

handyTest = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4] :: [Int]

findValid :: Int -> [Int] -> [Int]
findValid c xs = do
  a <- xs
  CM.guard (a - c <= 3)
  return a

findDifference :: [Int] -> Int -> M.Map Int Int -> M.Map Int Int
findDifference xs c m
  | null xs = m
  | otherwise =
    let closestX = minimum . findValid c $ xs
        remaining = filter (/= closestX) xs
        difference = closestX - c
        newM = M.insertWith (+) difference 1 m
     in findDifference remaining closestX newM

findArrangementBrute :: [Int] -> Int -> Int
findArrangementBrute xs j
  | length xs == 0 = 1
  | otherwise =
    let process x = findArrangementBrute (filter (> x) xs) x
     in sum . fmap process $ valid
  where
    valid = findValid j xs

findArrangement :: [Int] -> Int -> State (M.Map Int Int) Int
findArrangement xs j
  | j == 0 = return 1
  | otherwise = do
    cache <- gets $ M.lookup j
    let valid = do
          a <- xs
          CM.guard $ (j - a) <= 3
          return a
        process v = findArrangement (filter (< v) xs) v
    case cache of
      Just c -> return c
      Nothing -> do
        s <- sum <$> traverse process valid
        modify $ M.insert j s
        return s

main = do
  c <- readFile "10.txt"
  let l = lines c
      adapters = fmap read l :: [Int]
      adaptersWithDevice = (maximum adapters + 3) : adapters
      dist = findDifference adaptersWithDevice 0 M.empty
      out = dist M.! 1 * dist M.! 3
      out2 = runState (findArrangement (0 : adapters) (maximum adaptersWithDevice)) M.empty
  print out
  print $ fst out2
