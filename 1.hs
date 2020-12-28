import           Control.Applicative
import qualified Control.Monad                 as M
import           System.Environment

main :: IO ()
main = return ()

work :: IO Int
work = do
  contents <- readFile "1.txt"
  let input = map readInt . words $ contents
  return $ head . solve2' $ input

solve :: [Int] -> [Int]
solve xs = do
  fis <- xs
  sec <- xs
  M.guard (fis + sec == 2020)
  return $ fis * sec

solve' :: [Int] -> [Int]
solve' xs =
  map (uncurry (*)) . filter (\(f, s) -> f + s == 2020) $ (,) <$> xs <*> xs

solve2 :: [Int] -> [Int]
solve2 xs = do
  fis <- xs
  sec <- xs
  thi <- xs
  M.guard (fis + sec + thi == 2020)
  return $ fis * sec * thi

-- This is much faster

solve2' :: [Int] -> [Int]
solve2' xs = do
  fis <- xs
  sec <- filter (<= (2020 - fis)) xs
  thi <- filter (<= (2020 - fis - sec)) xs
  M.guard (fis + sec + thi == 2020)
  return $ fis * sec * thi

readInt :: String -> Int
readInt = read
