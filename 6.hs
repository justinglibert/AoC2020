import Data.Set

main :: IO Int
main = do
  contents <- readFile "6.txt"
  let l = lines contents
      groups = splitOn l ""
      groupsSingleQ = fmap processGroup groups
      total = sum $ fmap length groupsSingleQ
  print $ head groups
  print $ head groupsSingleQ
  return total

processGroup :: [String] -> Set Char
-- replace interesection with union for part 1
processGroup xs = foldr1 intersection $ fmap fromList xs

splitOn :: (Eq a) => [a] -> a -> [[a]]
splitOn xs s
  | s `elem` xs = x1 : rest
  | otherwise = [xs]
  where
    (x1, _ : x2) = break (== s) xs
    rest =
      if s `elem` x2
        then splitOn x2 s
        else [x2]
