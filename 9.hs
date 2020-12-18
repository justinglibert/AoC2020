import Control.Monad

slice start end xs
  | start < 0 = error "invalid start"
  | end >= length xs = error "invalid end"
  | otherwise = take (end - start + 1) . drop start $ xs

findSum :: [Int] -> Int -> Bool
findSum preamble target = not . null $ do
  a <- preamble
  b <- filter (/= a) preamble
  guard (a + b == target)
  return True

findContiguous :: [Int] -> Int -> Int -> Int -> [Int]
findContiguous xs target s n
  | ss == target = l
  | ss > target = findContiguous xs target (s + 1) 1
  | otherwise = findContiguous xs target s (n + 1)
  where
    l = slice s (s + n) xs
    ss = sum l

check :: [Int] -> Int -> Int -> Maybe Int
check xs i preambleLength
  | i == length xs = Nothing
  | otherwise =
    let prev = i - 1
        preamble = slice (prev - preambleLength + 1) prev xs
        found = findSum preamble (xs !! i)
     in case found of
          False -> Just (xs !! i)
          True -> check xs (i + 1) preambleLength

main = do
  content <- readFile "9.txt"
  let l = lines content
      n = fmap read l :: [Int]
      preambleLength = 25
      Just faulty = check n preambleLength preambleLength
      c = findContiguous n faulty 0 1
      weakness = minimum c + maximum c

  print faulty
  print weakness
