import Data.Char
import Numeric

main :: IO Int
main = work

requiredKeys = ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]

buildNumberValidator :: Int -> Int -> String -> Bool
buildNumberValidator mi ma v = (n >= mi) && (n <= ma)
  where
    n = read v :: Int

validateByr = buildNumberValidator 1920 2002

validateIyr = buildNumberValidator 2010 2020

validateEyr = buildNumberValidator 2020 2030

validateHgt hgt = (h >= minH) && (h <= maxH)
  where
    h = read $ takeWhile isDigit hgt :: Int
    ty = dropWhile isDigit hgt
    (minH, maxH) =
      if ty == "cm"
        then (150, 193)
        else (59, 76)

validateHcl hcl = (head hcl == '#') && (isHex $ tail hcl)

validateEcl ecl = or $ map (==) validEyeColors <*> pure ecl

validatePid pid = length pid == 9 && all isDigit pid

validators = [("byr", validateByr), ("iyr", validateIyr), ("eyr", validateEyr), ("hgt", validateHgt), ("hcl", validateHcl), ("ecl", validateEcl), ("pid", validatePid)]

validEyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isHex :: String -> Bool
isHex xs
  | 0 == (length $ readHex xs) = False
  | 0 == (length . snd . head $ readHex xs) = True
  | otherwise = False

work :: IO Int
work = do
  contents <- readFile "4.txt"
  let ls = splitOn contents '\n'
      dirtyPassports = splitOn ls ""
      passports = map (concatMap (`splitOn` ' ')) dirtyPassports
      passportsKeyVal = map (map parseKeyVal) passports
      filteredPassport = filter checkPassport passportsKeyVal
      -- Second part
      filteredPassportValided = filter validatePassport filteredPassport

  print $ head passportsKeyVal
  print $ length passportsKeyVal
  print $ length filteredPassport
  return $ length filteredPassportValided

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

parseKeyVal :: String -> (String, String)
parseKeyVal xs = (k, v)
  where
    [k, v] = splitOn xs ':'

checkPassport :: [(String, String)] -> Bool
checkPassport kvs = and $ containsReqKeys <*> pure keys
  where
    keys = map fst kvs
    containsReqKeys = map (elem :: String -> [String] -> Bool) requiredKeys

validatePassport :: [(String, String)] -> Bool
validatePassport = all validateKV . filter (\kv -> fst kv `elem` requiredKeys)

validateKV :: (String, String) -> Bool
validateKV (k, v) = val v
  where
    Just val = lookup k validators
