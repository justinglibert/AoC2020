import qualified Data.Map as M
import qualified Data.Text as T

myBag = "shiny gold"

type Rule = (String, M.Map String Int)

goodSplit :: String -> String -> [String]
goodSplit s xs = T.unpack <$> T.splitOn sp t
  where
    sp = T.pack s
    t = T.pack xs

createRule :: String -> (String, Int)
createRule xs = (bag, amount)
  where
    [unparsedAmount, bag1, bag2, _] = goodSplit " " xs
    amount = read unparsedAmount
    bag = bag1 ++ " " ++ bag2

processRule :: String -> Rule
processRule r = (name, rules)
  where
    [name, rr] = goodSplit " bags contain " r
    unparsedRules = init rr
    rules =
      if ',' `elem` unparsedRules
        then M.fromList . fmap createRule $ goodSplit ", " unparsedRules
        else
          if T.pack "no other bags" `T.isInfixOf` T.pack unparsedRules
            then M.empty
            else M.fromList [createRule unparsedRules]

containsBagType :: M.Map String (M.Map String Int) -> String -> String -> Bool
containsBagType m b curr
  | M.null lkp = False
  | b `elem` M.keys lkp = (> 0) $ lkp M.! b
  | otherwise = or $ containsBagType m b <$> M.keys lkp
  where
    lkp = m M.! curr

totalNumberOfBags :: M.Map String (M.Map String Int) -> String -> Int
totalNumberOfBags m curr
  | M.null lkp = 0
  | otherwise = (sum $ M.elems lkp) + (sum $ (\k -> (lkp M.! k) * (totalNumberOfBags m k)) <$> M.keys lkp)
  where
    lkp = m M.! curr

main :: IO Int
main = do
  contents <- readFile "7.txt"
  let l = lines contents
      rs = fmap processRule l
      allBags = fst <$> rs
      rules = M.fromList rs
  print $ length rules
  -- part 1
  print $ sum $ (\v -> if v then 1 else 0) <$> containsBagType rules myBag <$> allBags
  -- part 2
  print $ totalNumberOfBags rules myBag
  return 1
