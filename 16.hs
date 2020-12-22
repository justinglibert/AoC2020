import Text.Parsec
import Data.Maybe
import Data.List.Split (splitOn)
import Data.List (permutations, (\\))
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Control.Monad

type Ticker = [Int]
type Constraint = (String, (Int, Int), (Int, Int))

whitespace = void $ many $ oneOf " \n\t"

lexeme f = f <* whitespace

parseWord :: Parser String
parseWord = lexeme $ many1 letter
   
p :: Parser a -> String -> a
p f a = case parse f "parser" a of
    Left err -> error $ show err
    Right val -> val

parseConstraint :: Parser Constraint
parseConstraint = do
    w <- many1 parseWord
    void $ char ':'
    whitespace
    a1 <- many1 digit
    void $ char '-'
    a2 <- many1 digit
    whitespace
    string "or"
    whitespace
    b1 <- many1 digit
    void $ char '-'
    b2 <- many1 digit
    return (joinW w, (read a1, read a2), (read b1, read b2))

joinW :: [String] -> String
joinW = foldl1 (\a b -> a ++ " " ++ b)

parseTicket :: Parser [Int]
parseTicket = (map read) <$> (many1 digit) `sepBy1` char ',' <* whitespace

checkConstraint :: Int -> Constraint -> Bool
checkConstraint x (_, (a1, a2), (b1, b2)) = or [
                                                and [x >= a1, x <= a2],
                                                and [x >= b1, x <= b2]
                                               ]

computeErrorRate :: [Constraint] -> [Int] -> Int
computeErrorRate cs = sum . catMaybes . map check
    where check x = case any (checkConstraint x) cs of
                        True -> Nothing
                        False -> Just x

-- Part 2: Check every permutation of the list of constraints
-- until all tickets check out. and $ map (checkOrdering ordering)
-- to check if the permuation is correct

validTicket :: [Constraint] -> [Int] -> Bool
validTicket cs = (== 0) . length . catMaybes . map check
    where check x = case any (checkConstraint x) cs of
                        True -> Nothing
                        False -> Just x

checkOrdering :: [Constraint] -> [Int] -> Bool
checkOrdering cs xs = and $ zipWith checkConstraint xs cs


-- Brute
findPermutation :: [Constraint] -> [[Int]] -> [Constraint]
findPermutation cs xxs = go perms xxs
    where perms = permutations cs
          go [] _ = error "no permutation found"
          go (cs:ccs) xxs = case and $ map (checkOrdering cs) xxs of
                            True -> cs
                            False -> go ccs xxs

-- Too greedy, without backtracking it does not work 
findPermutation2 :: [Constraint] -> [[Int]] -> [Constraint]
findPermutation2 cs xxs = go cs [] xxs
    where go [] found xxs = reverse found
          go cs found xxs = let (left, f) = matchConstraint cs (length found) xxs
                            in go left (f:found) xxs

matchConstraint :: [Constraint] -> Int -> [[Int]] -> ([Constraint], Constraint)
matchConstraint cs i xxs = (left, found)
    where left = filter (/= found) cs
          found = head $ filter (\c -> and 
                        $ map (\xs -> checkConstraint (xs !! i) c) xxs) cs

-- Backtracking algorithm
findValidPositions :: Constraint -> [[Int]] -> [Int]
findValidPositions c xss = filter valid [0..length (head xss) - 1] 
    where valid i = all (flip checkConstraint c) (map (!! i) xss)  

--                ChosenIdx ValidIdx Tries 
addAssignement :: [Int] -> [Int] -> [[Int]] 
addAssignement curr valids = [next:curr | next <- valids \\ curr]

main = do
    c <- readFile "16.txt"
    let l = lines c
        [constraints, ticket, otherTickets] = splitOn [""] l
        cs = fmap (p parseConstraint) constraints
        t = p parseTicket (last ticket)
        ots = fmap (p parseTicket) (tail otherTickets)
        validOts = filter (validTicket cs) ots
        -- Backtracking algorithm for part2
        positions = fmap (flip findValidPositions validOts) cs
        assignement = reverse . head $ foldM addAssignement [] positions
        solution = product . map (t !!) . take 6 $ assignement
    -- Part 1
    print $ sum . map (computeErrorRate cs) $ ots 
    -- Part 2
    print $ solution
    return ()
