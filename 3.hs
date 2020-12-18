import Control.Applicative
import Data.List

data Square = Tree | Empty deriving (Show, Eq)

type Grid = [[Square]]

type Pos = (Int, Int)

rule1 = (1, 1)

rule2 = (3, 1)

rule3 = (5, 1)

rule4 = (7, 1)

rule5 = (1, 2)

addPos :: Pos -> Pos -> Pos
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

testInput = ["..##", "..##", "......##"]

testGrid = createGrid testInput

main :: IO ()
main = do
  work
  return ()

work :: IO Int
work = do
  contents <- readFile "3.txt"
  let input = lines contents
      grid = createGrid input
  return $ solveSecondProblem grid

countTrees :: Grid -> Int
countTrees grid = traverseGrid (0, 0) grid 0 rule2

solveSecondProblem :: Grid -> Int
solveSecondProblem grid = r1 * r2 * r3 * r4 * r5
  where
    solve r = traverseGrid (0, 0) grid 0 r
    r1 = solve rule1
    r2 = solve rule2
    r3 = solve rule3
    r4 = solve rule4
    r5 = solve rule5

createGrid :: [String] -> Grid
createGrid = go []
  where
    go :: Grid -> [String] -> Grid
    go rows [] = rows
    go rows (l : lss) = go (rows ++ [cycle $ parseLine l]) lss
    parseLine l = map parseChar l
    parseChar c = case c of
      '.' -> Empty
      '#' -> Tree
      _ -> Empty

traverseGrid :: Pos -> Grid -> Int -> Pos -> Int
traverseGrid pos grid tree rule =
  let numRows = length grid
   in case () of
        ()
          | numRows - 1 <= snd pos -> tree
          | otherwise ->
            let newPos = addPos pos rule
                elemAtNewPos = grid !! snd newPos !! fst newPos
                newTree =
                  if elemAtNewPos == Tree
                    then tree + 1
                    else tree
             in traverseGrid newPos grid newTree rule
