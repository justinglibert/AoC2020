import           Control.Monad

boardSize = 12 - 1

allCells = (,) <$> [0 .. boardSize] <*> [0 .. boardSize]

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack a@(a1, a2) b@(b1, b2) = sameRow || sameCol || diag
 where
  sameRow = a1 == b1
  sameCol = a2 == b2
  diag    = b `elem` generateDiag a

generateDiag :: (Int, Int) -> [(Int, Int)]
generateDiag x@(x1, x2) = concatMap (gen x) $ (,) <$> [1, -1] <*> [1, -1]
 where
  gen c@(c1, c2) s@(s1, s2) =
    let n = (c1 + s1, c2 + s2)
    in  case n of
          (a, _) | a < 0 || a > boardSize -> []
          (_, a) | a < 0 || a > boardSize -> []
          _                               -> n : gen n s
findQueens g = foldM g [] [0 .. boardSize]

findFirstSol = head $ findQueens getNextQueen
findFirstSol2 = head $ findQueens getNextQueen2

getNextQueen :: [(Int, Int)] -> Int -> [[(Int, Int)]]
getNextQueen queens r =
  [ next : queens
  | next <- filter (\c -> not $ any (canAttack c) queens)
                   ((,) <$> [r] <*> [0 .. boardSize])
  ]

-- If you truly want all solutions (every assignement for every possible queens)
getNextQueen2 :: [(Int, Int)] -> Int -> [[(Int, Int)]]
getNextQueen2 queens r =
  [ next : queens
  | next <- filter (\c -> not $ any (canAttack c) queens) allCells
  ]

main = do
  print findFirstSol
  print findFirstSol2
