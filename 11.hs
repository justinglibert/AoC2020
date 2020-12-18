import Data.Maybe
import Data.Vector (Vector, imap)
import qualified Data.Vector as V
import Text.ParserCombinators.Parsec (char, many, oneOf, parse)
import qualified Text.ParserCombinators.Parsec as P

data Cell = Empty | Occupied | Floor deriving (Show, Eq)

cell = do
  c <- oneOf "L."
  let out = case c of
        'L' -> Empty
        '.' -> Floor
  return out

cells = do
  first <- cell
  next <- remainingCells
  return (first : next)

remainingCells =
  (char '\n' >> return []) P.<|> cells

rows = do many cells

simulate :: Vector (Vector Cell) -> Vector (Vector Cell)
simulate xs = map8los (== Floor) step xs
  where
    step :: Cell -> [Cell] -> Cell
    step x ns = case x of
      Empty -> if count Occupied ns == 0 then Occupied else Empty
      Occupied -> if count Occupied ns >= 5 then Empty else Occupied
      Floor -> Floor

converges :: Vector (Vector Cell) -> Vector (Vector Cell)
converges xs
  | simulate xs == xs = xs
  | otherwise = converges $ simulate xs

count :: Eq a => a -> [a] -> Int
count c = length . filter (== c)

nbs8 = [(-1, 1), (0, 1), (1, 1), (-1, 0), (1, 0), (-1, -1), (0, -1), (1, -1)]

mapnbs ::
  -- | The list of coordinate offsets
  [(Int, Int)] ->
  -- | The mapping function
  (a -> [a] -> b) ->
  -- | The original grid
  Vector (Vector a) ->
  -- | The updated grid
  Vector (Vector b)
mapnbs nbs f m = imap (\y v -> imap (\x i -> modify i (x, y)) v) m
  where
    modify i x = f i $ mapMaybe (get x) nbs
    get (x0, y0) (x, y) = do
      row <- m V.!? (y0 + y)
      row V.!? (x0 + x)

maplos ::
  -- | The list of coordinates indicating the line-of-sight directions
  [(Int, Int)] ->
  -- | The isEmpty predicate - line of sight continues through cells whose value returns 'True'
  (a -> Bool) ->
  -- | The mapping function, given the cell value and the values of all cells found through a line-of-sight search
  (a -> [a] -> b) ->
  -- | The original grid
  Vector (Vector a) ->
  -- | The updated grid
  Vector (Vector b)
maplos nbs isEmpty f m = imap (\y v -> imap (\x i -> modify i (x, y)) v) m
  where
    modify i x = f i $ mapMaybe (getFirst x) nbs
    getFirst x0 x = do
      v <- get x0 x
      if isEmpty v then getFirst (x0 + x) x else return v
    get (x0, y0) (x, y) = do
      row <- m V.!? (y0 + y)
      row V.!? (x0 + x)

instance (Num a, Num b) => Num (a, b) where
  (x, y) + (u, v) = (x + u, y + v)
  (x, y) * (u, v) = (x * u, y * v)
  negate (x, y) = (negate x, negate y)
  fromInteger x = (fromInteger x, 0)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)

map8nbs = mapnbs nbs8

map8los = maplos nbs8

main = do
  c <- readFile "11.txt"
  let parseOutput = parse rows "SeatPlan" c
      plan = case parseOutput of
        Left err -> error $ show err
        Right val -> val
      planV = V.fromList $ map V.fromList plan
      convergedPlanV = converges planV
      convergedPlan = map V.toList $ V.toList convergedPlanV
      occupiedSeats = sum $ count Occupied <$> convergedPlan
  print $ head plan
  print $ length plan
  print $ occupiedSeats
