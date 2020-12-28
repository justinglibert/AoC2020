{-# LANGUAGE DeriveFunctor #-}
import           Text.Parsec.String
import           Text.Parsec
import           Text.Parsec.Combinator
import           Control.Comonad
import           Data.Array
import qualified Data.List                     as L


data U i a = U i (Array i a)
  deriving (Functor, Show)

instance Ix i => Comonad (U i) where
  extract (U i a) = a ! i
  duplicate (U i a) =
    U i $ listArray (bounds a) (flip U a <$> range (bounds a))
  extend f u = f <$> duplicate u


uElems :: U i a -> [a]
uElems (U _ a) = elems a

type Row = [Bool]
type Slice = [Row]
type Cube = [Slice]

type ConwayCube = U (Int, Int, Int) Bool

b = 30

readCube '.' = False
readCube '#' = True

parseRow :: Parser Row
parseRow = many1 (readCube <$> oneOf ".#")

parseSlice :: Parser Slice
parseSlice = parseRow `endBy` char '\n'

createInitialCube :: Slice -> ConwayCube
createInitialCube s = U (0, 0, 0) xs
 where
  is = listArray ((-b, -b, -b), (b, b, b)) $ repeat False
  xs = is // concat (zipWith genRow s [0 .. length s - 1])
  genRow r y = [ ((x, y, 0), v) | (v, x) <- zip r [0 ..] ]

neighbours :: [(Int, Int, Int)]
neighbours =
  ((,,) <$> [-1, 0, 1] <*> [-1, 0, 1] <*> [-1, 0, 1]) L.\\ [(0, 0, 0)]

activeNeighbours :: ConwayCube -> Int
activeNeighbours (U i a) = length . filter id $ (!) a . (+$) i <$> neighbours

(<+>) :: Int -> Int -> Int
c <+> d =
  let s   = c + d
      out = abs s >= b
  in  if out then c else s

(+$) :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
(a1, a2, a3) +$ (b1, b2, b3) = (a1 <+> b1, a2 <+> b2, a3 <+> b3)

rule :: ConwayCube -> Bool
rule u = case (e, activeNeighbours u) of
  (True , 2) -> True
  (True , 3) -> True
  (True , _) -> False
  (False, 3) -> True
  (False, _) -> False
  where e = extract u

steps :: (Ix i) => U i a -> (U i a -> a) -> Int -> U i a
steps u f i = iterate (extend f) u !! i

countActivated :: ConwayCube -> Int
countActivated = length . filter id . uElems

main = do
  c <- readFile "17.txt"
  let Right slice0 = parse parseSlice "sliceParser" c
      cube         = createInitialCube slice0
      futureCube   = steps cube rule
  print $ countActivated cube
  print $ countActivated (futureCube 6)
  return ()
