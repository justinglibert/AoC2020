import Data.List
import qualified Data.Map as Map

data Status = Unknown | Empty1 | Empty | Taken deriving (Show, Eq)

type Plane = Map.Map Integer Status

readBinary :: Char -> String -> Integer
readBinary low = foldr doBin 0 . zip [0 ..] . reverse . map parseBin
  where
    parseBin :: Char -> Integer
    parseBin s =
      if s == low
        then 0
        else 1
    doBin :: (Integer, Integer) -> Integer -> Integer
    doBin (exp, val) acc = acc + val * 2 ^ exp

readTicket :: String -> (Integer, Integer)
readTicket ticket = (readBinary 'F' rows, readBinary 'L' cols)
  where
    (rows, cols) = splitAt 7 ticket

getSeatId :: (Integer, Integer) -> Integer
getSeatId (r, c) = r * 8 + c

fillPlane :: [Integer] -> Plane
fillPlane = foldr fillP Map.empty
  where
    fillP :: Integer -> Plane -> Plane
    fillP id = Map.insert id Taken . Map.insertWith replaceUnknown (id - 1) Empty1 . Map.insertWith replaceUnknown (id + 1) Empty1
    replaceUnknown :: Status -> Status -> Status
    replaceUnknown new old =
      case old of
        Taken -> Taken
        Empty -> error (show old ++ show new)
        Unknown -> Empty1
        Empty1 -> Empty

main :: IO (Maybe Integer)
main = do
  contents <- readFile "5.txt"
  let l = lines contents
      seatIds = map (getSeatId . readTicket) $ l
      populatedPlane = fillPlane seatIds
      remainingSeat = find (\(_, v) -> v == Empty) . Map.toList $ populatedPlane
  return $ fst <$> remainingSeat
