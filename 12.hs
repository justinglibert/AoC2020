import Control.Monad.State
import Data.List
import Text.ParserCombinators.Parsec (char, many, oneOf, parse)
import qualified Text.ParserCombinators.Parsec as P

data Direction = N | E | S | W | L | R | F deriving (Show, Eq, Read, Enum)

type Move = (Direction, Integer)

type Position = (Integer, Integer)

parseInstruction :: P.GenParser Char st Move
parseInstruction = do
  d <- oneOf "NSEWLRF"
  dist <- many P.digit
  char '\n'
  return (read [d], read dist)

parseFile :: P.GenParser Char st [Move]
parseFile = do
  instructions <- many parseInstruction
  return instructions

forward :: Direction -> Position -> Integer -> Position
forward dir (x0, y0) d = case dir of
  N -> (x0, y0 + d)
  S -> (x0, y0 - d)
  E -> (x0 + d, y0)
  W -> (x0 - d, y0)
  _ -> error $ "Called forward with a wrong direction: " ++ show dir

turn :: Direction -> Integer -> Direction
turn d a = [N .. W] !! newAngle
  where
    Just oldAngle = d `elemIndex` [N .. W]
    shift = fromInteger $ (a `mod` 360) `div` 90
    newAngle = (oldAngle + shift) `mod` 4

move :: Move -> State (Direction, Position) ()
move (direction, arg) = do
  facing <- gets fst
  pos <- gets snd
  let newState = case direction of
        L -> (turn facing (- arg), pos)
        R -> (turn facing arg, pos)
        F -> (facing, forward facing pos arg)
        x -> (facing, forward direction pos arg)
  put newState
  return ()

rotateWaypoint :: Integer -> Position -> Position
rotateWaypoint a p@(x, y) = case a of
  0 -> p
  90 -> (y, - x)
  180 -> (- x, - y)
  270 -> (- y, x)
  -90 -> (- y, x)
  -180 -> (- x, - y)
  -270 -> (y, - x)

moveWithWaypoint :: Move -> State (Position, Position) ()
moveWithWaypoint (direction, arg) = do
  wPos@(x0, y0) <- gets fst
  pos@(xx0, yy0) <- gets snd
  let newState = case direction of
        N -> ((x0, y0 + arg), pos)
        S -> ((x0, y0 - arg), pos)
        E -> ((x0 + arg, y0), pos)
        W -> ((x0 - arg, y0), pos)
        L -> (rotateWaypoint (- arg) wPos, pos)
        R -> (rotateWaypoint arg wPos, pos)
        F -> (wPos, (xx0 + x0 * arg, yy0 + y0 * arg))
  put newState
  return ()

main = do
  c <- readFile "12.txt"
  let instructions = case parse parseFile "parser" c of
        Left err -> error $ show err
        Right val -> val
      -- 1
      (_, (x, y)) = execState (traverse move instructions) (E, (0, 0))
      -- 2
      -- (_, (x, y)) = execState (traverse moveWithWaypoint instructions) ((10, 1), (0, 0))
      mh = abs x + abs y
  print mh
