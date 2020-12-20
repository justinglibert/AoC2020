import Data.Map (Map)
import System.Environment
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import qualified Data.Map as M
import Control.Monad.ST
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as V

input :: [Int]
input = [20,9,11,0,1,2]

-- Pure version

gen :: [Int] -> (Int, Map Int Int)
gen xs = (last xs, mem)
    where mem = foldl f M.empty $ zip [1..] (reverse $ init xs)
          f mem (i, v) = M.insert v i mem

next :: (Int, Map Int Int) -> (Int, Map Int Int)
next (n, mem) = case M.lookup n mem of
    Nothing -> (0, newMem)
    Just a -> let diff = a
              in (diff, newMem)
    where newMem = (+ 1) <$> M.insert n 0 mem

iter curr 0 = fst $ curr
iter curr x = iter (next curr) $! x - 1

getNum x xs = iter (gen xs) (x - length xs)

-- Mutable Vector version

gen2 xs = do
    v <- V.new 30000000
    zipWithM_ (V.write v) (init xs) [1..]
    return (last xs, v, length xs) 

next2 n mem i = do
    m <- V.read mem n
    V.write mem n i
    case m of
        0 -> do
              return 0
        x -> do
              let diff = i - x
              return diff

iter2 curr 0 mem i = return curr
iter2 curr x mem i = do
     n <- next2 curr mem i
     out <- iter2 n (x - 1) mem (i + 1) 
     return out

getNum2 x xs = runST $ do
     (curr, mem, i) <- gen2 xs
     out <- iter2 curr (x - length xs) mem i
     return out

-- Mutable vectors are needed for Part2. The Map overflows otherwise
-- Don't run this in GHCI. Compile it with -O2 and do
-- $ ./15 30000000
main = do
   [x] <- getArgs
   print $ getNum2 (read x) input

