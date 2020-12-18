import Control.Applicative
import Data.List

main :: IO ()
main = return ()

work :: IO Int
work = do
  contents <- readFile "2.txt"
  let input = lines contents
  return $ length . filter verifyPassword $ input

verifyPassword :: String -> Bool
verifyPassword i = checkRule rule pass
  where
    (rule, pass) = splitString i ':'

splitString :: String -> Char -> (String, String)
splitString s char = (trim f, trim r)
  where
    (f, _ : r) = break (== char) s

checkRule :: String -> String -> Bool
checkRule r p = parseRule' r $ p

parseRule :: String -> (String -> Bool)
parseRule r = (\p -> checkCount . length . filter (\c -> c == requiredChar) $ p)
  where
    (unparsedCount, [requiredChar]) = splitString r ' '
    (unparsedMin, unparsedMax) = splitString unparsedCount '-'
    checkCount = (\c -> (c >= read unparsedMin) && (c <= read unparsedMax)) :: Int -> Bool

parseRule' :: String -> (String -> Bool)
parseRule' r = (\p -> (p !! (read unparsedPos1 - 1)) == requiredChar `xor` (p !! (read unparsedPos2 - 1)) == requiredChar)
  where
    (unparsedPos, [requiredChar]) = splitString r ' '
    (unparsedPos1, unparsedPos2) = splitString unparsedPos '-'

infixr 1 `xor`

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

isSpace = (== ' ')
