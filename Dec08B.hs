import Control.Monad
import Data.Functor (void)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Text.Parsec hiding (State, digit)

type InputParser = Parsec String ()

type Digit = Set.Set Char

digit :: InputParser Digit
digit = Set.fromList <$> many1 (oneOf "abcdefg")

data Display = Display [Digit] [Digit]
  deriving (Show)

displayParser :: InputParser Display
displayParser = Display
  <$> replicateM 10 (digit <* string " ")
  <* string "|"
  <*> replicateM 4 (string " " *> digit)

inputParser :: InputParser [Display]
inputParser = sepBy displayParser newline

solve :: Display -> Int
solve (Display sample [i1, i2, i3, i4]) = f i1 * 1000 + f i2 * 100 + f i3 * 10 + f i4
  where
    d1 = fromJust $ find ((== 2) . Set.size) sample
    d4 = fromJust $ find ((== 4) . Set.size) sample
    d7 = fromJust $ find ((== 3) . Set.size) sample
    d8 = fromJust $ find ((== 7) . Set.size) sample
    d235 = filter ((== 5) . Set.size) sample
    d3 = d1 `Set.union` foldr Set.intersection (head d235) d235
    d9 = d3 `Set.union` d4
    d60 = filter (/= d9) $ filter ((== 6) . Set.size) sample
    d0 = fromJust $ find (d7 `Set.isSubsetOf`) d60
    d6 = fromJust $ find (/= d0) d60
    d5 = d9 Set.\\ (d8 Set.\\ d6)
    d2 = fromJust $ find (\x -> Set.size x == 5 && x /= d3 && x /= d5) sample

    f d
      | d == d0 = 0
      | d == d1 = 1
      | d == d2 = 2
      | d == d3 = 3
      | d == d4 = 4
      | d == d5 = 5
      | d == d6 = 6
      | d == d7 = 7
      | d == d8 = 8
      | d == d9 = 9

solveAll :: [Display] -> Int
solveAll = sum . map solve

main :: IO ()
main = do
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> print $ solveAll input
    Left error -> print error
