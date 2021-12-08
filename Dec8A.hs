import Data.Functor (void)
import Data.List (sort)
import Control.Monad
import Control.Monad.Trans.State.Strict
import Text.Parsec hiding (State, digit)

type InputParser = Parsec String ()

type Digit = String

digit :: InputParser Digit
digit = many1 $ oneOf "abcdefg"

data Display = Display [Digit] [Digit]

displayParser :: InputParser Display
displayParser = Display
  <$> replicateM 10 (digit <* string " ")
  <* string "|"
  <*> replicateM 4 (string " " *> digit)

inputParser :: InputParser [Display]
inputParser = sepBy displayParser newline

is1478 :: Digit -> Bool
is1478 digit = let l = length digit in l == 2 || l == 4 || l == 3 || l == 7

solve :: [Display] -> Int
solve = sum . map (\(Display _ value) -> length $ filter is1478 value)

main :: IO ()
main = do
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> do
      print $ solve input
    Left error -> print error
