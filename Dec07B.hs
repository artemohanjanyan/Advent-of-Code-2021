import Data.Functor (void)
import Data.List (sort)
import Control.Monad
import Control.Monad.Trans.State.Strict
import Text.Parsec hiding (State)

type InputParser = Parsec String ()

int :: InputParser Int
int = read <$> many1 digit

inputParser :: InputParser [Int]
inputParser = sepBy int (string ",")

dist :: Int -> Int -> Int
dist a b = let q = abs (a - b) in (q + 1) * q `div` 2

ansFor :: Int -> [Int] -> Int
ansFor n positions = sum $ map (dist n) positions

ans :: [Int] -> Int
ans positions = minimum $ map (\pos -> ansFor pos positions) positionRange
  where
    positionRange = [minimum positions .. maximum positions]

main :: IO ()
main = do
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> do
      print $ ans input
    Left error -> print error
