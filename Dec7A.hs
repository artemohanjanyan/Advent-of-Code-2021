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

type MyState = (Int, Int, Int, Int, Int)

inputToStartState :: [Int] -> MyState
inputToStartState sortedInput =
  let x : xs = sortedInput
      fuel = sum $ map (\q -> q - x) xs
  in (x, fuel, fuel, 1, length xs)

step :: Int -> State MyState ()
step y = do
  current <- get
  let (x, minFuel, fuel, prev, next) = current
      yFuel = fuel - next * (y - x) + prev * (y - x)
  put (y, min minFuel yFuel, yFuel, prev + 1, next - 1)

main :: IO ()
main = do
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> do
      let sortedInput = sort input
      let resultState@(_, ans, _, _, _) = execState (mapM_ step $ tail sortedInput) (inputToStartState sortedInput)
      print ans
    Left error -> print error
