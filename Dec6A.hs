import Data.Functor (void)
import Control.Monad (replicateM_)
import Control.Monad.Trans.State.Strict
import Text.Parsec hiding (State)

type InputParser = Parsec String ()

int :: InputParser Int
int = read <$> many1 digit

inputParser :: InputParser [Int]
inputParser = sepBy int (string ",")

type MyState = [Int]

inputToStartState :: [Int] -> MyState
inputToStartState input = map (\n -> length $ filter (== n) input) [0..8]

simulationStep :: State MyState ()
simulationStep = do
  current <- get
  let x:xs = current
  put $ zipWith (+) (xs ++ [0]) [0, 0, 0, 0, 0, 0, x, 0, x]

main :: IO ()
main = do
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> do
      let resultState = execState (replicateM_ 80 simulationStep) (inputToStartState input)
      print $ sum resultState
    Left error -> print error
