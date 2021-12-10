import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec hiding (State, Line)

data Point = Point Int Int
  deriving (Eq, Ord, Show)

data Line = Line Point Point
  deriving (Eq, Ord, Show)

type InputParser = Parsec String ()

int :: InputParser Int
int = read <$> many1 digit

lineParser :: InputParser Line
lineParser = do
  x1 <- int
  void $ string ","
  y1 <- int
  void $ string " -> "
  x2 <- int
  void $ string ","
  y2 <- int
  pure $ Line (Point x1 y1) (Point x2 y2)

inputParser :: InputParser [Line]
inputParser = sepBy lineParser newline

type State = Map Point Int

emptyState :: State
emptyState = Map.empty

addLine :: State -> Line -> State
addLine pointMap (Line (Point x1 y1) (Point x2 y2))
  | x1 == x2 = addPoints [Point x1 y | y <- [min y1 y2 .. max y1 y2]]
  | y1 == y2 = addPoints [Point x y1 | x <- [min x1 x2 .. max x1 x2]]
  | abs (x1 - x2) == abs (y1 - y2) = addPoints $ do
    x <- [x1, signum (x2 - x1) + x1 .. x2]
    let y = y1 + signum (y2 - y1) * abs (x - x1)
    pure $ Point x y
  | otherwise = pointMap
  where
    addPoint p pointMap = Map.insertWith (+) p 1 pointMap
    addPoints = foldr addPoint pointMap

main :: IO ()
main = do
  parsedLines <- parse inputParser "" <$> getContents
  case parsedLines of
    Right lines -> do
      let finalState = foldl addLine emptyState lines
      print $ length $ filter ((> 1) . snd) $ Map.toList finalState
    Left error -> print error
