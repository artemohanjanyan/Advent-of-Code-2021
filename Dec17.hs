import Control.Monad (guard)
import Data.Functor (void)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import Text.Parsec hiding (State, between)

type InputParser = Parsec String ()

data Point = Point
  { px :: Int
  , py :: Int
  }
  deriving (Show, Eq)

data Rectangle = Rectangle
  { rLow :: Point
  , rHigh :: Point
  }
  deriving (Show, Eq)

data Probe = Probe
  { pPoint :: Point
  , pVelocity :: Point
  }
  deriving (Show, Eq)

int :: InputParser Int
int = do
  sign <- option "" (string "-")
  digits <- many1 digit
  pure $ read $ sign ++ digits

inputParser :: InputParser Rectangle
inputParser = do
  void $ string "target area: x="
  x1 <- int
  void $ string ".."
  x2 <- int
  void $ string ", y="
  y1 <- int
  void $ string ".."
  y2 <- int
  pure $ Rectangle (Point x1 y1) (Point x2 y2)

between :: Ord a => a -> a -> a -> Bool
between a b c = a <= b && b <= c

inside :: Rectangle -> Point -> Bool
inside (Rectangle low high) p =
  between (px low) (px p) (px high) &&
  between (py low) (py p) (py high)

hasGonePast :: Rectangle -> Probe -> Bool
hasGonePast rec p = px (rHigh rec) < px (pPoint p) ||
  py (pVelocity p) <= 0 && py (rLow rec) > py (pPoint p)

add :: Point -> Point -> Point
add (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

probeStep :: Probe -> Probe
probeStep p = Probe
  { pPoint = add (pPoint p) (pVelocity p)
  , pVelocity = Point (max 0 (px (pVelocity p) - 1)) (py (pVelocity p) - 1)
  }

getHighestPoint :: Point -> Rectangle -> Maybe Int
getHighestPoint startingVelocity rec = guard hasReachedTarget *> Just highestPoint
  where
    steps = iterate probeStep (Probe (Point 0 0) startingVelocity)
    pointsBeforeFail = map pPoint $ takeWhile (not . hasGonePast rec) steps
    highestPoint = maximum $ map py $ pointsBeforeFail
    hasReachedTarget = inside rec $ last pointsBeforeFail

getAllHighestPoints :: Rectangle -> [Int]
getAllHighestPoints rec = catMaybes
  [ getHighestPoint (Point vx vy) rec
  | vx <- [1..maxCoord]
  , vy <- [-maxCoord..maxCoord]
  ]
  where
    maxCoord = max (px (rHigh rec)) (py (rHigh rec))

solveA :: Rectangle -> Int
solveA rec = maximum $ getAllHighestPoints rec

solveB :: Rectangle -> Int
solveB rec = length $ getAllHighestPoints rec

main :: IO ()
main = do
  args <- getArgs
  solve <- case args of
    ["A"] -> pure $ print . solveA
    ["B"] -> pure $ print . solveB
    _ -> putStrLn "bad arguments, defaulting to A" *> pure (print . solveA)
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> solve input
    Left error -> print error
