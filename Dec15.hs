import Control.Monad (forM_, replicateM_, when)
import Control.Monad.Trans.State.Strict
import Data.Function (on)
import Data.List (zipWith)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Text.Parsec hiding (State)
import Debug.Trace (traceShow)

type InputParser = Parsec String ()

risk :: InputParser Int
risk = read . (:[]) <$> digit

inputParser :: InputParser RiskMap
inputParser = toMap <$> sepBy (many risk) newline

type Point = (Int, Int)
data RiskMap = RiskMap
  { rmMap :: Map.Map Point Int
  , rmColumnN :: Int
  , rmRowN :: Int
  } deriving (Show, Eq)

toMap :: [[Int]] -> RiskMap
toMap risks = RiskMap
  { rmMap = Map.fromList $ concat $
      zipWith
        (\y -> zipWith
          (\x risk -> ((x, y), risk))
          [0..]
        )
        [0..]
        risks
  , rmRowN = length risks
  , rmColumnN = length $ head risks
  }

lookup' :: Ord k => k -> Map.Map k a -> a
lookup' key m = fromJust $ Map.lookup key m

neightbours :: RiskMap -> Point -> [Point]
neightbours rm (x, y) = filter inside $ map (\(dx, dy) -> (x + dx, y + dy)) neightbourList
  where
    neightbourList = [(-1, 0), (0, -1), (0, 1), (1, 0)]
    inside (x, y) = x >= 0 && x < rmColumnN rm && y >= 0 && y < rmRowN rm

shortestPath :: RiskMap -> Point -> Point -> Int
shortestPath rm start finish = go Set.empty (Map.singleton start 0) (Set.singleton (0, start))
  where
    go :: Set.Set Point -> Map.Map Point Int -> Set.Set (Int, Point) -> Int
    go visited dist distSet = let
      ((vDist, v), distSet') = fromJust $ Set.minView distSet
      in if v == finish then vDist else let
        visited' = Set.insert v visited
        toList = filter (not . flip Set.member visited') $ neightbours rm v
        updateDist
          :: Point
          -> (Map.Map Point Int, Set.Set (Int, Point))
          -> (Map.Map Point Int, Set.Set (Int, Point))
        updateDist to (dist, distSet) = let
          distSet' = case Map.lookup to dist of
            Just toDist -> Set.delete (toDist, to) distSet
            Nothing -> distSet
          dist' = Map.insertWith min to (vDist + lookup' to (rmMap rm)) dist
          distSet'' = Set.insert (lookup' to dist', to) distSet'
          in (dist', distSet'')
        (dist', distSet'') = foldr updateDist (dist, distSet') toList
        in go visited' dist' distSet''

solveA :: RiskMap -> Int
solveA rm = shortestPath rm (0, 0) (rmColumnN rm - 1, rmRowN rm - 1)

solveB :: RiskMap -> Int
solveB rm = solveA RiskMap { rmMap = rmMap', rmColumnN = rmColumnN', rmRowN =  rmRowN' }
  where
    rmColumnN' = rmColumnN rm * 5
    rmRowN' = rmRowN rm * 5
    f ((x, y), risk) =
      [ ( (x + dx * rmColumnN rm, y + dy * rmRowN rm)
        , (risk + dx + dy - 1) `mod` 9 + 1
        )
      | dx <- [0..4]
      , dy <- [0..4]
      ]
    rmMap' = Map.fromList $ concat $ map f $ Map.toList $ rmMap rm

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
