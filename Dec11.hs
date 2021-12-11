import Control.Monad (forM_, replicateM_, when)
import Control.Monad.Trans.State.Strict
import Data.Function (on)
import Data.List (zipWith)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Text.Parsec hiding (State)

type InputParser = Parsec String ()

level :: InputParser Int
level = read . (:[]) <$> digit

inputParser :: InputParser [[Int]]
inputParser = sepBy (many level) newline

type Point = (Int, Int)
type LevelMap = Map.Map Point Int

toMap :: [[Int]] -> LevelMap
toMap levels = Map.fromList $ concat $
  zipWith
    (\x -> zipWith
      (\y level -> ((x, y), level))
      [0..]
    )
    [0..]
    levels

lookup' :: Point -> LevelMap -> Int
lookup' point levelMap = fromJust $ Map.lookup point levelMap

step :: Int -> Int -> State LevelMap ()
step rowN columnN = do
  forM_ points step12
  step3
  where
    step12 :: Point -> State LevelMap ()
    step12 point = do
      levelMap <- get
      let oldLevel = lookup' point levelMap
      let newLevel = min (oldLevel + 1) 10
      put $ Map.insert point newLevel levelMap
      when (oldLevel == 9) $ forM_ (neightbours point) step12

    step3 :: State LevelMap ()
    step3 = do
      modify $ Map.map $ \x -> if x > 9 then 0 else x

    points = [(x, y) | x <- [0..rowN - 1], y <- [0..columnN - 1]]
    neightbourList = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    inside (x, y) = x >= 0 && x < rowN && y >= 0 && y < columnN
    neightbours (x, y) = filter inside $ map (\(dx, dy) -> (x + dx, y + dy)) neightbourList

stepAndCountFlashes :: Int -> Int -> State (Int, LevelMap) ()
stepAndCountFlashes rowN columnN = do
  (n, levelMap) <- get
  let levelMap' = execState (step rowN columnN) levelMap
  let stepFlashN = length $ filter ((== 0) . snd) $ Map.toList levelMap'
  put (n + stepFlashN, levelMap')

stepNUntilAllFlash :: Int -> Int -> State LevelMap Int
stepNUntilAllFlash rowN columnN = do
  step rowN columnN
  levelMap <- get
  if all ((== 0) . snd) $ Map.toList levelMap
    then pure 1
    else (+1) <$> stepNUntilAllFlash rowN columnN

solveA :: [[Int]] -> Int
solveA levels =
  let levelMap = toMap levels
      rowN = length levels
      columnN = length (levels !! 0)
  in fst $ execState (replicateM_ 100 (stepAndCountFlashes rowN columnN)) (0, levelMap)

solveB :: [[Int]] -> Int
solveB levels =
  let levelMap = toMap levels
      rowN = length levels
      columnN = length (levels !! 0)
  in evalState (stepNUntilAllFlash rowN columnN) levelMap

main :: IO ()
main = do
  args <- getArgs
  solve <- case args of
    ["A"] -> pure solveA
    ["B"] -> pure solveB
    _ -> putStrLn "bad arguments, defaulting to A" *> pure solveA
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> print $ solve input
    Left error -> print error
