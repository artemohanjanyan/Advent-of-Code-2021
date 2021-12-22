import Common
import Data.List (sort, foldl')
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Bit

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed.Mutable as MU

lookup' :: Ord k => k -> Map.Map k a -> a
lookup' key m = fromJust $ Map.lookup key m

data Point = Point !Int !Int !Int
  deriving (Eq, Ord, Show)

data Cuboid = Cuboid !Bool !Point !Point
  deriving (Show)

cuboidParser :: Parser Cuboid
cuboidParser = do
  fl <- try (string "on") $> True <|> string "off" $> False
  void $ string " x="
  x1 <- intParser
  void $ string ".."
  x2 <- intParser
  void $ string ",y="
  y1 <- intParser
  void $ string ".."
  y2 <- intParser
  void $ string ",z="
  z1 <- intParser
  void $ string ".."
  z2 <- intParser
  pure (Cuboid fl (Point x1 y1 z1) (Point x2 y2 z2))

parser :: Parser [Cuboid]
parser = sepEndBy cuboidParser newline

cuboidPoints :: Cuboid -> [Point]
cuboidPoints (Cuboid _ (Point x1 y1 z1) (Point x2 y2 z2)) =
  [ Point x y z
  | x <- [x1..x2]
  , y <- [y1..y2]
  , z <- [z1..z2]
  ]

runStep :: Set.Set Point -> Cuboid -> Set.Set Point
runStep set c@(Cuboid fl _ _) =
  foldl' (flip $ if fl then Set.insert else Set.delete) set (cuboidPoints c)

isBig :: Cuboid -> Bool
isBig (Cuboid _ (Point x1 y1 z1) (Point x2 y2 z2)) =
  any (\x -> x < -50 || x > 50) [x1, y1, z1, x2, y2, z2]

solveA :: [Cuboid] -> Int
solveA = Set.size . foldl' runStep Set.empty . filter (not . isBig)

compress :: [Int] -> (Map.Map Int Int, Map.Map Int Int)
compress = go 0 Map.empty Map.empty . unique . sort
  where
    go next coordMap coordSize [] = (coordMap, coordSize)
    go next coordMap coordSize [x] = (Map.insert x next coordMap, Map.insert next 1 coordSize)
    go next coordMap coordSize (x:y:xs)
      | x + 1 == y = go (next + 1) coordMap' coordSize' (y:xs)
      | otherwise = go (next + 2) coordMap' (Map.insert (next + 1) (y - x - 1) $ coordSize') (y:xs)
      where
        coordMap' = Map.insert x next coordMap
        coordSize' = Map.insert next 1 coordSize

solveB :: [Cuboid] -> Int
solveB cuboids = runST $ do
  array <- MU.replicate arraySize (Bit False)
  forM_ cuboids $ \realCuboid -> do
    let logicalCuboid@(Cuboid fl _ _) = mapCuboid realCuboid
    forM_ (cuboidPoints logicalCuboid) $ \(Point x y z) ->
      MU.write array (getPhysicalAddress x y z) (Bit fl)
  MU.ifoldl' (\acc i (Bit b) -> if b then acc + cubeSize (getLogicalAddress i) else acc) 0 array
  -- sum $ map cubeSize $ Set.toList $ foldl' runStep Set.empty $ map mapCuboid cuboids
  where
    extractCoords (Cuboid _ (Point x1 y1 z1) (Point x2 y2 z2)) = [(x1, y1, z1), (x2, y2, z2)]
    (xs, ys, zs) = unzip3 $ concat $ map extractCoords cuboids
    ((xMap, xSize), (yMap, ySize), (zMap, zSize)) = (compress xs, compress ys, compress zs)
    mapCuboid (Cuboid fl (Point x1 y1 z1) (Point x2 y2 z2)) = Cuboid fl
      (Point (lookup' x1 xMap) (lookup' y1 yMap) (lookup' z1 zMap))
      (Point (lookup' x2 xMap) (lookup' y2 yMap) (lookup' z2 zMap))
    cubeSize (Point x y z) = lookup' x xSize * lookup' y ySize * lookup' z zSize

    coordSize = (+1) . maximum . map snd . Map.toList 
    xN = coordSize xMap
    yN = coordSize yMap
    zN = coordSize zMap
    arraySize = xN * yN * zN

    getPhysicalAddress x y z = x * yN * zN + y * zN + z
    getLogicalAddress n = Point (n `div` (yN * zN)) (n `mod` (yN * zN) `div` zN) (n `mod` zN)

main :: IO ()
main = mainImpl parser solveA solveB
