import Common
import Data.List (sort, foldl')
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

lookup' :: Ord k => k -> Map.Map k a -> a
lookup' key m = fromJust $ Map.lookup key m

{-
on x=-9..35,y=-49..3,z=-14..39
off x=-42..-30,y=-3..13,z=-31..-21
on x=-10..35,y=-35..17,z=-16..31
-}

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
solveB cuboids = sum $ map cubeSize $ Set.toList $ foldl' runStep Set.empty $ map mapCuboid cuboids
  where
    extractCoords (Cuboid _ (Point x1 y1 z1) (Point x2 y2 z2)) = [(x1, y1, z1), (x2, y2, z2)]
    (xs, ys, zs) = unzip3 $ concat $ map extractCoords cuboids
    ((xMap, xSize), (yMap, ySize), (zMap, zSize)) = (compress xs, compress ys, compress zs)
    mapCuboid (Cuboid fl (Point x1 y1 z1) (Point x2 y2 z2)) = Cuboid fl
      (Point (lookup' x1 xMap) (lookup' y1 yMap) (lookup' z1 zMap))
      (Point (lookup' x2 xMap) (lookup' y2 yMap) (lookup' z2 zMap))
    cubeSize (Point x y z) = lookup' x xSize * lookup' y ySize * lookup' z zSize

main :: IO ()
main = mainImpl parser solveA solveB
