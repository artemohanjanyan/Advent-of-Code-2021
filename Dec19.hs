{-# LANGUAGE TupleSections #-}

import Common
import Control.Monad (guard, forM_, when)
import Data.Maybe (listToMaybe)
import Data.List (sort)
import Prelude hiding (subtract)

data Point = Point Int Int Int
  deriving (Eq, Ord, Show)

type Scanner = [Point]

pointParser :: Parser Point
pointParser = Point <$>
  intParser <* string "," <*> intParser <* string "," <*> intParser

scannerParser :: Parser Scanner
scannerParser = string "--- scanner " *> intParser *> string " ---" *> newline *>
  sepEndBy pointParser newline

parser :: Parser [Scanner]
parser = many (scannerParser <* many newline)

data Matrix = Matrix Point Point Point
  deriving (Eq, Ord, Show)

determinant :: Matrix -> Int
determinant (Matrix (Point a b c) (Point d e f) (Point g h i)) =
  a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)

transformations :: [Matrix]
transformations = filter ((== 1) . determinant) $ concat
  [ [ Matrix (Point a 0 0) (Point 0 b 0) (Point 0 0 c)
    , Matrix (Point a 0 0) (Point 0 0 b) (Point 0 c 0)
    , Matrix (Point 0 a 0) (Point b 0 0) (Point 0 0 c)
    , Matrix (Point 0 a 0) (Point 0 0 b) (Point c 0 0)
    , Matrix (Point 0 0 a) (Point b 0 0) (Point 0 c 0)
    , Matrix (Point 0 0 a) (Point 0 b 0) (Point c 0 0)
    ]
  | a <- [-1, 1]
  , b <- [-1, 1]
  , c <- [-1, 1]
  ]

multiply :: Matrix -> Point -> Point
multiply (Matrix (Point a b c) (Point d e f) (Point g h i)) (Point x y z) =
  Point (x * a + y * b + z * c) (x * d + y * e + z * f) (x * g + y * h + z * i)

add :: Point -> Point -> Point
add (Point a b c) (Point d e f) = Point (a + d) (b + e) (c + f)

subtract :: Point -> Point -> Point
subtract (Point a b c) (Point d e f) = Point (a - d) (b - e) (c - f)

transformScanner :: Matrix -> Scanner -> Scanner
transformScanner matrix = map (multiply matrix)

countEqual :: Ord a => [a] -> [a] -> Int
countEqual (x:xs) (y:ys)
  | x < y = countEqual xs (y:ys)
  | x > y = countEqual (x:xs) ys
  | x == y = 1 + countEqual xs ys
countEqual _ _ = 0

unique :: Ord a => [a] -> [a]
unique [] = []
unique [x] = [x]
unique (x:y:xs)
  | x == y = unique (x:xs)
  | otherwise = x : unique (y:xs)

intersectScanners :: Scanner -> Scanner -> Maybe (Scanner, Point)
intersectScanners xs ys = listToMaybe $ do
  yTransformation <- transformations
  let ys' = transformScanner yTransformation ys
  xBase <- xs
  yBase <- ys'
  let shift = subtract xBase yBase
      ys'' = sort $ map (add shift) ys'
  guard $ countEqual xs ys'' >= 12
  pure $ (unique $ sort $ xs ++ ys'', shift)

intersectUntil1IsLeft :: [Scanner] -> Scanner
intersectUntil1IsLeft [s] = s
intersectUntil1IsLeft scanners = intersectUntil1IsLeft $ head $ do
  let indexed = zip [0..] scanners
  (i, s1) <- indexed
  (j, s2) <- indexed
  guard $ i < j
  case fst <$> intersectScanners s1 s2 of
    Just result -> pure $ result : map snd (filter (\(k, _) -> k /= i && k /= j) indexed)
    Nothing -> []

solveA :: [Scanner] -> Int
solveA = length . intersectUntil1IsLeft . map sort

intersectUntil1IsLeft' :: [([Point], Scanner)] -> ([Point], Scanner)
intersectUntil1IsLeft' [s] = s
intersectUntil1IsLeft' scanners = intersectUntil1IsLeft' $ head $ do
  let indexed = zip [0..] scanners
  (i, (c1, s1)) <- indexed
  (j, (c2, s2)) <- indexed
  guard $ i < j
  case intersectScanners s1 s2 of
    Just (result, shift) ->
      pure $ ((c1 ++ map (add shift) c2, result) : map snd (filter (\(k, _) -> k /= i && k /= j) indexed))
    Nothing -> []

distance :: Point -> Point -> Int
distance a b = let Point x y z = subtract a b in abs x + abs y + abs z

solveB :: [Scanner] -> Int
solveB = findMax . fst . intersectUntil1IsLeft' . map ([Point 0 0 0],) . map sort
  where
    findMax xs = maximum [distance a b | a <- xs, b <- xs, a < b]

main :: IO ()
main = mainImpl parser solveA solveB
